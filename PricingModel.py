client = 'CCSI'

import pandas as pd
import numpy as np
import os
from time import ctime
import sys
sys.path.append('c:/code/general')
from NCHGeneral import Use, Save, fewSpreadsheets
from NCHGeneral import *

os.chdir('c:/AdvAnalytics/OCM/' + client + '/BaselineUpdated')

def readCoefficients():
    df = pd.read_excel('c:/AdvAnalytics/OCM/Reference/Input/Model_Coefficients_Revised.xlsx',
                       sheetname='ParameterEstimates', skiprows=3, header=None,
                       names=['Variable', 'df', 'Estimate', 'StdError', 'AssocProb', 'Impact'])
    df = df[['Variable', 'Impact']]
    df = df[df.Variable!='Intercept']
    df['BasePrice'] = np.where(df.Impact>100, 'Y', 'N')
    df['AgeSexVariable'] = np.where(df.Variable.str.contains('ale'), "Yes", "No")
    Save(df, 'c:/AdvAnalytics/OCM/Reference/ref_pricingCoefficients')
    return df


def readEpi():
    df = Use('Working/dfepi')
    return df


def addBenchmarks2dfepi(df):
    '''
    This function adds benchmark rates for certain key variables to dfepi, e.g., the percentage of patients who have
    radiation therapy during their episodes.  Benchmarks are the mean by detailed cancer type (included the
    breakouts for breast, prostate, and bladder, but not surgery).  There are lots of fields for which benchmarks are
    created, including cost and utilization, along with pricing model variables.

    :param df: dfepi, the episode summary dataframe
    :return: the episode summary dataframe, now limited to episodes that are used in PBP
    '''
    df = df[df.ReconciliationEligible > 0.5]
    df['Female'] = np.where(df.Sex == 2, 1.0, 0.0)
    df['DiedDuringEpisode'] = np.where(df.DeathDate.between(df.EpiStart, df.EpiEnd), 1.0, 0.0)
    df['DualEligible'] = np.where(df.DualPartDLIS == 3, 1., 0.)
    df['HasPartD'] = np.where(df.DualPartDLIS.between(1, 2), 1., 0.)
    df['HasLIS'] = np.where(df.DualPartDLIS == 2, 1., 0.)
    df['HadRadOnc'] = np.where(df.RadiationFlag == 1, 1., 0.)
    df['NumberHCCs'] = df.HCCCount.map({'00': 0.,
                                        '01': 1.,
                                        '02': 2.,
                                        '03': 3.,
                                        '4-5': 4.33,
                                        '6+': 6.5})
    df['CancerTypeDetailed'] = df.CancerType
    df.loc[(df.CancerType == 'Breast Cancer') & (df.PartDChemo == 0), 'CancerTypeDetailed'] = 'Breast w/ Infused Drugs'
    df.loc[(df.CancerType == 'Breast Cancer') & (df.PartDChemo == 1), 'CancerTypeDetailed'] = 'Breast, Orals Only'
    df.loc[(df.CancerType == 'Prostate Cancer') & (
    df.CastrationSensitiveProstate == 1), 'CancerTypeDetailed'] = 'Prostate, Castration Sensitive'
    df.loc[(df.CancerType == 'Prostate Cancer') & (
    df.CastrationSensitiveProstate == 0), 'CancerTypeDetailed'] = 'Prostate, Castration Resistant'
    df.loc[
        (df.CancerType == 'Bladder Cancer') & (df.LowRiskBladderFlag == 0), 'CancerTypeDetailed'] = 'Bladder, High Risk'
    df.loc[
        (df.CancerType == 'Bladder Cancer') & (df.LowRiskBladderFlag == 1), 'CancerTypeDetailed'] = 'Bladder, Low Risk'
    df['SurgeryFlag'] = df.SurgeryFlag.astype(np.float16)
    df['ClinicalTrialFlag'] = df.ClinicalTrialFlag.astype(np.float16)
    df['CleanPeriodRecentHistory'] = np.where(df.CleanPeriod == 1, 1., 0.)
    df['CleanPeriodOlderHistory'] = np.where(df.CleanPeriod == 2, 1., 0.)
    df['CleanPeriodNoHistory'] = np.where(df.CleanPeriod == 2, 1., 0.)
    listCols = ['Female', 'Age', 'DiedDuringEpisode', 'DualEligible', 'HasPartD', 'HasLIS',
                'HadRadOnc', 'NumberHCCs', 'CancerTypeDetailed', 'CleanPeriodRecentHistory',
                'CleanPeriodOlderHistory', 'CleanPeriodNoHistory', 'BaselinePrice',
                'WinsorizedCost', 'ActualCost', 'PartBTOSPaidProcedures', 'PartBTOSPaidDrugs',
                'PartBTOSPaidImaging', 'PartBTOSPaidLab', 'PartBTOSPaidOther',
                'PartBTOSPaidChemo', 'PartBTOSPaidE&M', 'PartBTOSServicesE&M',
                'PartBTOSPaidRadOnc', 'PartBTOSServicesRadOnc', 'PartBTOSPaidDME',
                'PartBTOSPaidEmerg', 'IPTotalPaid', 'IPOncologyPaid', 'IPAdmits',
                'IPOncologyAdmits', 'IPNonOncologyPaid', 'IPNonOncologyAdmits',
                'SNFPaid', 'SNFLOS', 'HHAPaid', 'HHAVisits', 'HospicePaid', 'HospiceLOS',
                'PartDChemoPaid', 'PartDChemoScripts', 'PartDNonChemoPaid',
                'PartDNonChemoScripts', 'DMEDrugPaid', 'DMENonDrugPaid']
    dfA = df[listCols].groupby('CancerTypeDetailed').agg('mean')
    dfA.reset_index(inplace=True)
    Save(dfA, Working + '/dfepiBenchmarks')
    for c in dfA.columns.tolist():
        if c != 'CancerTypeDetailed':
            dfA.rename(columns={c: c + 'Benchmark'}, inplace=True)
    df = pd.merge(df, dfA, on='CancerTypeDetailed')
    Save(df, Working + '/dfepi')
    return df



dfcoef = readCoefficients()
dfepi = readEpi()
