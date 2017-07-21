client = 'CCSI'
dataEndDate = '12/31/2016'

import pandas as pd
import numpy as np
import os
from time import ctime
import sys
sys.path.append('c:/code/general')
from NCHGeneral import *
import math
import warnings
# warnings.simplefilter('ignore', 'FutureWarning')

global parser
global InputFeather
parser = lambda date: pd.datetime.strptime(date, '%d%b%Y')
dataEndDate = pd.to_datetime(dataEndDate)

############################################
### General functions to read data files ###
############################################
def createDirectories(client):
    global InputFeather
    InputFeather = os.getcwd() + '/Input'
    try:
        os.makedirs(InputFeather)
    except:
        pass
    global Working
    Working = os.getcwd() + '/Working'
    try:
        os.makedirs(Working)
    except:
        pass
    global Output
    Output = os.getcwd() + '/Output'
    try:
        os.makedirs(Output)
    except:
        pass
    global AggWorking
    AggWorking = 'c:/AdvAnalytics/OCM/' + client + '/QtrAggregate/Working'
    try:
        os.makedirs(AggWorking)
    except:
        pass
    global AggOutput
    AggOutput = 'c:/AdvAnalytics/OCM/' + client + '/QtrAggregate/Output'
    try:
        os.makedirs(AggOutput)
    except:
        pass


def readBeneFile():
    fn = getFN('benelevel')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'QTR_START_DATE': 'str', 'DOB': 'str', 'DEATH': 'str',
                            'CHEMO_DATE': 'str'})
    df = RenameVars(df)
    for c in ['BirthDate', 'DeathDate', 'ChemoStartDate', 'QuarterStartDate']:
        df[c].fillna('19700101', inplace=True)
        df[c] = df[c].apply(lambda x: pd.to_datetime(x, format='%Y%m%d'))
    df['PatientName'] = df.LastName.apply(lambda x: x.title()) + ', ' + df.FirstName.apply(lambda x: x.title())
    Save(df, InputFeather + '/dfbene')
    return df


def readDMEQuarterly():
    fn = getFN('dmehead')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'DenialCode': 'str'},
                     parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT'],
                     date_parser=parser,
                     infer_datetime_format=True)
    dfhead = RenameVars(df)
    fn = getFN('dmeline')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'PRVDR_SPCLTY': 'str',
                            'LINE_CMS_TYPE_SRVC_CD': 'str',
                            'LINE_PLACE_OF_SRVC_CD': 'str',
                            'LINE_NDC_CD': 'str'},
                     parse_dates=['CLM_THRU_DT', 'LINE_1ST_EXPNS_DT', 'LINE_LAST_EXPNS_DT'],
                     date_parser=parser,
                     infer_datetime_format=True)
    dfline = RenameVars(df)
    dfdme = pd.merge(dfhead, dfline, how='left')
    dfdme.CPTMod2.fillna(' ', inplace=True)
    dfdme['DenialCode'] = dfdme.DenialCode.apply(lambda x: str(x))
    Save(dfdme, InputFeather + '/dfdme')
    return dfdme


def readHHAQuarterly():
    fn = getFN('hhahead')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'PRVDR_NUM':'str'},
                     parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT', 'FI_CLM_PROC_DT'],
                     date_parser=parser)
    df = RenameVars(df)
    Save(df, InputFeather + '/dfhha')
    return df


def readHospiceQuarterly():
    fn = getFN('hsphead')
    df = pd.read_csv(fn, delimiter='|', dtype={'PRVDR_NUM': 'str'},
                     parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT',
                                  'FI_CLM_PROC_DT', 'CLM_HOSPC_START_DT_ID'],
                     date_parser=parser)
    df = RenameVars(df)
    df['LOS'] = (df.ThruDate - df.FromDate)/np.timedelta64(1, 'D')
    Save(df, InputFeather + '/dfhs')
    return df


def readIPQuarterly():
    try:
        dfhead = Use(InputFeather + '/dfip')
    except:
        fn = getFN('inphead')
        # Left off.  Need to parse dates and specify string fields
        df = pd.read_csv(fn, delimiter='|',
                         dtype={'PRVDR_NUM': 'str',
                                'PRVDR_STATE_CD': 'str',
                                'PTNT_DSCHRG_STUS_CD': 'str',
                                'CLM_IP_ADMSN_TYPE_CD': 'str',
                                'CLM_SRC_IP_ADMSN_CD': 'str',
                                'CLM_DRG_CD': 'str'},
                         parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT', 'FI_CLM_PROC_DT'],# 'CLM_ADMSN_DT', 'NCH_BENE_DSCHRG_DT'],
                         date_parser=parser)
        dfhead = RenameVars(df)
        xw = Use('c:/AdvAnalytics/Reference/list_OncologyDRGs.feather')
        listDRGs = xw.DRG.tolist()
        dfhead['OncologyDRG'] = np.where(dfhead.DRG.isin(listDRGs), 1, 0)
        dfhead['OncologyRelatedPaid'] = dfhead.Paid * dfhead.OncologyDRG

        for c in ['AdmitDate', 'DischargeDate']:
            dfhead[c].fillna('01JAN1970', inplace=True)
            dfhead[c] = dfhead[c].apply(lambda x: pd.to_datetime(x, format='%d%b%Y'))
    try:
        dfline = Use(InputFeather + '/dfiprev')
    except:
        fn = getFN('inprev')
        df = pd.read_csv(fn, delimiter='|',
                         dtype={'REV_CNTR': 'str'},
                         parse_dates=['CLM_THRU_DT'],
                         date_parser=parser)
        dfline = RenameVars(df)
        dfline = pd.merge(dfline, dfhead, how='left')
        dfline.loc[dfline.RevCode.between('045', '0459'), 'EmergencyAdmit'] = 'Emerg'
        dfemerg = dfline[dfline.RevCode.between('045', '0459')]
        # dfemerg['EmergencyAdmit'] = 'Emerg'
        dfemerg = dfemerg[['ClaimNum', 'EmergencyAdmit']]
        dfemerg.drop_duplicates(inplace=True)
        dfhead['ClaimNum'] = dfhead.ClaimNum.apply(lambda x: str(x))
        dfemerg['ClaimNum'] = dfemerg.ClaimNum.apply(lambda x: str(x))
        dfhead = pd.merge(dfhead, dfemerg, on='ClaimNum', how='left')
        dfhead['EmergencyAdmit'].fillna('No ER', inplace=True)
    Save(dfhead, InputFeather + '/dfip')
    Save(dfline, InputFeather + '/dfiprev')
    return dfhead, dfline


def readSNFQuarterly():
    fn = getFN('snfhead')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'PRVDR_NUM': 'str',
                            'PRVDR_STATE_CD': 'str',
                            'PTNT_DSCHRG_STUS_CD': 'str',
                            'CLM_IP_ADMSN_TYPE_CD': 'str',
                            'CLM_SRC_IP_ADMSN_CD': 'str',
                            'CLM_DRG_CD': 'str'},
                     parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT', 'FI_CLM_PROC_DT'],
                     date_parser=parser)
    df = RenameVars(df)
    for c in ['AdmitDate', 'DischargeDate']:
        df[c].fillna('01JAN1970', inplace=True)
        df[c] = df[c].apply(lambda x: pd.to_datetime(x, format='%d%b%Y'))
    Save(df, InputFeather + '/dfsnf')
    return df


def readRxQuarterly():
    try:
        df = Use(InputFeather + '/dfPartD')
    except:
        fn = getFN('_pde_')
        df = pd.read_csv(fn, delimiter='|',
                         parse_dates=['SRVC_DT'],
                         date_parser=parser,
                         dtype={'PROD_SRVC_ID': 'str'})
        df = RenameVars(df)
        df['Paid'] = df.LICSPaid + 0.8*df.CostAboveCatastrophic
        xwndc = Use('c:/AdvAnalytics/Reference/ref_NDC')
        xwndc = xwndc[['NDC', 'GPI']]
        xw = Use('c:/AdvAnalytics/Reference/ref_GPI')
        xw = xw[['GPI', 'TherapeuticClassLevel1', 'DrugName']]
        xw = pd.merge(xw, xwndc)
        df = pd.merge(df, xw, on='NDC', how='left')
        Save(df, InputFeather + '/dfPartD')
    return df


def readphyQuarterly():
    fn = getFN('phyhead')
    try:
        dfhead = Use(InputFeather + '/dfphyhead')
        print('  Use method applied for dfphyhead')
    except:
        df = pd.read_csv(fn, delimiter='|',
                         parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT'],
                         date_parser=parser,
                         dtype={'BENE_CNTY_CD': 'str',
                                'BENE_STATE_CD': 'str',
                                'BENE_MLG_CNTCT_ZIP_CD': 'str',
                                'CARR_NUM': 'str',
                                'CARR_CLM_PMT_DNL_CD': 'str'})
        dfhead = RenameVars(df)
        Save(dfhead, InputFeather + '/dfphyhead')
    try:
        dfline = Use(InputFeather + '/dfphyline')
        print('  Use method applied for dfphyline')
    except:
        fn = getFN('phyline')
        df = pd.read_csv(fn, delimiter='|',
                         dtype={'TAX_NUM': 'str',
                                'PRVDR_SPCLTY': 'str',
                                'LINE_CMS_TYPE_SRVC_CD': 'str',
                                'LINE_PLACE_OF_SRVC_CD': 'str',
                                'HCPCS_CD': 'str',
                                'HCPCS_1ST_MDFR_CD': 'str',
                                'HCPCS_2ND_MDFR_CD': 'str',
                                'LINE_ICD_DGNS_CD': 'str'},
                         parse_dates=['CLM_THRU_DT', 'LINE_1ST_EXPNS_DT', 'LINE_LAST_EXPNS_DT'],
                         date_parser=parser)
        dfline = RenameVars(df)
        xw = Use('c:/AdvAnalytics/Reference/ref_BETOS')
        xw = xw[['BETOS', 'Level3Group_lbl']]
        xw.columns = ['BETOS', 'BETOSLevel3Group']
        xw.BETOSLevel3Group.replace({'Durable Medical Equipment': 'DME',
                                     'Evaluation and Management': 'E&M',
                                     'Exceptions / Unclassified': 'Other',
                                     'Imaging': 'Imaging',
                                     'Tests': 'Lab'}, inplace=True)
        xw.loc[xw.BETOS.isin(['D1G', 'O1E']), 'BETOSLevel3Group'] = 'Drugs'
        xw.loc[xw.BETOS.isin(['O1D']), 'BETOSLevel3Group'] = 'Chemo'
        xw.loc[xw.BETOS.isin(['M3']),  'BETOSLevel3Group'] = 'Emerg'
        xw.loc[xw.BETOS.isin(['P7A']), 'BETOSLevel3Group'] = 'RadOnc'
        xw.loc[xw.BETOS.isin(['T2A', 'T2B', 'T2C', 'T2D']), 'BETOSLevel3Group'] = 'Other'
        dfline = pd.merge(dfline, xw)
        Save(dfline, InputFeather + '/dfphyline')
    dfphy = pd.merge(dfline, dfhead)
    Save(dfphy, InputFeather + '/dfphy')
    return dfhead, dfline, dfphy


def readOPQuarterly():
    try:
        dfhead = Use(InputFeather + '/dfophead')
        print('  Use method applied for dfophead')
    except:
        fn = getFN('outhead')
        df = pd.read_csv(fn, delimiter='|',
                         dtype={'PRVDR_NUM': 'str',
                                'PTNT_DSCHRG_STUS_CD': 'str',
                                'ICD_PRCDR_CD1': 'str',
                                'ICD_PRCDR_CD2': 'str',
                                'ICD_PRCDR_CD3': 'str',
                                'ICD_PRCDR_CD4': 'str',
                                'ICD_PRCDR_CD5': 'str',
                                'ICD_PRCDR_CD6': 'str',
                                'ICD_PRCDR_CD7': 'str',
                                'ICD_PRCDR_CD8': 'str',
                                'ICD_PRCDR_CD9': 'str',
                                'ICD_PRCDR_CD10': 'str',
                                'ICD_PRCDR_CD11': 'str',
                                'ICD_PRCDR_CD12': 'str',
                                'ICD_PRCDR_CD13': 'str',
                                'ICD_PRCDR_CD14': 'str',
                                'CLM_SRVC_CLSFCTN_TYPE_CD': 'str',
                                'NCH_PRMRY_PYR_CD': 'str',
                                'ICD_DGNS_CD1': 'str',
                                'ICD_DGNS_CD2': 'str',
                                'ICD_DGNS_CD3': 'str',
                                'ICD_DGNS_CD4': 'str',
                                'ICD_DGNS_CD5': 'str',
                                'ICD_DGNS_CD6': 'str',
                                'ICD_DGNS_CD7': 'str',
                                'ICD_DGNS_CD8': 'str',
                                'ICD_DGNS_CD9': 'str',
                                'ICD_DGNS_CD10': 'str',
                                'ICD_DGNS_CD11': 'str',
                                'ICD_DGNS_CD13': 'str',
                                'ICD_DGNS_CD14': 'str',
                                'ICD_DGNS_CD15': 'str',
                                'ICD_DGNS_CD16': 'str',
                                'ICD_DGNS_CD17': 'str',
                                'ICD_DGNS_CD18': 'str',
                                'ICD_DGNS_CD19': 'str',
                                'ICD_DGNS_CD20': 'str',
                                'ICD_DGNS_CD21': 'str',
                                'ICD_DGNS_CD22': 'str',
                                'ICD_DGNS_CD23': 'str',
                                'ICD_DGNS_CD24': 'str',
                                'ICD_DGNS_CD25': 'str',
                                'ICD_DGNS_CD12': 'str'},
                         parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT', 'FI_CLM_PROC_DT'],
                         date_parser=parser)
        dfhead = RenameVars(df)
        missDate = pd.to_datetime('1/1/1970')
        for a in range(1,25):
            try:
                var = 'Dx' + str(a)
                dfhead[var].fillna(' ', inplace=True)
            except:
                pass
            try:
                var = 'Px' + str(a)
                dfhead[var].fillna(' ', inplace=True)
                var = 'Px' + str(a) + 'Date'
                dfhead[var].fillna(missDate, inplace=True)
                dfhead[var] = dfhead[var].apply(lambda x: str(x))
            except:
                pass
        Save(dfhead, InputFeather + '/dfophead')
    try:
        dfline = Use(InputFeather + '/dfopline')
        print('  Use method applied for dfopline')
    except:
        fn = getFN('outrev')
        df = pd.read_csv(fn, delimiter='|',
                         dtype={'REV_CNTR': 'str',
                                'REV_CNTR_APC_HIPPS_CD': 'str',
                                'HCPCS_CD': 'str',
                                'HCPCS_1ST_MDFR_CD': 'str',
                                'HCPCS_2ND_MDFR_CD': 'str',
                                'REV_CNTR_IDE_NDC_UPC_NUM': 'str',
                                'REV_CNTR_DT': 'str'},
                         parse_dates=['CLM_THRU_DT'],
                         date_parser=parser)
        dfline = RenameVars(df)
        df.RevCodeDate.fillna('01JAN1970', inplace=True)
        df['RevCodeDate'] = df.RevCodeDate.apply(lambda x: pd.to_datetime(x, format='%d%b%Y'))
        dfline.CPT.fillna('Blank', inplace=True)
        xw = Use('c:/AdvAnalytics/Reference/xw_CPT2BETOS.feather')
        dfline = pd.merge(dfline, xw, how='left')
        xw = Use('c:/AdvAnalytics/Reference/ref_BETOS')
        xw = xw[['BETOS', 'Level3Group_lbl']]
        xw.columns = ['BETOS', 'BETOSLevel3Group']
        xw.BETOSLevel3Group.replace({'Durable Medical Equipment': 'DME',
                                     'Evaluation and Management': 'E&M',
                                     'Exceptions / Unclassified': 'Other',
                                     'Imaging': 'Imaging',
                                     'Tests': 'Lab'}, inplace=True)
        xw.loc[xw.BETOS.isin(['D1G', 'O1E']), 'BETOSLevel3Group'] = 'Drugs'
        xw.loc[xw.BETOS.isin(['O1D']), 'BETOSLevel3Group'] = 'Chemo'
        xw.loc[xw.BETOS.isin(['P7A']), 'BETOSLevel3Group'] = 'RadOnc'
        xw.loc[xw.BETOS.isin(['T2A', 'T2B', 'T2C', 'T2D']), 'BETOSLevel3Group'] = 'Other'
        dfline = pd.merge(dfline, xw)
        dfline.BETOSLevel3Group.fillna('Other', inplace=True)
        dfline.loc[(dfline.RevCode.between('0450', '0459')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Emerg'
        dfline.loc[(dfline.RevCode.between('0250', '0259')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Drugs'
        dfline.loc[(dfline.RevCode.between('0290', '0299')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'DME'
        dfline.loc[(dfline.RevCode.between('0300', '0319')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Lab'
        dfline.loc[(dfline.RevCode.between('0320', '0329')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Imaging'
        dfline.loc[(dfline.RevCode.between('0330', '0339')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'RadOnc'
        dfline.loc[(dfline.RevCode.between('0250', '0259')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Drugs'
        dfline.loc[(dfline.RevCode.between('0350', '0359')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Imaging'
        dfline.loc[(dfline.RevCode.between('0360', '0379')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Procedures'
        dfline.loc[(dfline.RevCode.between('0400', '0409')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Imaging'
        dfline.loc[(dfline.RevCode.between('0610', '0619')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Imaging'
        dfline.loc[(dfline.RevCode.between('0631', '0639')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Drugs'
        dfline.loc[(dfline.RevCode.between('0710', '0719')) & (dfline.BETOSLevel3Group == 'Other'),
                 'BETOSLevel3Group'] = 'Procedures'
        Save(dfline, InputFeather + '/dfopline')
    dfop = pd.merge(dfhead, dfline)
    Save(dfop, InputFeather + '/dfop')
    return dfhead, dfline, dfop


def compileDrugs():
    dfphy = Use('dfphy')
    dfdme = Use('dfdme')
    dfop = Use('dfop')
    dfPartD = Use('dfPartD')
    dffiltNDC = Use('c:/AdvAnalytics/OCM/Reference/list_InitiatingNDC')
    dffiltNDC['Filter'] = 'Y'
    dffiltCPT = Use('c:/AdvAnalytics/OCM/Reference/list_InitiatingCPT')
    dffiltCPT['Filter'] = 'Y'
    dfphy = dfphy[dfphy.BETOS.isin(['O1D', 'O1E'])]
    dfphy = dfphy[['EpiNum', 'BeneSK', 'CPT', 'LineFromDate', 'LinePaid']]
    dfphy['ClaimType'] = 'Phys'
    dfop['ClaimType'] = 'OP'
    dfop = dfop[['EpiNum', 'BeneSK', 'CPT', 'RevCodeDate', 'LinePaid', 'ClaimType']]
    listCols = ['EpiNum', 'BeneSK', 'CPT', 'ServiceDate', 'Paid', 'ClaimType']
    dfphy.columns = listCols
    dfop.columns = listCols
    dfB = pd.concat([dfphy, dfop])
    dfB = pd.merge(dfB, dffiltCPT, on='CPT', how='left')
    dfB.rename(columns={'CPT': 'HCPCS'}, inplace=True)
    xw = Use('c:/AdvAnalytics/Reference/xw_HCPCS2NDC.feather')
    dfB = pd.merge(dfB, xw, on='HCPCS')
    listCols = ['Filter', 'EpiNum', 'BeneSK', 'NDC', 'ServiceDate', 'Paid', 'ClaimType']
    dfB = dfB[listCols]
    dfdme = dfdme[dfdme.NDC.notnull()]
    dfdme = dfdme[['EpiNum', 'BeneSK', 'NDC', 'LineFromDate', 'LinePaid']]
    dfdme['ClaimType'] = 'DME'
    dfdme['NDC9'] = dfdme.NDC.apply(lambda x: x[:9])
    dfdme = pd.merge(dfdme, dffiltNDC, on='NDC9', how='left')
    del dfdme['NDC9']
    dfdme.rename(columns={'LineFromDate': 'ServiceDate',
                          'LinePaid': 'Paid'}, inplace=True)
    dfdme = dfdme[listCols]
    dfPartD['ClaimType'] = 'Oral'
    dfPartD['NDC9'] = dfPartD.NDC.apply(lambda x: x[:9])
    dfPartD = pd.merge(dfPartD, dffiltNDC, on='NDC9', how='left')
    del dfPartD['NDC9']
    dfPartD = dfPartD[listCols]
    dfdrugs = pd.concat([dfB, dfdme, dfPartD])
    Save(dfdrugs, AggWorking + '/dfdrugs')
    return dfdrugs


def findTrigger(dfdrugs):
    dfdrugs = dfdrugs[dfdrugs.Filter=='Y']
    dfdrugs.sort_values(['BeneSK', 'ServiceDate'], ascending=[1, 1], inplace=True)
    dftrigger = dfdrugs.groupby(['BeneSK']).first().reset_index()
    Save(dftrigger, AggWorking + '/dftrigger')
    return dftrigger


def findMEOS(dfop, dfphy):
    dfmeosp = dfphy[dfphy.CPT == 'G9678']
    dfmeoso = dfop[dfop.CPT == 'G9678']
    dfmeosp = dfmeosp[['EpiNum', 'BeneSK', 'LineFromDate', 'LinePaid', 'TaxID', 'NPI']]
    dfmeoso = dfmeoso[['EpiNum', 'BeneSK', 'RevCodeDate', 'LinePaid', 'CCN', 'AttendingNPI']]
    listCols = ['EpiNum', 'BeneSK', 'ServiceDate', 'LinePaid', 'TaxIDCCN', 'NPI']
    dfmeosp.columns = listCols
    dfmeoso.columns = listCols
    dfmeos = pd.concat([dfmeoso, dfmeosp])
    Save(dfmeos, AggWorking + '/dfmeos')
    return dfmeos


def getDirs(client):
    subfolders = [f.path for f in os.scandir('c:/AdvAnalytics/OCM/' + client) if f.is_dir() ]
    listDirs = []
    for d in subfolders:
        if 'Quarter' in d:
            listDirs.append(d)
    return listDirs


def appendFiles(client, listdf):
    print(ctime() + ': Appending quarterly files together')
    subfolders = [f.path for f in os.scandir('c:/AdvAnalytics/OCM/' + client) if f.is_dir()]
    listDirs = []
    for d in subfolders:
        if 'Quarter' in d:
            listDirs.append(d)
    for f in listdf:
        for i in range(len(listDirs)):
            dfname = 'df' + str(i)
            fn = listDirs[i] + '/Input/' + f + '.feather'
            cmd2exec = dfname + "= Use('" + fn + "')"
            exec(cmd2exec)
        cmd2exec = f + ' = pd.concat([df0'
        for i in range(1, len(listDirs)):
            cmd2exec += ', df' + str(i)
        cmd2exec += '], axis=0, ignore_index=True)'
        exec(cmd2exec)
        cmd2exec = "Save(" + f + ", '" + AggWorking + '/' + f + "')"
        exec(cmd2exec)
    return dfbene, dfdme, dfhha, dfhs, dfip, dfiprev, dfop, dfophead, dfopline, dfPartD, dfphy, dfphyhead, dfphyline, dfsnf


def auditMEOS(dataEndDate):
    dftrigger = Use('dftrigger')
    dfbene = Use('dfbene')
    dfbene.sort_values(['BeneSK', 'ChemoStartDate'])
    dfbene['CancerType'] = dfbene.CancerType.apply(lambda x: x[:40])
    dfbene = dfbene.groupby('BeneSK').last().reset_index()
    dfNDC = Use('c:/AdvAnalytics/Reference/ref_NDC')
    dfNDC = dfNDC[['NDC', 'Drug_Name']]
    dfNDC.columns = ['NDC', 'DrugName']
    dfNDC.drop_duplicates(subset='NDC', inplace=True)
    dftrigger = pd.merge(dftrigger, dfNDC, on='NDC', how='left')
    dfaudit = pd.merge(dfbene, dftrigger, on='BeneSK')
    dfaudit = dfaudit[['BeneSK', 'HICNumber', 'PatientName', 'BirthDate', 'CancerType', 'DeathDate',
                       'ServiceDate', 'NDC', 'DrugName', 'EMVisitsYou', 'EMVisitsAllProviders']]
    dfhs = Use('dfhs')
    dfG = dfhs.groupby('BeneSK')
    dfA = dfG.agg({'FromDate': {'HospiceStart': 'min'}})
    dfA = postAgg(dfA)
    dfaudit = pd.merge(dfaudit, dfA, on='BeneSK', how='left')
    dfaudit.HospiceStart.fillna(pd.to_datetime('12/31/2088'), inplace=True)

    dfmeos = Use('dfmeos')
    dfG = dfmeos.groupby('BeneSK')
    dfA = dfG.agg({'ServiceDate': {'FirstBillDate': 'min',
                                   'LastBillDate': 'max'},
                   'LinePaid': {'MEOSPaid': 'sum',
                                'MEOSClaims': 'count'}})
    dfA = postAgg(dfA)
    dfA['MEOSClaims'] = dfA.MEOSClaims.astype('int16')
    dfaudit = pd.merge(dfaudit, dfA, on='BeneSK', how='left')
    for c in ['MEOSPaid', 'MEOSClaims']:
        dfaudit[c].fillna(0, inplace=True)
    for c in ['FirstBillDate', 'LastBillDate']:
        dfaudit[c].fillna(pd.to_datetime('1/1/1970'), inplace=True)
    dfaudit['EpisodeEndDate'] = dfaudit.ServiceDate + np.timedelta64(6, 'M') - np.timedelta64(1, 'D')
    dfaudit.DeathDate.replace({pd.to_datetime('1,1,1970'): pd.to_datetime('12/31/2088')}, inplace=True)
    dfaudit.loc[dfaudit.EpisodeEndDate > dataEndDate, 'EpisodeEndDate'] = dataEndDate
    dfaudit.loc[dfaudit.DeathDate < dfaudit.EpisodeEndDate, 'EpisodeEndDate'] = dfaudit.DeathDate
    dfaudit.loc[dfaudit.HospiceStart < dfaudit.EpisodeEndDate, 'EpisodeEndDate'] = dfaudit.HospiceStart
    dfaudit['MEOSEligibleMonths'] = ((dfaudit.EpisodeEndDate - dfaudit.ServiceDate) / np.timedelta64(1, 'M')).astype('float32')
    dfaudit['MEOSEligibleMonths'] = dfaudit['MEOSEligibleMonths'].apply(lambda x: math.ceil(x))
    dfaudit['PctVisits'] = dfaudit.EMVisitsYou / dfaudit.EMVisitsAllProviders
    dfaudit = dfaudit[['HICNumber', 'PatientName', 'BirthDate', 'CancerType',
                       'ServiceDate', 'EpisodeEndDate',
                       'NDC', 'DrugName', 'EMVisitsYou', 'EMVisitsAllProviders', 'PctVisits',
                       'FirstBillDate', 'LastBillDate', 'MEOSPaid', 'MEOSClaims', 'MEOSEligibleMonths']]
    Save(dfaudit, AggOutput + '/dfaudit')
    dfaudit.columns = ['HIC #', 'Patient Name', 'Birth Date', 'Cancer Type', 'Initiation',
                       'Last Day', 'NDC', 'Drug Name', 'You', 'Total',
                       'Percent to You', 'First Claim', 'Last Claim', '$Paid', '# Bills', 'MEOS Elig Months']
    WSA = ['Patient Name', 'Cancer Type', 'Drug Name']
    ColumnNames = {}
    ColumnGroups = [['Initiation', 'Last Day', 'Episode Start and End'],
                    ['NDC', 'Drug Name', 'Initiating Drug'],
                    ['You', 'Percent to You', 'E&M Services'],
                    ['First Claim', '# Bills', 'MEOS Claims']]
    Excel = xlsxwriter.Workbook(AggOutput + '/MEOS Audit.xlsx', {'nan_inf_to_errors': True})
    fewSpreadsheets(DF=dfaudit,
                    workbook=Excel,
                    sheet='MEOS Audit Results',
                    title='MEOS Audit Results',
                    notes=['Part D drugs are not checked to see if there is a visit within 59 days.',
                           'Data are cumulative through most current OCM quarterly file.  More recent claims may exist.',
                           'E&M visits reflect CMMI reporting and stretch back before episode start date.',
                           'Episode End Date is the earlier of 6 months after episode initiation or the end of the quarterly data files.'],
                    WSA=WSA,
                    ColumnNames=ColumnNames,
                    ColumnGroups=ColumnGroups,
                    DataSource='CMMI Quarterly Files (cumulative)',
                    images=[],
                    minColWidth=9,
                    headerLines=3)
    Excel.close()


listDirs = getDirs(client)

createDirectories(client)

for dir in listDirs:
    InputFeather = dir + '/Input'
    Working = dir + '/Working'
    Output = dir + '/Output'
    os.chdir(dir)
    print(os.getcwd())
    # Read all quarterly files
    dfbene= readBeneFile()
    dfdme = readDMEQuarterly()
    dfhha = readHHAQuarterly()
    dfhs = readHospiceQuarterly()
    dfsnf = readSNFQuarterly()
    dfip, dfiprev = readIPQuarterly()
    dfPartD = readRxQuarterly()
    dfophead, dfopline, dfop = readOPQuarterly()
    dfphyhead, dfphyline, dfphy = readphyQuarterly()

# Append the quarterly data files together
listdf = ['dfbene', 'dfdme', 'dfhha', 'dfhs', 'dfip', 'dfiprev', 'dfPartD',
          'dfop', 'dfophead', 'dfopline', 'dfphyhead', 'dfphyline', 'dfphy', 'dfsnf']
dfbene, dfdme, dfhha, dfhs, dfip, dfiprev, dfop, dfophead, dfopline, dfPartD, dfphy, dfphyhead, dfphyline, dfsnf = \
    appendFiles(client, listdf)

os.chdir(AggWorking)
dfdrugs = compileDrugs()

dftrigger = findTrigger(dfdrugs)

dfmeos = findMEOS(dfop, dfphy)

auditMEOS(dataEndDate)
