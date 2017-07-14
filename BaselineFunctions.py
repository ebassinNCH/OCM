import pandas as pd
import numpy as np
import os
from time import ctime
import sys
sys.path.append('c:/code/general')
from NCHGeneral import *


global parser
parser = lambda date: pd.datetime.strptime(date, '%d%b%Y')

############################################
### General functions to read data files ###
############################################
def readEpi():
    fn = getFN('episodes')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'ZIPCODE': 'str'},
                     parse_dates=['DOB', 'EP_BEG', 'EP_END', 'DOD'],
                     infer_datetime_format=True)
    df = RenameVars(df)
    df['LastName'] = df.LastName.apply(lambda x: str(x))
    df['FirstName'] = df.FirstName.apply(lambda x: str(x))
    df['PatientName'] = df.LastName.apply(lambda x: x.strip().title()) + ', ' + df.FirstName.apply(lambda x: x.strip().title())
    del df['LastName']
    del df['FirstName']
    print('Number of episode runs at the end of readEpi: ' + str(len(df.index)))
    listVars = []
    return df


def readDME():
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
    dfdme = pd.merge(dfhead, dfline, on='ClaimNum', how='inner')
    dfdme.CPTMod2.fillna(' ', inplace=True)
    return dfdme


def readHHA():
    fn = getFN('hhahead')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'PRVDR_NUM':'str'},
                     parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT', 'FI_CLM_PROC_DT'],
                     date_parser=parser)
    df = RenameVars(df)
    return df


def readHospice():
    fn = getFN('hsphead')
    df = pd.read_csv(fn, delimiter='|', dtype={'PRVDR_NUM': 'str'},
                     parse_dates=['CLM_FROM_DT', 'CLM_THRU_DT', 'NCH_WKLY_PROC_DT',
                                  'FI_CLM_PROC_DT', 'CLM_HOSPC_START_DT_ID'],
                     date_parser=parser)
    df = RenameVars(df)
    return df


def readIP():
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
    for c in ['AdmitDate', 'DischargeDate']:
        df[c].fillna('01JAN1970', inplace=True)
        df[c] = df[c].apply(lambda x: pd.to_datetime(x, format='%d%b%Y'))
    fn = getFN('inprev')
    df = pd.read_csv(fn, delimiter='|',
                     dtype={'REV_CNTR': 'str'},
                     parse_dates=['CLM_THRU_DT'],
                     date_parser=parser)
    dfline = RenameVars(df)
    dfline = pd.merge(dfline, dfhead, how='left')
    return dfhead, dfline


def readSNF():
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
    return df


def readRx():
    fn = getFN('_pde_')
    df = pd.read_csv(fn, delimiter='|',
                     parse_dates=['SRVC_DT'],
                     date_parser=parser,
                     dtype={'PROD_SRVC_ID': 'str'})
    df = RenameVars(df)
    df['Paid'] = df.LICSPaid + 0.8*df.CostAboveCatastrophic
    return df


os.chdir('c:/AdvAnalytics/CCSI/BaselineUpdated')
InputFeather = os.getcwd() + '/Input'
try:
    os.makedirs(InputFeather)
except:
    pass

dfepi = readEpi()
'''
Save(dfepi, InputFeather + '/dfepi')

dfDME = readDME()
Save(dfDME, InputFeather + '/dfDME')
dfDMEDrugs = dfDME[dfDME.NDC.notnull()]
Save(dfDMEDrugs, InputFeather + '/dfDMEDrugs')
print('Number of rows in DME drug dataframe after reading: ' + str(len(dfDMEDrugs.index)))
print('Number of rows in DME file after reading: ' + str(len(dfDME.index)))

dfHHA = readHHA()
print('Number of rows in HHA file after reading: ' + str(len(dfHHA.index)))
Save(dfHHA, InputFeather + '/dfHHA')

dfHS = readHospice()
print('Number of rows in Hospice file after reading: ' + str(len(dfHS.index)))
Save(dfHS, InputFeather + '/dfHS')

dfip, dfiprev = readIP()
print('Number of rows in IP header file after reading: ' + str(len(dfip.index)))
print('Number of rows in IP revenue file after reading: ' + str(len(dfiprev.index)))
Save(dfip, InputFeather + '/dfip')
Save(dfiprev, InputFeather + '/dfiprev')

dfsnf = readSNF()
print('Number of rows in SNF header file after reading: ' + str(len(dfsnf.index)))
Save(dfsnf, InputFeather + '/dfsnf')

dfPartD = readRx()
print('Number of rows in Part D file after reading: ' + str(len(dfPartD.index)))
Save(dfPartD, InputFeather + '/dfPartD')
'''


