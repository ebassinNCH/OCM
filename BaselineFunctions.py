import pandas as pd
import numpy as np
import os
from time import ctime
import pyodbc
import sys
sys.path.append('c:/code/general')
from NCHGeneral import Use, Save, fewSpreadsheets, postAgg, getFN, RenameVars, toMySQL
from NCHGeneral import *
pd.options.mode.chained_assignment = None  # default='warn'
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

global parser
global InputFeather
parser = lambda date: pd.datetime.strptime(date, '%d%b%Y')

os.chdir('c:/AdvAnalytics/OCM/CCSI/BaselineUpdated')

############################################
### General functions to read data files ###
############################################
def createDirectories():
    '''
    This function creates the directory structure used for the baseline analysis and sets global variables that are
    used to determine the subdirectory to which files are written.
    :return: None
    '''
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
    dfdme = pd.merge(dfhead, dfline, how='left')
    dfdme.CPTMod2.fillna(' ', inplace=True)
    xwndc = Use('c:/AdvAnalytics/Reference/ref_NDC')
    xwndc = xwndc[['NDC', 'GPI']]
    xwndc.drop_duplicates(subset=['NDC'], inplace=True)
    xwndc.GPI.fillna(' ', inplace=True)
    xwndc['GPI10'] = xwndc.GPI.apply(lambda x: x[:10])
    dfdme = pd.merge(dfdme, xwndc, on='NDC', how='left')
    Save(dfdme, InputFeather + '/dfdme')
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
    df['LOS'] = (df.ThruDate - df.FromDate)/np.timedelta64(1, 'D')
    return df


def readIP():
    try:
        dfhead = Use(InputFeather + '/dfip2')
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
        dfline = Use(InputFeather + '/dfiprev2')
    except:
        fn = getFN('inprev')
        df = pd.read_csv(fn, delimiter='|',
                         dtype={'REV_CNTR': 'str'},
                         parse_dates=['CLM_THRU_DT'],
                         date_parser=parser)
        dfline = RenameVars(df)
        dfline = pd.merge(dfline, dfhead, how='left')
        dfemerg = dfline[dfline.RevCode.between('045', '0459')]
        dfemerg['EmergencyAdmit'] = 'Emerg'
        dfemerg = dfemerg[['ClaimNum', 'EmergencyAdmit']]
        dfemerg.drop_duplicates(inplace=True)
        dfhead['ClaimNum'] = dfhead.ClaimNum.apply(lambda x: str(x))
        dfemerg['ClaimNum'] = dfemerg.ClaimNum.apply(lambda x: str(x))
        dfhead = pd.merge(dfhead, dfemerg, on='ClaimNum', how='left')
        dfhead['EmergencyAdmit'].fillna('No ER', inplace=True)
        dficu = calcICUDays(dfline)
        dficu['ClaimNum'] = dficu.ClaimNum.apply(lambda x: str(x))
        dfhead = pd.merge(dfhead, dficu, on=['EpiNum', 'ClaimNum'], how='left')
        dfhead.ICUDays.fillna(0, inplace=True)
    return dfhead, dfline, dficu


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
    try:
        df = Use(InputFeather + '/dfPartD2')
    except:
        fn = getFN('_pde_')
        df = pd.read_csv(fn, delimiter='|',
                         parse_dates=['SRVC_DT'],
                         date_parser=parser,
                         dtype={'PROD_SRVC_ID': 'str'})
        print('Rows in dfPartD after reading: ' + str(len(df.index)))
        df = RenameVars(df)
        df['Paid'] = df.LICSPaid + 0.8*df.CostAboveCatastrophic
        xwndc = Use('c:/AdvAnalytics/Reference/ref_NDC')
        xwndc = xwndc[['NDC', 'GPI']]
        xwndc.drop_duplicates(subset=['NDC'], inplace=True)
        xw = Use('c:/AdvAnalytics/Reference/ref_GPI')
        xw = xw[['GPI', 'TherapeuticClassLevel1', 'DrugName']]
        xw = pd.merge(xw, xwndc)
        xw.drop_duplicates(subset=['NDC'], inplace=True)
        df = pd.merge(df, xw, on='NDC', how='left')
        df['GPI'] = df.GPI.apply(lambda x: str(x))
        df['GPI'] = df.GPI.apply(lambda x: x.zfill(14))
        df['GPI10'] = df.GPI.apply(lambda x: x[:10])
        print('Rows in dfPartD before Saving: ' + str(len(df.index)))
        Save(df, InputFeather + '/dfPartD')
    return df


def readphy():
    fn = getFN('phyhead')
    try:
        dfhead = Use(InputFeather + '/dfphyhead')
        print('  Use method applied for dfphyhead')
        Save(dfphyhead, InputFeather + '/dfphyhead')
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
        for c in ['LineNum', 'PricingLocality', 'LineMTUSCount', 'LineMTUSCode', 'LineDxVersion']:
            del dfline[c]
        dfline['Services'] = dfline.Services.apply(lambda x: x.astype('int16'))
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
        dfline = pd.merge(dfline, xw, how='left')
        '''
        conn = pyodbc.connect('DRIVER={SQL Server};SERVER=BIDATACA2;DATABASE=EDW;UID=ebassin;\
                              PWD=ebassin;Trusted_Connection=yes')
        ## This section adds the 10 digit GPI code to the claim line where appropriate.
        # Build the J Code to GPI crosswalk
        myQuery = 'SELECT ServiceCode, NdcNumber FROM EDW.MSTR.AvgSalesPrice_NDC'
        xw = pd.read_sql(myQuery, conn)
        xw.columns = ['CPT', 'NDC']
        xw['NDC'] = xw.NDC.apply(lambda x: x.replace('-', ''))
        xw['NDC9'] = xw.NDC.apply(lambda x: x[:9])
        del xw['NDC']
        Save(xw, 'c:/temp/xw_CPT2NDC9')
        '''
        #xw = Use('c:/AdvAnalytics/Reference/xw_CPT2BETOS')
        #dfline = pd.merge(dfline, xw, on='CPT', how='left')
        xw = Use('c:/temp/xw_CPT2NDC9')
        dfline = pd.merge(dfline, xw, on='CPT', how='left')
        xw = Use('/AdvAnalytics/Reference/ref_NDC')
        xw['NDC9'] = xw.NDC.apply(lambda x: x[:9])
        xw['GPI'] = xw.GPI.apply(lambda x: str(x))
        xw['GPI'] = xw.GPI.apply(lambda x: x.zfill(14))
        xw.GPI.fillna('MISSING', inplace=True)
        xw['GPI10'] = xw.GPI.apply(lambda x: x[:10])
        xw = xw[['NDC9', 'GPI10']]
        xw.drop_duplicates(subset='NDC9', inplace=True)
        Save(xw, 'c:/temp/NDC9ToGPI10')
        xw.set_index('NDC9', inplace=True)
        dictNDC = xw.to_dict()
        print(dfline.info())
        dfline['GPI10'] = dfline.NDC9.map(dictNDC)
        # dfline = pd.merge(dfline, xw, on='NDC9', how='left')
        Save(dfline, InputFeather + '/dfphyline')
    print(dfline.columns.tolist())
    print(dfhead.columns.tolist())
    dfphy = pd.merge(dfline, dfhead)
    Save(dfphy, InputFeather + '/dfphy')
    return dfhead, dfline, dfphy


def readOP():
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
        Save(dfophead, InputFeather + '/dfophead')
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
                                'REV_CNTR_IDE_NDC_UPC_NUM': 'str'},
                         parse_dates=['CLM_THRU_DT', 'REV_CNTR_DT'],
                         date_parser=parser)
        dfline = RenameVars(df)
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
        # Add the GPI 10 to the claim line.  This section uses NDC if it exists and uses the J Code if the NDC
        # does not exist.
        xw = Use('c:/temp/xw_CPT2NDC9')
        xw.drop_duplicates(subset='CPT', inplace=True)
        xw['NDC9'] = xw.NDC9.apply(lambda x: str(x))
        xw['NDC9'] = xw.NDC9.apply(lambda x: x.zfill(9))
        dfline = pd.merge(dfline, xw, on='CPT', how='left')
        xw = Use('/AdvAnalytics/Reference/ref_NDC')
        xw['NDC9'] = xw.NDC.apply(lambda x: x[:9])
        xw.GPI.fillna('MISSING', inplace=True)
        xw['GPI'] = xw.GPI.apply(lambda x: str(x))
        xw['GPI'] = xw.GPI.apply(lambda x: x.zfill(14))
        xw['GPI10'] = xw.GPI.apply(lambda x: x[:10])
        xw = xw[['NDC9', 'GPI10']]
        xw.drop_duplicates(subset='NDC9', inplace=True)
        dfline = pd.merge(dfline, xw, on='NDC9', how='left')
        dfline.rename(columns={'GPI10': 'GPICPT'}, inplace=True)
        dfline['NDC'] = dfline.NDC.apply(lambda x: str(x))
        dfline['NDC'] = dfline.NDC.apply(lambda x: x.zfill(11))
        dfline['NDC9'] = dfline.NDC.apply(lambda x: x[:9])
        dfline = pd.merge(dfline, xw, on='NDC9', how='left')
        dfline.loc[dfline.GPI10.isnull(), 'GPI10'] = dfline.GPICPT
        dfline.to_csv('c:/temp/op.csv')
        #Save(dfline, InputFeather + '/dfopline')
        Save(dfline, InputFeather + '/dfopline')
    dfop = pd.merge(dfhead, dfline)
    # dfop.to_csv('c:/temp/dfop.csv')
    Save(dfop, InputFeather + '/dfop')
    return dfhead, dfline, dfop


def calcICUDays(dfiprev):
    dfepi = Use(InputFeather + '/dfepi')
    dfepi = dfepi[['EpiNum', 'DeathDate']]
    dfiprev = dfiprev[dfiprev.RevCode.between('0200', '0204')]
    dfiprev = pd.merge(dfiprev, dfepi, on='EpiNum')
    dfiprev = dfiprev[['ClaimNum', 'Units', 'DeathDate', 'ThruDate', 'EpiNum']]
    dfiprev['DaysToDeath'] = (dfiprev.DeathDate - dfiprev.ThruDate) / np.timedelta64(1, 'D')
    dfiprev['ICULast14Days'] = np.where(dfiprev.DaysToDeath.between(0, 14), 1., 0.)
    dfiprev = dfiprev[['ClaimNum', 'Units', 'ICULast14Days', 'EpiNum']]
    dfiprev = dfiprev.groupby('ClaimNum').agg('sum').reset_index()
    dfiprev.rename(columns={'Units': 'ICUDays'}, inplace=True)
    return dfiprev


def addICU2Epi(dficu, dfepi):
    dfG = dficu.groupby('EpiNum')
    dfA = dfG.agg({'ICUDays' : {'ICUDays': 'sum',
                                'ICUStays': 'count'},
                   'ICULast14Days': {'ICULast14Days': 'sum'}})
    dfA = postAgg(dfA)
    dfA.loc[dfA['ICULast14Days']>1, 'ICULast14Days']=1.
    print('    dficu episodes: %d' % (len(dfA.index)) )
    dfepi = pd.merge(dfepi, dfA, on='EpiNum', how='left')
    listCols = ['ICUDays', 'ICUStays', 'ICULast14Days']
    for c in listCols:
        dfepi[c].fillna(0, inplace=True)
    return dfepi


def addTOSMerge(df, dfepi):
    listCols = df.columns.tolist()
    listCols.remove('EpiNum')
    dfepi = pd.merge(dfepi, df, how='left')
    for c in listCols:
        dfepi[c].fillna(0, inplace=True)
    return dfepi


def addTOSCostToEpi(dfepi, dfphy, dfop, dfip, dfsnf, dfhha, dfhs, dfPartD, dfDME):
    '''
    This function adds cost and utilization statistics by type of service to the episode file.

    :param dfepi: The Episode Summary dataframe
    :param dfphy: the Part B "phy" claims at the line level
    :param dfop: the Part B hospital outpatient claims at the line level
    :param dfip: the inpatient hospital claims at the header level
    :param dfsnf: the SNF claims dataframe at the header level
    :param dfhha: the home health claims dataframe at the header level
    :param dfhs: the hospice claims dataframe at the header level
    :param dfPartD: the Part D (orals) dataframe at the line level
    :param dfDME: the DME claims at the line level
    :return: an enhanced episode summary dataframe
    '''
    # Start by breaking the "phy" claims down into BETOS driven types of service
    print('Starting addTOSCostToEpi at ' + ctime())
    dfdeath = dfepi[['EpiNum', 'DeathDate']]
    dfphy = pd.merge(dfdeath, dfphy, on='EpiNum')
    dfphy['DaysBeforeDeath'] = (dfphy.DeathDate - dfphy.LineFromDate) / np.timedelta64(1, 'D')
    dfphy.loc[dfphy.DaysBeforeDeath < 0, 'DaysBeforeDeath'] = 999
    dfphy['Last14Days'] = dfphy.DaysBeforeDeath.apply(lambda x: np.where(x<=14, 1, 0))
    dfphy['PaidLast14'] = dfphy.LinePaid * dfphy.Last14Days
    dfG = dfphy.groupby(['EpiNum', 'BETOSLevel3Group'])
    dfA = dfG.agg({'LinePaid': {'Paid': 'sum'}})
    dfA.columns = dfA.columns.droplevel(0)
    dfB = dfA.copy()
    dfB = dfB.unstack(level=-1)
    dfB.columns = dfB.columns.droplevel(0)
    for c in dfB.columns.tolist():
        newc = 'phyTOSPaid' + c
        dfB.rename(columns={c: newc}, inplace=True)
    dfB.fillna(0, inplace=True)
    dfG = dfphy.groupby(['EpiNum', 'BETOSLevel3Group'])
    dfA = dfG.agg({'LinePaid': {'Services': 'count'}})
    dfA.columns = dfA.columns.droplevel(0)
    dfC = dfA.unstack(level=1)
    dfC.columns = dfC.columns.droplevel(0)
    for c in dfC.columns.tolist():
        newc = 'phyTOSServices' + c
        dfC.rename(columns={c: newc}, inplace=True)
    dfC.fillna(0, inplace=True)
    dfG = dfphy.groupby(['EpiNum', 'BETOSLevel3Group'])
    dfD = dfG.agg({'PaidLast14': {'PaidLast14': 'sum'}})
    dfD.columns = dfD.columns.droplevel(0)
    dfD = dfD.unstack(level=1)
    dfD.columns = dfD.columns.droplevel(0)
    for c in dfD.columns.tolist():
        newc = 'phyTOSPaidLast14' + c
        dfD.rename(columns={c: newc}, inplace=True)
    dfD.fillna(0, inplace=True)
    dfphy = pd.merge(dfB, dfC, left_index=True, right_index=True)
    dfphy = pd.merge(dfD, dfphy, left_index=True, right_index=True)
    dfphy.reset_index(inplace=True)
    listCols = dfphy.columns.tolist()
    listCols.remove('EpiNum')
    dfepi = pd.merge(dfepi, dfphy, how='left')
    print('    After dfphy merge, there are %d episodes' % (len(dfepi.index)))
    dfepi[listCols].fillna(0, inplace=True)

    # Now do something very similary with the OP claim lines
    print('  ' + ctime() + '--Starting OP')
    listGroups = pd.unique(dfop.BETOSLevel3Group)
    dfop = pd.merge(dfop, dfdeath, on='EpiNum')
    dfop['DaysBeforeDeath'] = (dfop.DeathDate - dfop.RevCodeDate) / np.timedelta64(1, 'D')
    dfop.loc[dfop.DaysBeforeDeath<0, 'DaysBeforeDeath'] = 999
    dfop['Last14'] = dfop.DaysBeforeDeath.apply(lambda x: x<=14, 1, 0)
    dfop['PaidLast14'] = dfop.Last14 * dfop.LinePaid
    dfG = dfop.groupby(['EpiNum', 'BETOSLevel3Group'])
    dfA = dfG.agg({'LinePaid': {'Paid': 'sum'}})
    dfA.columns = dfA.columns.droplevel(0)
    dfB = dfA.unstack(level=-1)
    dfB.columns = dfB.columns.droplevel(0)
    for c in dfB.columns.tolist():
        newc = 'opTOSPaid' + c
        dfB.rename(columns={c: newc}, inplace=True)
    dfB.fillna(0, inplace=True)
    dfG = dfop.groupby(['EpiNum', 'BETOSLevel3Group'])
    dfA = dfG.agg({'LinePaid': {'Services': 'count'}})
    dfA.columns = dfA.columns.droplevel(0)
    dfC = dfA.unstack(level=1)
    dfC.columns = dfC.columns.droplevel(0)
    for c in dfC.columns.tolist():
        newc = 'opTOSServices' + c
        dfC.rename(columns={c: newc}, inplace=True)
    dfC.fillna(0, inplace=True)
    dfG = dfop.groupby(['EpiNum', 'BETOSLevel3Group'])
    dfD = dfG.agg({'PaidLast14': {'PaidLast14': 'count'}})
    dfD.columns = dfD.columns.droplevel(0)
    dfD = dfD.unstack(level=1)
    dfD.columns = dfD.columns.droplevel(0)
    for c in dfD.columns.tolist():
        newc = 'opTOSPaidLast14' + c
        dfD.rename(columns={c: newc}, inplace=True)
    dfD.fillna(0, inplace=True)
    dfop = pd.merge(dfB, dfC, left_index=True, right_index=True)
    dfop = pd.merge(dfop,dfD, left_index=True, right_index=True)
    dfop.reset_index(inplace=True)
    listCols = dfop.columns.tolist()
    listCols.remove('EpiNum')
    dfepi = pd.merge(dfepi, dfop, how='left')
    print('    After dfop merge, there are %d episodes' % (len(dfepi.index)))
    dfepi[listCols].fillna(0, inplace=True)
    for c in listGroups:
        dfepi['PartBTOSPaid' + c] = dfepi['phyTOSPaid' + c] + dfepi['opTOSPaid' + c]
        dfepi['PartBTOSServices' + c] = dfepi['phyTOSServices' + c] + dfepi['opTOSServices' + c]
        dfepi['PartBLast14Paid' + c] = dfepi['phyTOSPaidLast14' + c] + dfepi['opTOSPaidLast14' + c]

    # Inpatient now.  We break out costs and utilization by whether the admit was for cancer.
    print('  ' + ctime() + '--Starting IP')
    dfip = pd.merge(dfip, dfdeath, on='EpiNum')
    dfip['DaysDischargeToDeath'] = (dfip.DeathDate - dfip.DischargeDate) / np.timedelta64(1, 'D')
    dfip.loc[dfip.DaysDischargeToDeath<-99, 'DaysDischargeToDeath'] = 999
    dfip['DischargeLast14Days'] = dfip.DaysDischargeToDeath.apply(lambda x: np.where(x<=14,1,0))
    dfip['IPDischargeLast14Paid'] = dfip.Paid * dfip.DischargeLast14Days
    dfip['DaysAdmitToDeath'] = (dfip.DeathDate - dfip.AdmitDate) / np.timedelta64(1, 'D')
    dfip.loc[dfip.DaysAdmitToDeath<-500, 'DaysAdmitToDeath'] = 999
    dfip['AdmitLast14Days'] = dfip.DaysAdmitToDeath.apply(lambda x: np.where(x<=14,1,0))
    dfip['IPDischargeLast14Paid'] = dfip.Paid * dfip.DischargeLast14Days
    dfip['IPAdmitLast14Paid'] = dfip.Paid * dfip.AdmitLast14Days
    dfG = dfip.groupby('EpiNum')
    dfA = dfG.agg({'Paid': {'IPTotalPaid': 'sum'},
                   'OncologyRelatedPaid': {'IPOncologyPaid': 'sum'},
                   'OncologyDRG': {'IPAdmits': 'count',
                                   'IPOncologyAdmits': 'sum'},
                   'AdmitLast14Days': {'AdmitLast14Days': 'sum'},
                   'IPAdmitLast14Paid': {'IPPaidAdmitLast14Days': 'sum'},
                   'DischargeLast14Days': {'DischargesLast14Days': 'sum'},
                   'IPDischargeLast14Paid': {'IPPaidDischargeLast14Days': 'sum'}
                   })
    dfA = postAgg(dfA)
    dfA['IPNonOncologyPaid'] = dfA.IPTotalPaid - dfA.IPOncologyPaid
    dfA['IPNonOncologyAdmits'] = dfA.IPAdmits - dfA.IPOncologyAdmits
    dfepi = addTOSMerge(dfA, dfepi)

    # SNF now.  I am going to aggregate cost and util days (LOS).
    print('  ' + ctime() + '--Starting SNF')
    dfG = dfsnf.groupby('EpiNum')
    dfA = dfG.agg({'Paid' : {'SNFPaid' : 'sum',
                             'SNFClaims':'count'},
                   'LOS' : {'SNFLOS' : 'sum'}})
    dfA = postAgg(dfA)
    dfepi = addTOSMerge(dfA, dfepi)

    # HHA Now
    print('  ' + ctime() + '--Starting HHA')
    dfG = dfhha.groupby('EpiNum')
    dfA = dfG.agg({'Paid' : {'HHAPaid' : 'sum',
                             'HHAClaims':'count'},
                   'VisitCount' : {'HHAVisits' : 'sum'}})
    dfA = postAgg(dfA)
    dfepi = addTOSMerge(dfA, dfepi)

    # Hospice Now
    print('  ' + ctime() + '--Starting Hospice')
    dfG = dfhs.groupby('EpiNum')
    dfA = dfG.agg({'Paid' : {'HospicePaid' : 'sum',
                             'HospiceClaims':'count'},
                   'LOS' : {'HospiceLOS' : 'sum'}})
    dfA = postAgg(dfA)
    dfepi = addTOSMerge(dfA, dfepi)

    # Part D Drugs.  I need to break drugs down by class, but that isn't an available field yet.
    print('  ' + ctime() + '--Starting Part D')
    dfPartD = pd.merge(dfPartD, dfdeath, on='EpiNum')
    dfPartD['DaysToDeath'] = (dfPartD.DeathDate - dfPartD.ServiceDate) / np.timedelta64(1, 'D')
    dfPartD.loc[dfPartD.DaysToDeath<-99, 'DaysToDeath'] = 999
    dfPartD['Last14Days'] = dfPartD.DaysToDeath.apply(lambda x: np.where(x<=14,1,0))
    dfPartD['PaidLast14'] = dfPartD.Last14Days * dfPartD.Paid
    dfPartD['CancerDrug'] = np.where(dfPartD.TherapeuticClassLevel1=='Antineoplastic Agents',1,0)
    dfPartD['CancerDrugPaid'] = dfPartD.CancerDrug * dfPartD.Paid
    dfPartD['OtherDrugPaid'] = dfPartD.Paid - dfPartD.CancerDrugPaid
    dfG = dfPartD.groupby(['EpiNum'])
    dfA = dfG.agg({'CancerDrugPaid' : {'PartDChemoPaid': 'sum'},
                   'CancerDrug' : {'PartDChemoScripts': 'sum'},
                   'OtherDrugPaid'  : {'PartDNonChemoPaid': 'sum',
                                       'PartDNonChemoScripts': 'count'},
                   'PaidLast14' : {'PartDLast14DaysPaid' : 'sum'} })
    dfA = postAgg(dfA)
    dfA['PartDNonChemoScripts'] = dfA.PartDNonChemoScripts - dfA.PartDChemoScripts
    dfepi = addTOSMerge(dfA, dfepi)

    # DME
    print('  ' + ctime() + '--Starting HHA')
    dfdme['DrugClaim'] = np.where(dfdme.NDC.notnull(),1,0)
    dfdme['DMEDrugPaid'] = dfdme.DrugClaim * dfdme.Paid
    dfdme['DMENonDrugPaid'] = dfdme.Paid - dfdme.DMEDrugPaid
    dfG = dfdme.groupby(['EpiNum'])
    dfA = dfG.agg({'DMEDrugPaid' : {'DMEDrugPaid': 'sum'},
                   'DMENonDrugPaid': {'DMENonDrugPaid': 'sum'}})
    dfA = postAgg(dfA)
    dfepi = addTOSMerge(dfA, dfepi)
    return dfepi


def getTaxID(df):
    dfx = df.groupby('TaxID').agg({'NPI': {'Rows': 'count'}})
    dfx = postAgg(dfx)
    dfx.sort_values('Rows', ascending=False, inplace=True)
    dfx.reset_index(inplace=True)
    ti = dfx.TaxID[0]
    return ti


def physicianAttribution(df, ti, dfepi):
    df = df[df.CPT.between('99201', '99499')]
    df = df[df.TaxID==ti]
    dfG = df.groupby(['EpiNum', 'NPI'])
    dfA = dfG.agg({'LinePaid': {'Claims': 'count',
                                'Paid': 'sum'},
                   'LineFromDate': {'FirstClaim': 'min'}})
    dfA = postAgg(dfA)
    dfA.sort_values(['EpiNum', 'Claims', 'Paid', 'FirstClaim'],
                    ascending=[1,0,0,1], inplace=True)
    df = dfA.groupby('EpiNum').first()
    df.reset_index(inplace=True)
    Save(df, InputFeather + '/dfattrib')
    df = df[['NPI', 'EpiNum']]
    dfnpi = Use('c:/AdvAnalytics/Reference/code_npi')
    df = pd.merge(df, dfnpi, how='left')
    df.columns = ['AttributedNPI', 'EpiNum', 'AttributedPhysicianName']
    dfepi = pd.merge(dfepi, df, on=['EpiNum'], how='left')
    return dfepi


def diedOutsideEpisode(dfepi):
    dfepi['DeathDate'].fillna(pd.to_datetime('1/1/1970'), inplace=True)
    dfepi = dfepi[dfepi.DeathDate>pd.to_datetime('1/1/1970')]
    dfepi['DiedInEpisode'] = np.where(dfepi.DeathDate.between(dfepi.EpiStart, dfepi.EpiEnd), 1., 0.)
    dfepi.sort_values(['BeneSK', 'EpiEnd'], inplace=True)
    '''
    dfG=dfepi.groupby('BeneSK')
    dfA = dfG.agg({'DiedInEpisode': {'DiedInEpisode': 'sum'}})
    dfA = postAgg(dfA)
    dfB = dfepi.groupby('BeneSK').last().reset_index()
    del dfB['DiedInEpisode']
    dfA = pd.merge(dfA, dfB, on='BeneSK')
    '''
    dfepi = dfepi.groupby('BeneSK').last().reset_index()
    dfA = dfepi[['BeneSK', 'EpiNum', 'DiedInEpisode', 'AttributedPhysicianName', 'CancerType', 'Age', 'PatientName']]
    toMySQL(dfA, 'ocm', 'DiedInEpisode')
    return dfA


def ip2PowerBI(dfip, dfepi):
    dfepi = dfepi[['EpiNum', 'HICNumber', 'PatientName', 'Age', 'Sex', 'DeathDate',
                   'CancerType', 'ActualCost', 'WinsorizedCost', 'BaselinePrice', 'EpiStart', 'EpiEnd',
                   'AttributedNPI', 'AttributedPhysicianName', 'ReconciliationEligible']]
    dfip = dfip[['FromDate', 'ThruDate', 'CCN', 'FacilityType', 'Paid', 'ProviderState',
                 'OrgNPI', 'AttendingNPI', 'SurgeonNPI', 'DischargeStatus', 'PPSIndicator',
                 'AdmitDate', 'AdmitType', 'AdmitSource', 'Deductible', 'Coinsurance',
                 'LOS', 'DischargeDate', 'DRG', 'OutlierFlag', 'OutlierPaid', 'AdmitDx', 'PrincipalDx', 'Dx1', 'Dx2',
                 'Px1', 'Px1Date', 'Px2', 'Px2Date', 'EpiNum', 'ServiceType', 'ICUDays', 'ICULast14Days',
                 'OncologyDRG', 'OncologyRelatedPaid', 'EmergencyAdmit']]
    dfccs = Use('c:/AdvAnalytics/Reference/ref_CCS')
    dfccs = dfccs[['Dx', 'CCS', 'CCS_lbl', 'Dx_lbl']]
    dfccs.rename(columns={'Dx': 'Dx1'}, inplace=True)
    dfip = pd.merge(dfip, dfccs, on='Dx1', how='left')
    dfip = pd.merge(dfip, dfepi, how='left', on='EpiNum')
    dfccn = Use('c:/AdvAnalytics/Reference/code_CCN')
    dfip = pd.merge(dfip, dfccn, on='CCN', how='left')
    dfdrg = Use('c:/AdvAnalytics/Reference/ref_DRG')
    dfdrg2 = Use('c:/AdvAnalytics/Reference/code_BaseDRG')
    dfdrg = pd.merge(dfdrg, dfdrg2, on='BaseDRG')
    dfdrg = dfdrg[['DRG', 'DRG_lbl', 'BaseDRG_lbl']]
    dfdrg.drop_duplicates(inplace=True)
    dfip = pd.merge(dfip, dfdrg, on='DRG', how='left')
    dfip.FromDate.apply(lambda x: pd.to_datetime('1/1/1970'))
    dfip['MonthsFromEpiStart'] = ((dfip.FromDate - dfip.EpiStart) / np.timedelta64(1, 'M'))
    dfip.MonthsFromEpiStart.fillna(0, inplace=True)
    dfip['MonthsFromEpiStart'] = dfip.MonthsFromEpiStart.apply(lambda x: int(x))
    dfip.DeathDate.fillna(pd.to_datetime('1/1/1970'), inplace=True)
    dfip['DischargeDaysToDeath'] = (dfip.DeathDate - dfip.DischargeDate) / np.timedelta64(1, 'D')
    dfi = dfip.copy()
    Save(dfip, Output + '/dfip4PowerBI')
    toMySQL(dfip, 'ocm', 'dfip')
    dfG = dfip.groupby('EpiNum')
    dfA = dfG.agg({'DischargeDate': {'FirstDischargeDate' : 'min',
                                     'LastDischargeDate' : 'max'},
                   'AdmitDate': {'FirstAdmitDate': 'min',
                                 'LastAdmitDate': 'max',
                                 'NumberOfAdmits': 'count'}})
    dfA = postAgg(dfA)
    dfe = dfepi[['EpiNum', 'EpiStart', 'EpiEnd', 'DeathDate']]
    dfip = pd.merge(dfA, dfe, on='EpiNum')
    dfip[['FirstAdmitDate','LastAdmitDate','FirstDischargeDate','LastDischargeDate']].fillna(
        pd.to_datetime('1/1/1970'), inplace=True)
    dfip.NumberOfAdmits.fillna(0, inplace=True)
    dfip['DischargeDaysToDeath'] = (dfip.DeathDate - dfip.DischargeDate) / np.timedelta64(1, 'D')
    dfip['DaysToFirstAdmit'] = (dfip.FirstAdmitDate - dfip.EpiStart) / np.timedelta64(1, 'D')
    dfip['DaysLastAdmitToEpiEnd'] =  (dfip.EpiEnd - dfip.LastAdmitDate) / np.timedelta64(1, 'D')
    dfip['DaystoFirstDischarge'] = (dfip.FirstDischargeDate - dfip.EpiStart) / np.timedelta64(1, 'D')
    dfip['DaysLastDischargeToEpiEnd'] = (dfip.EpiEnd - dfip.LastDischargeDate) / np.timedelta64(1, 'D')
    Save(dfip, Outpat + '/dfipnpatEpiSummary')
    toMySQL(dfip, 'ocm', 'dfipnpatEpiSummary')
    return


def combinePartB(dfop, dfphy, dfdme, dfepi):
    try:
        dfPartB = Use(Working + '/dfPartB')
    except:
        dfop['Specialty'] = 'A0'
        dfop['ServiceType'] = '#'
        dfop['ClinicalTrialNum'] = -1
        xw = Use('c:/AdvAnalytics/Reference/code_ServicePlace')
        dfphy = pd.merge(dfphy, xw, on='ServicePlace', how='left')
        dfdme = pd.merge(dfdme, xw, on='ServicePlace', how='left')
        dfphy.ServicePlace_lbl.fillna('Unknown', inplace=True)
        dfdme.ServicePlace_lbl.fillna('Unknown', inplace=True)
        dfphy['RevCode'] = 'N/A'
        dfdme['RevCode'] = 'N/A'
        dfop['LineDx'] = dfop.Dx1
        xw = Use('c:/AdvAnalytics/Reference/code_FacilityType')
        dfop = pd.merge(dfop, xw, on='FacilityType', how='left')
        dfop.rename(columns={'AttendingNPI': 'NPI',
                             'RevCodeDate': 'LineFromDate',
                             'FacilityType_lbl': 'ServicePlace_lbl',
                             'Units': 'Services'}, inplace=True)
        dfop['LineThruDate'] = dfop.LineFromDate
        dfop['TaxID'] = dfop.CCN

        xw = Use('c:/AdvAnalytics/Reference/xw_HCPCS2NDC')
        xw.columns = ['NDC', 'CPT']
        dfphy = pd.merge(dfphy, xw, on='CPT', how='left')
        dfphy = dfphy[['BeneSK', 'FromDate', 'ThruDate', 'NPI', 'TaxID', 'Specialty', 'ServicePlace_lbl', 'Services',
                       'ServiceType', 'LineFromDate', 'LineThruDate', 'CPT', 'CPTMod', 'CPTMod2', 'BETOS',
                       'RevCode', 'LinePaid', 'Paid', 'LineDx', 'Dx1', 'Dx2', 'ClinicalTrialNum', 'EpiNum', 'NDC']]
        dfop = dfop[['BeneSK', 'FromDate', 'ThruDate', 'NPI', 'TaxID', 'Specialty', 'ServicePlace_lbl', 'Services',
                     'ServiceType', 'LineFromDate', 'LineThruDate', 'CPT', 'CPTMod', 'CPTMod2', 'BETOS',
                     'RevCode', 'LinePaid', 'Paid', 'LineDx', 'Dx1', 'Dx2', 'ClinicalTrialNum', 'EpiNum', 'NDC']]
        dfdme = dfdme[['BeneSK', 'FromDate', 'ThruDate', 'NPI', 'TaxID', 'Specialty', 'ServicePlace_lbl', 'Services',
                       'ServiceType', 'LineFromDate', 'LineThruDate', 'CPT', 'CPTMod', 'CPTMod2', 'BETOS',
                       'RevCode', 'LinePaid', 'Paid', 'LineDx', 'Dx1', 'Dx2', 'ClinicalTrialNum', 'EpiNum', 'NDC']]
        dfPartB = pd.concat([dfphy, dfop, dfdme])
        dfPartB['TaxID'] = dfPartB.TaxID.apply(lambda x: str(x))
        dfe = dfepi[['EpiNum', 'PatientName', 'Sex', 'Age', 'DeathDate', 'EpiStart', 'EpiEnd', 'CancerType',
                     'RadiationFlag', 'HCCCount', 'PartDChemo', 'PartBTOSPaidImaging', 'PartBTOSPaidLab',
                     'AttributedPhysicianName', 'ReconciliationEligible']]
        dfPartB = pd.merge(dfPartB, dfe, on='EpiNum', how='left')
        dfPartB = dfPartB[dfPartB.ReconciliationEligible>=1]
        dfPartB = addCCSProc(dfPartB)
        Save(dfPartB, Working + '/dfPartB')
    return dfPartB


def addCCSProc(df):
    xw = Use('c:/AdvAnalytics/Reference/xw_CPT2CCSProc')
    xw.set_index('CPT', inplace=True)
    dictCCS = xw['CCSProc'].to_dict()
    df['CCSProc'] = df.CPT.map(dictCCS)
    return df


def writeRadiologyFile(df):
    df = df[df.CPT.between('7', '8')]
    df = df[~df.BETOS.between('P', 'P9Z')]
    xw = Use('c:/AdvAnalytics/Reference/ref_BETOS')
    df = pd.merge(df, xw, on='BETOS', how='left')
    xw = Use('c:/AdvAnalytics/Reference/code_CCSProc')
    df = pd.merge(df, xw, on='CCSProc', how='left')
    xw = Use('c:/AdvAnalytics/Reference/code_CPT')
    df = pd.merge(df, xw, on='CPT', how='left')
    dfG = df.groupby('EpiNum')
    dfA = dfG.agg({'LinePaid' : {'EpisodeImagingClaims': 'count',
                                 'EpisodeImagingPaid': 'sum'}})
    dfA = postAgg(dfA)
    df = pd.merge(df, dfA, on='EpiNum')
    xw = Use('c:/AdvAnalytics/Reference/code_HCFASpecialty')
    df = pd.merge(df, xw, left_on='Specialty', right_on='HCFASpecialty', how='left')
    for c in ['Level1Group', 'Level2Group', 'Level3Group', 'CCSProc', 'CPTMod2', 'Specialty', 'HCFASpecialty']:
        del df[c]
    for c in ['Dx2', 'Dx1']:
        del df[c]
    for c in df.columns.tolist():
        if df[c].dtype == 'object':
            df[c].fillna('#', inplace=True)
        elif df[c].dtype == 'datetime64[ns]':
            df[c].fillna(pd.to_datetime('1/1/1970'), inplace=True)
        else:
            df[c].fillna(-99, inplace=True)
    Save(df, Output + '/dfImagingForPowerBI')


def readCoefficients():
    df = pd.read_excel('c:/AdvAnalytics/OCM/Reference/Input/Model_Coefficients_Revised.xlsx',
                       sheetname='ParameterEstimates', skiprows=3, header=None,
                       names=['Variable', 'df', 'Estimate', 'StdError', 'AssocProb', 'Impact'])
    df = df[['Variable', 'Impact']]
    df = df[df.Variable != 'Intercept']
    df['BasePrice'] = np.where(df.Impact > 100, 'Y', 'N')
    df['AgeSexVariable'] = np.where(df.Variable.str.contains('ale'), "Yes", "No")
    Save(df, 'c:/AdvAnalytics/OCM/Reference/ref_pricingCoefficients')
    return df


def enhancedfepi(dfepi, dfop):
    '''
    This function adds a couple of columns to dfepi that are used in the reports.  Those columns are simple computations.
    :param dfepi: the episode summary dataframe
    :param dfop: the outpatient claim line level dataframe
    :return: an enhanced dfepi
    '''
    dfop = dfop[(dfop.RevCode.between('0450', '0459')) | (dfop.RevCode=='0981')]
    dfG = dfop.groupby('EpiNum')
    dfA = dfG.agg({'RevCodeDate': {'ERFirstVisit': 'min',
                                   'ERLastVisit': 'max',
                                   'ERVisits': 'count'}})
    dfA = postAgg(dfA)
    dfepi = pd.merge(dfepi, dfA, how='left', on='EpiNum')
    dfepi.ERVisits.fillna(0, inplace=True)
    dfepi.ERFirstVisit.fillna(pd.to_datetime('1/1/1970'), inplace=True)
    dfepi.ERLastVisit.fillna(pd.to_datetime('1/1/1970'), inplace=True)
    dfepi['ERVisitFlag'] = np.sign(dfepi.ERVisits)
    return dfepi


def addRegimen(dfdrugs, dfepi):
    '''
    This function adds a unique regimen ID to each episode.

    The logic is as follows:
        1) Prep the drugsummary table (1 row per drug per episode) as follows:
           a) Keep only drugs that are first use during 4 weeks of the episode
        2) Open the xw_CPT2Regimen table
        3) Build a regimen overview table that has 2 fields: the regimen ID and the number of drugs in the regimen
        4) Join the drug summary to the xw table described above
        5) Create and EpisodeRegimenSummary table that has 4 fields:
           a) EpiNum
           b) RegimenID
           c) UniqueDrugCount
           d) Total Spend
        6) Join EpisodeDrugSummary with info_RegimenDrugCount.  That will limit the list to the regimens where all of
           the drugs are present
        7) For each episode, pick a regimen.
           a) if there are multiple regimens, start by dropping any "UNKNOWN" regimen.
           b) Next, pick the regimen(s) with the most unique substances.  This captures cases where there are both
              a "K" drug regimen and a separate regimen that adds a "K+1"th drug.
           c) In the event of a tie, go with the one that has the more expensive drugs.
    :param dfdrugs: the drug summary dataframe
    :param dfepi: the episode summary dataframe
    :return: dfepi with the regimen information added
    '''
    dfdrugs = dfdrugs[dfdrugs.DaysFromEpiStart<=28]
    dfdrugs['temp'] = dfdrugs.DrugName.str.split('[')
    dfdrugs['temp'] = dfdrugs.temp.apply(lambda x: x[1])
    dfdrugs['temp'] = dfdrugs.temp.apply(lambda x: x.replace(']', ''))
    dfdrugs['temp'] = dfdrugs.temp.apply(lambda x: x[:5].upper())
    dfdrugs.loc[dfdrugs.CPT=='J9999', 'CPT'] = dfdrugs.temp
    del dfdrugs['temp']
    xw = Use('/AdvAnalytics/OCM/Reference/xw_CPT2Regimen')
    dfrc = Use('/AdvAnalytics/OCM/Reference/info_RegimenDrugCount')
    df = pd.merge(dfdrugs, xw, on='CPT', how='inner')
    dfG = df.groupby(['EpiNum', 'Regimen', 'RegimenMedicationOrder'])
    dfA = dfG.agg({'TotalPaid': {'RegimenPaid': 'sum',
                                'RegimenMeds': 'count'}})
    dfA = postAgg(dfA)
    dfA = pd.merge(dfA, dfrc, on='Regimen', how='left')
    for c in ['DrugCount', 'RegimenMeds']:
        dfA[c].fillna(0,inplace=True)
    dfA['MedCountDiff'] = dfA.DrugCount - dfA.RegimenMeds
    dfA.sort_values(['EpiNum', 'MedCountDiff', 'DrugCount', 'RegimenPaid'],
        ascending=[1, 1, 0, 0], inplace=True)
    dfA = dfA.groupby('EpiNum').first().reset_index()
    dfepi = pd.merge(dfepi, dfA, on='EpiNum', how='left')
    return dfepi

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
    df['HasClinicalTrial'] = np.where(df.ClinicalTrialFlag>0.5, 1., 0.)
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
    df['SurgeryFlag'] = df.SurgeryFlag.astype(np.float32)
    df['ClinicalTrialFlag'] = df.ClinicalTrialFlag.astype(np.float32)
    df['CleanPeriodRecentHistory'] = np.where(df.CleanPeriod == 1, 1., 0.)
    df['CleanPeriodOlderHistory'] = np.where(df.CleanPeriod == 2, 1., 0.)
    df['CleanPeriodNoHistory'] = np.where(df.CleanPeriod == 2, 1., 0.)
    df['HospicePassOCM3'] = np.where((df.HospiceLOS>=3) & (df.DiedDuringEpisode>0.5), 1., 0.)
    listCols = ['Female', 'Age', 'DiedDuringEpisode', 'DualEligible', 'HasPartD', 'HasLIS',
                'HasClinicalTrial', 'HospicePassOCM3', 'ERVisitFlag', 'ERVisits',
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
    # Adjust Hospice Compliance Benchmark so that the denominator of the measure is the number of patients who died.
    # This works because the denominators of the two measures cancel each other out, leaving the measure as
    # sum(HospicePassOCM3)/count(Deaths)
    dfA['HospicePassOCM3'] = dfA.HospicePassOCM3 / dfA.DiedDuringEpisode
    Save(dfA, Working + '/dfepiBenchmarks')
    for c in dfA.columns.tolist():
        if c != 'CancerTypeDetailed':
            dfA.rename(columns={c: c + 'Benchmark'}, inplace=True)
    df = pd.merge(df, dfA, on='CancerTypeDetailed')
    Save(df, Working + '/dfepi')
    return df


def prepDrugTable(df):
    '''
    This function creates a crosswalk from the first 9 characters of the NDC to information about the drug class and
    the drug name.  It creates a drug name field that concatenations the generic name with the most commonly used brand
    name in parentheses.
    :param df: drugs dataframe
    :return: xwalk table from NDC9 to the classes and specific drug names.
    '''
    refNDC = Use('c:/AdvAnalytics/Reference/ref_NDC.feather')
    refGPI = Use('c:/AdvAnalytics/Reference/ref_GPI.feather')
    refNDC = refNDC[['NDC', 'BrandNameDrug', 'Drug_Name', 'GPI']]
    refGPI = refGPI[['GPI', 'TherapeuticClassLevel1', 'TherapeuticClassLevel2', 'TherapeuticClassLevel3', 'TherapeuticClassLevel4', 'DrugName', 'DrugNameDetailed']]
    refNDC = pd.merge(refNDC, refGPI, on='GPI', how='left')
    refNDC['NDC9'] = refNDC.NDC.apply(lambda x: x[:9])
    refNDC = refNDC.groupby('NDC9').last().reset_index()
    xw = refNDC[['NDC9', 'TherapeuticClassLevel4', 'BrandNameDrug', 'Drug_Name', 'DrugName']]
    df['NDC9'] = df.NDC.apply(lambda x: x[:9])
    df = df[['EpiNum', 'NDC9']]
    df = pd.merge(df, xw, on='NDC9', how='left')
    del df['EpiNum']
    gv = ['TherapeuticClassLevel4', 'DrugName', 'BrandNameDrug', 'Drug_Name']
    dfA = df.groupby(gv).count()
    dfA.reset_index(inplace=True)
    dfA.sort_values(['TherapeuticClassLevel4', 'DrugName', 'BrandNameDrug'],
                    ascending=[1, 1, 0], inplace=True)
    dfA['DrugNameFull'] = dfA.DrugName + ' [' + dfA.Drug_Name + ']'
    dfA.groupby(['TherapeuticClassLevel4', 'DrugName']).first().reset_index()
    del dfA['Drug_Name']
    del dfA['NDC9']
    refNDC = pd.merge(refNDC, dfA, on=['TherapeuticClassLevel4', 'DrugName'], how='left')
    refNDC['DrugName'] = refNDC.DrugNameFull
    del refNDC['DrugNameFull']
    del refNDC['NDC']
    return refNDC


def dfdrugsBuild(dfop, dfphy, dfdme, dfPartD, dfepi):
    # Should add drug costs and volumes
    dfepi = dfepi[['DeathDate', 'ZipCode', 'EpiNum', 'EpiStart', 'EpiEnd',
                   'CancerType', 'BaselinePrice', 'WinsorizedCost', 'AttributedPhysicianName']]
    dfPartD = Use('Input/dfPartD.feather')
    xw = Use('c:/AdvAnalytics/Reference/xw_HCPCS2NDC.feather')
    xw.rename(columns={'HCPCS': 'CPT'}, inplace=True)
    dfdme = dfdme[dfdme.NDC.notnull()]
    dfphy = pd.merge(dfphy, xw, on='CPT')
    try:
        del dfop['NDC']
    except:
        pass
    dfop = pd.merge(dfop, xw, on='CPT')
    dfop.rename(columns={'RevCodeDate': 'LineFromDate'}, inplace=True)
    dfop = dfop[['BeneSK', 'EpiNum', 'NDC', 'LineFromDate', 'LinePaid', 'GPI10']]
    dfphy = dfphy[['BeneSK', 'EpiNum', 'NDC', 'LineFromDate', 'LinePaid', 'GPI10']]
    dfdme = dfdme[['BeneSK', 'EpiNum', 'NDC', 'LineFromDate', 'LinePaid', 'GPI10']]
    dfPartD = dfPartD[['BeneSK', 'EpiNum', 'NDC', 'ServiceDate', 'Paid', 'DaysSupply', 'GPI10']]
    dfPartD.rename(columns={'Paid': 'LinePaid',
                            'ServiceDate': 'LineFromDate'}, inplace=True)
    dfop['ClaimType'] = 'OP'
    dfphy['ClaimType'] = 'Office'
    dfdme['ClaimType'] = 'DME'
    dfPartD['ClaimType'] = 'Pharm'
    for d in ['dfop', 'dfphy', 'dfdme']:
        CMD = d + "['DaysSupply'] = 1"
        exec(CMD)
    # Append files together
    dfdrugs = pd.concat([dfop, dfphy, dfdme, dfPartD])
    refNDC = prepDrugTable(dfdrugs)
    dfdrugs['NDC9'] = dfdrugs.NDC.apply(lambda x: x[:9])
    try:
        del dfdrugs['BrandNameDrug']
    except:
        print('BrandNameDrug not found')
    xw = Use('/AdvAnalytics/Reference/xw_NDC2HCPCS.feather')
    xw['NDC9'] = xw.NDC.apply(lambda x: x[:9])
    del xw['NDC']
    xw.drop_duplicates(inplace=True)
    xw.rename(columns={'HCPCS': 'CPT'}, inplace=True)
    refNDC = pd.merge(refNDC, xw, on='NDC9', how='left')
    dfdrugs = pd.merge(dfdrugs, refNDC, how='left', on='NDC9')
    dfdrugs.CPT.fillna('J9999', inplace=True)
    del dfdrugs['NDC9']
    dfdrugs['MailOrderDrug'] = np.where(dfdrugs.DaysSupply > 35, 1., 0.)
    dfdrugs = pd.merge(dfdrugs, dfepi, on='EpiNum')
    #Save(dfdrugs, Working + '/dfdrugs')
    try:
        toMySQL(dfdrugs, 'ocm', 'dfdrugsDetail')
    except:
        pass
    dfG = dfdrugs.groupby(['ClaimType', 'EpiNum', 'TherapeuticClassLevel1',
                           'TherapeuticClassLevel2', 'TherapeuticClassLevel3',
                           'TherapeuticClassLevel4', 'DrugName', 'CPT'])
    dfA = dfG.agg({'LineFromDate': {'FirstClaimDate': 'min',
                                    'LastClaimDate': 'max'},
                   'LinePaid': {'TotalPaid': 'sum',
                                'MeanClaimPaid': 'mean'},
                   'DaysSupply': {'TotalDaysSupply': 'sum'},
                   'MailOrderDrug': {'Claims': 'count',
                                     'MailOrderClaims': 'sum'}})
    dfA = postAgg(dfA)
    dfA = pd.merge(dfepi, dfA, on='EpiNum')
    dfA['DaysFromEpiStart'] = (dfA.FirstClaimDate - dfA.EpiStart) / np.timedelta64(1, 'D')
    dfA['DaysBeforeEpiEnd'] = (dfA.EpiEnd - dfA.LastClaimDate) / np.timedelta64(1, 'D')
    dfA['DaysBeforeDeath'] = (dfA.DeathDate - dfA.LastClaimDate) / np.timedelta64(1, 'D')
    dfA['PBP'] = dfA.BaselinePrice - dfA.WinsorizedCost
    df = dfA[['EpiNum', 'DaysBeforeEpiEnd', 'DaysFromEpiStart', 'DaysBeforeDeath']]
    dfG = dfA.groupby('EpiNum')
    df1 = dfG.agg({'DaysBeforeEpiEnd': {'LastDrugDaysBeforeEpiEnd': 'min'},
                   'DaysFromEpiStart': {'FirstDrugDaysAfterEpiStart': 'min'},
                   'DaysBeforeDeath' : {'LastDrugDaysBeforeDeath' : 'min'}})
    df1 = postAgg(df1)
    df1.LastDrugDaysBeforeDeath.fillna(-99, inplace=True)
    Save(df1, Output + '/dfDrugTiming')
    #toMySQL(df1, 'ocm', 'dfDrugTiming')
    Save(dfA, 'Working/dfDrugSummary')
    return dfA


def build_dfie(dfop, dfip, dfiprev, dfepi):
    '''
    This function builds a dataframe with one record for each emergency visit along with the discharge disposition
    (admitted, sent home, transferred and admitted) for each visit.
    :param dfop: the outpat claims dataframe
    :param dfip: the inpat claims dataframe
    :param dfiprev: the inpat line dataframe
    :param dfepi: the episode summary dataframe
    :return: dfie--a dataframe with one record for each ER visit along with disposition, diagnosis, facility, cost,
                   and other information.
    '''
    dfiprev = Use('Input/dfiprev.feather')
    dfip = Use('Input/dfip.feather')
    dfop = Use('Input/dfop.feather')
    listCols = ['BeneSK', 'FromDate', 'ThruDate', 'CCN', 'AttendingNPI', 'Dx1', 'EpiNum',
                'RevCode', 'RevCodeDate', 'CPT', 'LinePatientPaid', 'LinePaid']
    dfop = dfop[listCols]
    dfepi = Use('Working/dfepi.feather')

    dfiprev = dfiprev[dfiprev.RevCode.between('0450', '0459')]
    dfiprev['RevCodeDate'] = dfiprev.FromDate
    dfiprev = dfiprev[['RevCode', 'RevCodeDate', 'CPT', 'EpiNum', 'CCN', 'Paid', 'AttendingNPI',
                       'AdmitDate', 'Deductible', 'LOS', 'DischargeDate', 'Dx1']]
    dfiprev['ClaimType'] = 'Admitted'
    # Filter the OP dataframe for records with ER visit codes
    dfop = dfop[dfop.RevCode.between('0450', '0459')]
    # Find ER visits that are transferred
    dfip = dfip[['EpiNum', 'BeneSK', 'AdmitDate', 'DischargeDate', 'Paid', 'LOS', 'Deductible']]
    dfip['ClaimType'] = 'Transfer'
    dfip.drop_duplicates(inplace=True)
    dfop2 = pd.merge(dfop, dfip, on=['EpiNum', 'BeneSK'])
    dfop2 = dfop2[dfop2.RevCodeDate == dfop2.AdmitDate]
    dfop2 = dfop2[['BeneSK', 'EpiNum', 'AdmitDate', 'DischargeDate', 'Paid', 'LOS', 'Deductible', 'ClaimType']]
    dfop = pd.merge(dfop, dfop2, on=['EpiNum', 'BeneSK'], how='left')
    # Now clean up the fields that might not be in OP
    dfop['AdmitDate'].fillna(dfop.ThruDate, inplace=True)
    dfop['DischargeDate'].fillna(dfop.ThruDate, inplace=True)
    dfop['Deductible'].fillna(0, inplace=True)
    dfop['Deductible'] += dfop.LinePatientPaid
    dfop['LOS'].fillna(0, inplace=True)
    dfop.ClaimType.fillna('Sent Home', inplace=True)
    # Get a common list of filed with dfiprev then append the dataframes
    dfop = dfop[['RevCode', 'RevCodeDate', 'CPT', 'EpiNum', 'CCN', 'Paid', 'AttendingNPI',
                 'AdmitDate', 'Deductible', 'LOS', 'DischargeDate', 'Dx1', 'ClaimType']]
    dfie = pd.concat([dfiprev, dfop])
    # Now add code information to the file
    xw = Use('/AdvAnalytics/Reference/xw_Dx2CCS.feather')
    xw.columns = ['Dx1', 'CCS']
    ref = Use('/AdvAnalytics/Reference/code_CCS.feather')
    ref.drop_duplicates(inplace=True)
    xw = pd.merge(xw, ref, on='CCS')
    xw.drop_duplicates(subset='Dx1', inplace=True)
    dfie = pd.merge(dfie, xw, on='Dx1', how='left')
    ref = Use('/AdvAnalytics/Reference/ref_ProviderOfService.feather')
    ref = ref[['CCN', 'CCN_lbl', 'State', 'CBSA', 'Beds', 'ChemoServiceCode']]
    dfie = pd.merge(dfie, ref, on='CCN', how='left')
    listCols = ['Age', 'DeathDate', 'EpiNum', 'EpiStart', 'EpiEnd', 'CancerType',
                'ClinicalTrialFlag', 'WinsorizedCost', 'ActualCost', 'BaselinePrice',
                'PatientName', 'IPAdmits', 'AttributedNPI', 'AttributedPhysicianName',
                'Female', 'DiedDuringEpisode', 'DualEligible', 'CancerTypeDetailed']
    dfepi = dfepi[listCols]
    dfie = pd.merge(dfie, dfepi, on='EpiNum')
    Save(dfie, Output + '/dfie')
    toMySQL(dfie, 'ocm', 'dfie')
    return dfie


##################
# Execution area #
##################
createDirectories()
'''
dfepi = readEpi()
Save(dfepi, InputFeather + '/dfepi')

dfdme = readDME()
dfdmeDrugs = dfdme[dfdme.NDC.notnull()]
dfdmeDrugs.reset_index(inplace=True)
del dfdmeDrugs['index']
Save(dfdmeDrugs, InputFeather + '/dfdmeDrugs')
print('Number of rows in DME drug dataframe after reading: ' + str(len(dfdmeDrugs.index)))
print('Number of rows in DME file after reading: ' + str(len(dfdme.index)))

dfhha = readHHA()
print('Number of rows in HHA file after reading: ' + str(len(dfhha.index)))
Save(dfhha, InputFeather + '/dfhha')
print()

dfhs = readHospice()
print('Number of rows in Hospice file after reading: ' + str(len(dfhs.index)))
Save(dfhs, InputFeather + '/dfhs')
print()

dfip, dfiprev, dficu = readIP()
print('Number of rows in IP header file after reading: ' + str(len(dfip.index)))
print('Number of rows in IP revenue file after reading: ' + str(len(dfiprev.index)))
Save(dfip, InputFeather + '/dfip')
Save(dfiprev, InputFeather + '/dfiprev')
print()

dfsnf = readSNF()
print('Number of rows in SNF header file after reading: ' + str(len(dfsnf.index)))
Save(dfsnf, InputFeather + '/dfsnf')
print()

dfPartD = readRx()
print('Number of rows in Part D file after reading: ' + str(len(dfPartD.index)))
print()

dfophead, dfopline, dfop = readOP()
print('Number of rows in OP header file after reading: ' + str(len(dfophead.index)))
print('Number of rows in OP line file after reading: ' + str(len(dfopline.index)))
print('Number of rows in OP combined file after reading: ' + str(len(dfop.index)))
print()
'''

dfphyhead, dfphyline, dfphy = readphy()
print('Number of rows in phy header file after reading: ' + str(len(dfphyhead.index)))
print('Number of rows in phy line file after reading: ' + str(len(dfphyline.index)))
print('Number of rows in phy combined file after reading: ' + str(len(dfphy.index)))
print()

dfepi = addTOSCostToEpi(dfepi, dfphy, dfop, dfip, dfsnf, dfhha, dfhs, dfPartD, dfdme)
#print(dfepi.columns.tolist())

ti=getTaxID(dfphyline)
dfepi = physicianAttribution(dfphy, ti, dfepi)
dfepi = enhancedfepi(dfepi, dfop)
dfepi = addBenchmarks2dfepi(dfepi)
dfepi = addICU2Epi(dficu, dfepi)
toMySQL(dfepi, 'ocm', 'dfepi')
#diedOutsideEpisode(dfepi)

build_dfie(dfop, dfip, dfiprev, dfepi)

dfPartB = combinePartB(dfop, dfphy, dfdme, dfepi)

# Write radiology claims files
writeRadiologyFile(dfPartB)
dfdrugs = dfdrugsBuild(dfop, dfphy, dfdme, dfPartD, dfepi)
Save(dfdrugs, Output + '/dfdrugs')

# Write files for Power BI
ip2PowerBI(dfip, dfepi)
dfcoef = readCoefficients()