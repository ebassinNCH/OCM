client = 'CCSI'

## app.R ##
## Import Libraries ----
# options(warn=-1)
# options(shiny.reactlog=TRUE)
# options(shiny.error = recover)
# options(error=recover)
# options(show.error.locations=TRUE)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(feather)
library(plotly)
library(googleVis)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(sp)
library(leaflet)
library(rlang)
library(rpivotTable)
library(reshape2)

#### Read Data files and prep data ####
wd = paste0('/AdvAnalytics/Aggregate/OCM/', client, '/Quarterly')
basewd = paste0('/AdvAnalytics/OCM/', client, '/BaselineUpdated/')
setwd(wd)
refCCN <- read_feather('/AdvAnalytics/Reference/code_CCN.feather')
dfpriceCoef <- read_feather('/AdvAnalytics/OCM/Reference/ref_pricingCoefficients.feather')
#dfhcc <- read_feather('Output/dfhcc.feather')
code_hcc <- read_feather('/AdvAnalytics/Reference/code_HCC.feather')
codeCounty <- read_feather('/AdvAnalytics/Reference/code_FIPSCounty.feather')
xwZip2County <- read_feather('/AdvAnalytics/Reference/xw_ZipCode2County.feather')
codeCounty <- read_feather('/AdvAnalytics/Reference/code_FIPSCounty.feather')


#### Set fonts ####
fontTitle <- list(family='Droid Serif',
                  size=18,
                  color='#878787')
font1 <- list(family = 'Arial',
              size=15,
              color = '#878787')
font2 <- list(family='Arial', 
              size=12,
              color='#878787')
fontNarrow <- list(family='PT Sans Narrow',
                   size=11, 
                   color='#878787')
legHorizontal <- list(font=list(family='PT Sans Narrow',
                                size=10,
                                color='#878787'),
                      bgcolor='#ffffff',
                      orientation='h',
                      x=0, y=0)
#### Functions ####
HBarCount <- function(df, grp, color='#7ac043', xtitle='') {
  names(df)[names(df)==grp] <- 'gV'
  df <- df %>% group_by(gV) %>% 
    summarize(Count = n()) %>% 
    arrange(desc(Count))
  df$RowNum = seq(1:nrow(df))
  barChart <- plot_ly(df, x=~Count, y=~gV, orientation='h', type='bar', opacity=0.7, 
                      marker=list(color=color), text=~paste0(Count), textposition='auto',
                      insidetextfont=list(size=13, family='Balto', color='white')) %>% 
    layout(
      title='',
      xaxis=list(title=xtitle, 
                 showticklabels=TRUE, 
                 titlefont=font1, 
                 tickfont=font2,
                 separatethousands=TRUE), 
      yaxis=list(title='',
                 autorange='reversed',
                 categoryarray=~Count, 
                 categoryorder='array',
                 showgrid=FALSE, 
                 showticklabels=TRUE, 
                 tickfont = fontNarrow,
                 ticklen=0,
                 zeroline=FALSE,
                 showline=FALSE),
      margin=list(l=170)
    )
  return(barChart)
}

OutputTable <- function(df, headerCols) {
  # Like a Python dict, this is a list with mappings of R column names to friendly names.  
  #    If a column name, appears in the list, we use the friendly name.
  #    If the column name does not appear in the list, the column names is used.  
  #    If the list contains a column name that is not in the data frame, that list element is 
  #        unused, so extra list elements are ignored. ####
  mapCols <- list('EpiNum' = 'Episode #', 
                  'YV' = 'Your Variable',
                  'PatientName'='Patient Name',
                  'Actual Cost'='$Actual',
                  'MostExpensiveDrug'='Most Expensive Agent',
                  'CancerTypeDetailed'='Cancer Type',
                  'AttributedPhysicianName'='Attributed Provider',
                  'EpiStart'='Episode Start',
                  'IPTotalPaid'='$Inpat',
                  'TotalIPPaid'='$Inpat Total',
                  'MeanIPPaid'='$Inpat Per Episode',
                  'PaidPerEpisode'='Mean Episode Cost',
                  'BenchmarkPaid'='$Benchmark',
                  'DrugPaid'='$Drugs',
                  'DrugsPaid'='$Drugs',
                  'DrugSavings'='Drugs $Savings Per Episode',
                  'MeanDrugPaid'='$Drugs Per Episode',
                  'MeanSavings'='Savings Per Episode',
                  'InpatPaidPerEpisode'='$Inpat Per Episode',
                  'InpatPaidPerEpisBenchmark'='$Inpat Per Epis Benchmark',
                  'PartBTOSPaidImaging'='$Imaging',
                  'BaselinePrice'='$OCM Target',
                  'WinsorizedCost'='$Winsorized',
                  'MeanActualCost'='$Mean Episode Cost',
                  'ClinicalTrialStatus'='Trial ID Method',
                  'PctHome'='% Sent Home',
                  'PctAdmit'='% Admitted',
                  'PctTrans'='% Transferred',
                  'ERVisits'='# of ER Visits',
                  'RevCodeDate'='Date of Service',
                  'CCS_lbl'='Diag Group', 
                  'CCN_lbl'='Facility Name',
                  'ClaimType'='Discharged To',
                  'MeanERPaid'='$Mean Paid',
                  'Regimen_lbl'='Drug Regimen',
                  'MeanEpisodeCost'='$Cost Per Episode',
                  'SDEpisodeCost'='Std Dev Cost Per Episode',
                  'MeanCostSavings'='$Savings Per Episode',
                  'AdmitsPerEpisode'='Admits Per Episode',
                  'ERVisitsPerEpisode'='ER Visits Per Episode',
                  'EmergencyAdmits'='# of Emerg Admits',
                  'NonEmergAdmits'='# of Non-Emerg Admits',
                  'EmergencyPaid'='$Paid per Emerg Admit',
                  'NonEmergPaid'='$Paid per Non-Emerg Admit',
                  'AdmitLast14Days' = 'Admitted in Last 14 Days',
                  'ICULast14Days'='ICU During Last 14 Days',
                  'OCM3Pass'='3+ Days of Hospice',
                  'HospiceLOS'='Hospice LOS',
                  'HospiceDaysGroup'='Length of Stay',
                  'PctPatients'='% of Patients',
                  'MeanHospicePaid'='$Hospice Per Patient',
                  'MeanHospicePaidBenchmark'='Benchmark',
                  'MeanEpisodeCostBenchmark'='Benchmark')
  # Specify the "row headers"
  headerColumns = headerCols
  # Specify the title you want on the table
  tableTitle=''
  # This is the code to create a vector of column names/headers.
  columnNames=c()
  # Put friendly names on columns
  for (var in names(df)){
    myResult = mapCols[[var]]
    if (is.null(myResult)) {
      columnNames <- append(columnNames, var)
    } else {
      columnNames <- append(columnNames, myResult)
    }
  }
  # Creates the basic table
  wrkTable <- datatable(df, class='hover', rownames=FALSE, colnames=columnNames,
                        caption=tableTitle)
  # This section does column formatting.  As in fewSpreadsheets, I use strings in the column name to determine format. ####
  for (var in names(df)) {
    var2 <- tolower(var)
    if ( (grepl('paid',var2)) | (grepl('cost',var2)) | (grepl('price',var2)) | (grepl('savings',var2))) {
      wrkTable <- formatCurrency(wrkTable, columns=c(var), currency='$', digits=0)
    } 
    if ( (grepl('date',var2)) | (var2=='epistart') | (var2=='epiend') ) {
      wrkTable <- formatDate(wrkTable, columns=c(var), method='toDateString')
    }  
    if ( (grepl('pct', var2)) | (grepl('percent', var2)) | (grepl('%', var2)) ) {
      wrkTable <- formatPercentage(wrkTable, columns=c(var), digits=1)
    } 
    if ( (grepl('sperepisode', var2))  ) {
      if ( (grepl('ervis', var2)) | grepl('admits', var2) ) {
        wrkTable <- formatRound(wrkTable, columns=c(var), digits=3)
      }
      else {
        wrkTable <- formatRound(wrkTable, columns=c(var), digits=1)
      }
    } 
  }
  # Format the row headers
  for (var in headerCols) {
    wrkTable <- formatStyle(wrkTable, c(var), backgroundColor='#f9f9f9', fontWeight='bold')
  }
  
  return(wrkTable)
}

zipMap <- function(dfw, dfName, Var, varlabel, digits=0, palette='RdYlGn') {
  MyCmd <- paste0("pal <- colorNumeric(palette='", palette, "', domain=", dfName, "$", Var, ")")
  eval(parse(text=MyCmd))
  #pal<- colorNumeric(palette='RdYlGn', domain=dfw$MeanSavings)
  Popup<-"popup=~paste0('<H4><center>Zip Code: ', GEOID10, '</center></H4>', '</b>Episodes: <b>', Episodes,"
  Popup<-paste0(Popup, " '</b><BR>Admits Per Episode: <b>', round(AdmitsPerEpisode,1),")
  Popup<-paste0(Popup, " '</b><BR>ER Visits Per Episode: <b>', round(ERVisitsPerEpisode,1),")
  Popup<-paste0(Popup, " '</b><BR>Savings Per Episode: <b>$', format(round(MeanSavings,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Drug Cost Per Episode: <b>$', format(round(DrugsPaid,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Drug Savings/Losses Per Episode: <b>$', format(round(DrugSavings,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Inpat Cost Per Episode: <b>$', format(round(InpatPaid,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Inpat Savings/Loss Per Episode: <b>$', format(round(InpatSavings,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Lab/Imaging Cost Per Episode: <b>$', format(round(TestingPaid,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Lab/Imaging Savings/Loss Per Episode: <b>$', format(round(TestingSavings,0), big.mark=',', trim=TRUE)")
  MyCmd <- paste0("myMap <- leaflet(data=", dfName, ") %>% ",
                  "addProviderTiles(providers$OpenMapSurfer.AdminBounds) %>% ", 
                  "addPolygons(fillColor=~pal(", Var, "), ",
                  "weight=1, ",
                  "opacity=0, ",
                  "fillOpacity=.6, ", Popup, "))")
  eval(parse(text=MyCmd))
  return(myMap)
}

countyMap <- function(dfwc, dfName, Var, varlabel, digits=0, palette='RdYlGn') {
  MyCmd <- paste0("pal <- colorNumeric(palette='", palette, "', domain=", dfName, "$", Var, ")")
  eval(parse(text=MyCmd))
  print(MyCmd)
  Popup<-"popup=~paste0('<H4><center>', NAMELSAD10, '</center></H4>', '</b>Episodes: <b>', Episodes,"
  Popup<-paste0(Popup, " '</b><BR>Admits Per Episode: <b>', round(AdmitsPerEpisode,1),")
  Popup<-paste0(Popup, " '</b><BR>ER Visits Per Episode: <b>', round(ERVisitsPerEpisode,1),")
  Popup<-paste0(Popup, " '</b><BR>Savings Per Episode: <b>$', format(round(MeanSavings,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Drug Cost Per Episode: <b>$', format(round(DrugsPaid,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Drug Savings/Losses Per Episode: <b>$', format(round(DrugSavings,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Inpat Cost Per Episode: <b>$', format(round(InpatPaid,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Inpat Savings/Loss Per Episode: <b>$', format(round(InpatSavings,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Lab/Imaging Cost Per Episode: <b>$', format(round(TestingPaid,0), big.mark=',', trim=TRUE),")
  Popup<-paste0(Popup, " '</b><BR>Lab/Imaging Savings/Loss Per Episode: <b>$', format(round(TestingSavings,0), big.mark=',', trim=TRUE))")
  MyCmd <- paste0("myMap <- leaflet(data=", dfName, ") %>% ",
                  " addProviderTiles(providers$OpenMapSurfer.AdminBounds)  %>%", 
                  " addPolygons(fillColor=~pal(", Var, "), ",
                  "weight=1, ",
                  "opacity=0, ",
                  "fillOpacity=.6, ", Popup, ")")
  print(MyCmd)
  eval(parse(text=MyCmd))
  return(myMap)
}

aggQuality <- function(df, dfB, var, grp) {
  names(df)[names(df) == grp] <- 'gV'
  names(df)[names(df) == var] <- 'sV'
  names(dfB)[names(dfB) == grp] <- 'gV'
  names(dfB)[names(dfB) == var] <- 'sV'
  dfb <- df  %>%  
    group_by(CancerTypeDetailed) %>%
    summarise( Measurebenchmark=mean(sV))
  df <- merge(df, dfb, by='CancerTypeDetailed', all.x=TRUE)
  df <- mutate(df, Measurebenchmark = ifelse(is.na(Measurebenchmark), 0, Measurebenchmark))
  dfb <- df %>%  group_by(gV) %>% 
    summarise(
      YourScore = mean(sV),
      Benchmark = mean(Measurebenchmark),
      Episodes=n())
  dfB = dfB %>% group_by(gV) %>% 
    summarise(
      Baseline = mean(sV),
      BaselineEpisodes=n())
  dfb = merge(dfb, dfB, by='gV', all.x=TRUE)
  dfb = dfb %>% 
    mutate(Baseline = ifelse(is.na(Baseline), 0, Baseline),
           BaselineEpisodes = ifelse(is.na(BaselineEpisodes), 0, BaselineEpisodes))
  if ( (max(dfb$YourScore)<=1)  & (min(dfb$YourScore)>=0) ) {
    dfb <- mutate(dfb, 
                  YourScore = YourScore * 100,
                  Benchmark = Benchmark * 100,
                  Baseline = Baseline * 100)
  }
  names(dfb)[names(dfb)=='YourScore'] <- var
  names(dfb)[names(dfb)=='gV'] <- grp
  return(dfb)
}

barVsBenchmark <- function(df, var, grp, color='#7ac043', xlabel='') {
  df <- arrange(df, desc(Episodes))
  myfnchart <- plot_ly(df, y=eval(substitute(~y, list(y=as.name(grp)))), height='500px') %>% 
    add_trace(x=eval(substitute(~x, list(x=as.name(var)))), 
              name='Actual', type='bar', marker=list(color=color), opacity=0.7, 
              hovertext=~paste(substitute(y, list(y=as.name(grp))), '<br>', Episodes, 'Episodes'),
              hoverinfo='text', 
              text=eval(substitute(~round(x,0), list(x=as.name(var)))), textposition='auto', textfont=list(color='white')) %>% 
    add_trace(x=~Benchmark, name='Benchmark', marker=list(color=color), type='bar', opacity=0.4,
              hovertext=~paste(eval(substitute(y, list(y=as.name(grp)))), 
                               '<br>', Episodes, 'Episodes'),
              hoverinfo='text', text=~round(Benchmark,0), textposition='auto') %>% 
    add_trace(x=~Baseline, name='Baseline', marker=list(color=color), type='bar', opacity=0.25,
              hovertext=~paste(eval(substitute(y, list(y=as.name(grp)))), 
                               '<br>', Episodes, 'Episodes'),
              hoverinfo='text', text=~round(Benchmark,0), textposition='auto') %>% 
    layout(    title=xlabel,
               xaxis=list(title=xlabel, 
                          showticklabels=TRUE, 
                          titlefont=font1, 
                          tickfont=font2,
                          separatethousands=TRUE), 
               yaxis=list(title='', 
                          autorange='reversed',
                          categoryarray=~Episodes, 
                          categoryorder='array',
                          showgrid=FALSE, 
                          showticklabels=TRUE, 
                          tickfont = fontNarrow,
                          ticklen=0,
                          zeroline=FALSE,
                          showline=FALSE),
               barmode = 'group', 
               margin=list(l=170),#, t=0, b=0),
               legend=list(font=fontNarrow)
    )
  return(myfnchart)
}

titleQuality <- function(var) {
  mapVars <- list('HasAdmit' = '% of Episodes with Admissions',
                  'ERVisitFlag' = '% of Episodes with ER Visit',
                  'IPAdmits' = 'Admits per Episode',
                  'ERVisits' = 'ER Visits per Episode',
                  'ICUStays' = 'ICU Stays per Episode',
                  'Savings' = '$Savings (Losses) per Episode',
                  'HadRadOnc' = '% of Episodes with Rad. Onc.',
                  'DiedDuringEpisode' = 'Mortality Rate',
                  'HospicePassOCM3' = '% Episodes with 3+ Days Hospice',
                  'AdmitLast14Days' = '% Admitted in Last 14 Days of Life',
                  'DischargesLast14Days' = '% Discharged in Last 14 Days of Life',
                  'ICULast14Days' = '% in ICU During Last 14 Days of Life',
                  'SurgeryFlag' = '% of Episodes Flagged as Surgical')
  title = mapVars[[var]]
  return(title)
}

attachColors <- function(dfb, dfc, var) {
  # Get all unique values of variable specified
  names(dfb)[names(dfb)==var] <- 'gV'
  names(dfc)[names(dfc)==var] <- 'gV'
  b = distinct(dfb, gV)
  c = distinct(dfc, gV)
  df = rbind(b, c)
  df = distinct(df, gV)
  df = df %>% arrange(gV)
  names(df)[names(df)=='gV'] = var
  UV = nrow(df)
  colors <- colorRampPalette(brewer.pal(12, 'Set3'))(UV)
  df$PhysicianColor = colors
  return(df)
}

histCost = function(df, df2, var, xtitle, barcolor='#8943c0', opaq=0.6, legname='') {
  names(df)[names(df)==var] = 'sumVar'
  names(df2)[names(df2)==var] = 'sumVar'
  mc = max(df$sumVar, na.rm=TRUE)
  mb = max(df2$sumVar, na.rm=TRUE)
  df$sumVar = ifelse(df$sumVar<0, 0, df$sumVar)
  maxcost = max(mc, mb)
  maxPrice = maxcost
  if (maxcost>60000) {
    bw = 5000
  }
  else {
    bw = 2000
  }
  print(maxcost)
  print(min(df$sumVar))
  hist <- hist(df$sumVar, 
               breaks=seq(0,maxPrice+bw,bw), 
               plot=FALSE)
  hist$BarLabel=paste('$', (hist$mids-(bw/2))/1000, '-', (hist$mids+(bw/2))/1000, 'K')
  pc <- plot_ly(x = hist$mids, y = hist$counts, name=legname) %>% 
    add_trace(type='bar', 
              text=~paste('Range: ', hist$BarLabel, '<BR>Episodes: ', hist$counts),
              hoverinfo='text',
              marker=list(color=barcolor),
              opacity=opaq) %>%
    layout(
      xaxis=list(title=xtitle, 
                 showticklabels=TRUE, 
                 titlefont=font1, 
                 tickfont=font2,
                 separatethousands=TRUE,
                 range=c(0,maxcost)), 
      yaxis=list(title='# of Episodes', titlefont=font1,
                 showgrid=TRUE, gridwidth=1, gridcolor='#f9f9f9',
                 showticklabels=TRUE, 
                 ticks='outside', tickfont=font2,
                 separatethousands=TRUE),
      bargap=0)
  return(pc)
}

pieTOS = function(df, opaq=0.8) {
  dfpie <- select(df, "EpiStart", "EpiEnd", "CancerType", "Procs" = "PartBTOSPaidProcedures",
                  "PartBDrugs"="PartBTOSPaidDrugs", "PartBTOSPaidImaging", "PartBTOSPaidLab",
                  "E_M"="PartBTOSPaidE&M", "PartBTOSPaidOther", "PartBTOSPaidChemo", 
                  "RadOnc"="PartBTOSPaidRadOnc", "PartBTOSPaidDME", "PartBTOSPaidEmerg",
                  "Inpat"="IPTotalPaid", "SNFPaid", "HHAPaid", "Hospice"="HospicePaid", "PartDChemoPaid", 
                  "PartDNonChemoPaid", "DMEDrugPaid", "DMENonDrugPaid", "AttributedPhysicianName")
  dfpie <- dfpie %>% na.omit()
  dfpie <- dfpie %>% as_tibble() %>% mutate(
    PartBDrugs = PartBDrugs + DMEDrugPaid + PartBTOSPaidChemo,
    Testing = PartBTOSPaidLab + PartBTOSPaidImaging,
    Other = PartBTOSPaidEmerg + PartBTOSPaidDME + PartBTOSPaidOther + DMENonDrugPaid,
    PostAcute = SNFPaid + HHAPaid,
    Orals = PartDChemoPaid + PartDNonChemoPaid)
  dropCols = c("DMEDrugPaid", "PartBTOSPaidDME", "PartBTOSPaidChemo",
               "PartBTOSPaidImaging", "PartBTOSPaidEmerg", "PartBTOSPaidLab", "DMENonDrugPaid",
               "PartBTOSPaidOther", "SNFPaid", "HHAPaid", "PartDChemoPaid", "PartDNonChemoPaid")
  dfpie <- dfpie %>% dplyr::select(-one_of(dropCols))
  dfpie <- dfpie %>% replace_na(list(Procs=0, PartBDrugs=0, Inpat=0, Hospice=0, Testing=0, 
                                     Other=0, PostAcute=0, Orals=0, E_M=0, RadOnc=0))
  dfpie2 <- gather(dfpie, key=CostType, value=AmountPaid, 
                   Procs, PartBDrugs, Inpat, Hospice, Testing, Other, PostAcute, Orals, E_M, RadOnc)
  dfpie2 <- dfpie2 %>% group_by(CostType) %>% summarize(AmountPaid = mean(AmountPaid))
  dfTOSColors = read_feather('/AdvAnalytics/OCM/Reference/dfTOSColors.feather')
  dfpie2 = merge(dfpie2, dfTOSColors)
  p = plot_ly(dfpie2, labels=~CostType, values=~AmountPaid, type='pie', opacity=opaq,
            textposition='inside', textinfo='label+percent',
            text=~paste('$', AmountPaid), hoverinfo='text', rotation=90, 
            textfont=list(color='white', family='Balto', size=13),
            marker=list(colors=~TOSColor,
                        line=list(width=0)),
            showlegend=FALSE)
  return(p)
}

