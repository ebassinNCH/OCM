## app.R ##
## Import Libraries ----

library(shiny)
library(shinydashboard)
library(shinythemes)
library(feather)
library(plotly)
library(googleVis)
library(treemap)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(RMySQL)
library(leaflet)
library(rlang)
library(rpivotTable)
library(reshape2)

#### Read Data files and prep data ####
setwd('/AdvAnalytics/OCM/CCSI/BaselineUpdated')
#load('/AdvAnalytics/Reference/shapefile_zipcode.RData')
dfepi <- read_feather('Output/dfepi.feather') %>% 
  filter(ReconciliationEligible>0.5)
dfepi$DrugPaid = ( dfepi$PartBTOSPaidDrugs + dfepi$PartBTOSPaidChemo + 
                     dfepi$PartDChemoPaid + dfepi$PartDNonChemoPaid + 
                     dfepi$DMEDrugPaid + dfepi$DMENonDrugPaid)
dfepi$DrugPaidBenchmark = (dfepi$PartBTOSPaidDrugsBenchmark + dfepi$PartBTOSPaidChemoBenchmark +
                             dfepi$PartDChemoPaidBenchmark + dfepi$PartDNonChemoPaidBenchmark + 
                             dfepi$DMEDrugPaidBenchmark + dfepi$DMENonDrugPaidBenchmark)
dfepi$CalendarYear = format(dfepi$EpiStart, '%Y')
dfepi$Savings <- dfepi$BaselinePrice - dfepi$WinsorizedCost
dfepi$OutlierPaid <- dfepi$ActualCost - dfepi$WinsorizedCost
dfepi$IsOutlier <- ifelse(dfepi$ActualCost>dfepi$WinsorizedCost, 100, 0)
dfepi$HasAdmit <- sign(dfepi$IPAdmits)
dfep <- dfepi
dfip <- read_feather('Output/dfip4PowerBI.feather')
refCCN <- read_feather('/AdvAnalytics/Reference/Code_CCN.feather')
dfie  <- read_feather('Output/dfie.feather')
dfie <- distinct(dfie, PatientName, EpiNum, CCN, RevCodeDate, .keep_all=TRUE)
dfie$SentHome <- ifelse(dfie$ClaimType=='Sent Home', 100, 0)
dfie$Admitted <- ifelse(dfie$ClaimType=='Admitted', 100, 0)
dfie$Transfer <- ifelse(dfie$ClaimType=='Transfer', 100, 0)
dfpriceCoef <- read_feather('/AdvAnalytics/OCM/Reference/ref_pricingCoefficients.feather')
dfhcc <- read_feather('Output/dfhcc.feather')
code_hcc <- read_feather('/AdvAnalytics/Reference/code_HCC.feather')
codeCounty <- read_feather('/AdvAnalytics/Reference/code_FIPSCounty.feather')
xwZip2County <- read_feather('/AdvAnalytics/Reference/xw_ZipCode2County.feather')
codeCounty <- read_feather('/AdvAnalytics/Reference/code_FIPSCounty.feather')

#### Get default values and ranges for selects ####
potentialdocs <- sort(unique(dfepi$AttributedPhysicianName))
potentialCT <- sort(unique(dfepi$CancerTypeDetailed))
potentialHosp <- sort(unique(dfie$CCN_lbl))
date1 <- min(dfepi$EpiStart)
date2 <- max(dfepi$EpiStart)
dfdied <- filter(dfepi, (DeathDate>=EpiStart) & (DeathDate<=EpiEnd))
maxActual = max(dfepi$ActualCost)
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

aggQuality <- function(df, var, grp) {
  names(df)[names(df) == grp] <- 'gV'
  names(df)[names(df) == var] <- 'sV'
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
  if ( (max(dfb$YourScore)<=1)  & (min(dfb$YourScore)>=0) ) {
    dfb <- mutate(dfb, 
                  YourScore = YourScore * 100,
                  Benchmark = Benchmark * 100)
  }
  print(names(dfb))
  names(dfb)[names(dfb)=='YourScore'] <- var
  names(dfb)[names(dfb)=='gV'] <- grp
  print(names(dfb))
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
    add_trace(x=~Benchmark, name='$Benchmark', marker=list(color=color), type='bar', opacity=0.4,
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

#### UI Section ####
ui <- dashboardPage(
  skin='blue',
  dashboardHeader(title='OCM Baseline Analysis', titleWidth=300),
  #### Dashboard Sidebar ####
  dashboardSidebar(width=225,
    HTML('<H4><center>DATA FILTERS</center></h4><hr>'),
    selectInput('Docs', label='Attributed Provider(s)', 
                choices=potentialdocs, multiple=TRUE),
    selectInput('CT', label='Cancer Type(s)', 
              choices=potentialCT, multiple=TRUE),
    dateRangeInput('filterDates', label='Episode Start Date',
                   start=date1, end=date2, format='mm/dd/yyyy'),
    HTML('<BR><HR><H4><center>Report Configuration</center></h4><hr>'),
    selectInput('TOSGroupBy', label='Y-Axis Groups', 
                choices=c('CancerTypeDetailed', 
                          'AttributedPhysicianName',
                          'CalendarYear'),
                selected='AttributedPhysicianName'),
    selectInput('TOSSortBy', label='Sort Data By',
                choices=c('MeanCost', 'TotalCost', 'Inpat', 'PartBDrugs', 
                          'Orals', 'RadOnc', 'Testing'),
                selected='MeanCost')
    
  ),
  #### Dashboard Body ####
  dashboardBody(
    tabsetPanel(type='tabs',
                #### KPI Tab ####
                tabPanel('KPI', 
                    fluidRow(
                      valueBoxOutput('ValueEpis', width=2),
                      valueBoxOutput('ValueUniquePatients', width=2),
                      valueBoxOutput('ValueMeanAge', width=2), 
                      valueBoxOutput('ValueUniqueProv', width=2),
                      valueBoxOutput('ValueTotalSpend', width=2),
                      valueBoxOutput('ValueAdmits', width=2)
                    ),
                    fluidRow( 
                      column(6, 
                          tabBox(
                            title='Financial Distributions',
                            side='left', width=12,
                            selected='Actual Cost',
                            tabPanel('Target Price', plotlyOutput('histTargetPrice', height='250px')),
                            tabPanel('Actual Cost',  plotlyOutput('histActualCost', height='250px')),
                            tabPanel('Winsorized Cost',  plotlyOutput('histWinsorized', height='250px'))
                          )
                      ),
                      column(6,          
                        box(solidHeader=TRUE, title='Target Price v. Winsorized Cost',
                           status='success', width=NULL,
                           plotlyOutput('scatter2', height='250px')
                        )
                      )
                    ),
                    fluidRow(
                      column(4,
                        box(solidHeader=TRUE, title='Physician Measures',
                            status='success', width=NULL,
                            HTML('<h4><CENTER>% Patients with 3+ Days of Hospice</CENTER></h4>'),
                            plotlyOutput('dotHospice', height='120px'),
                            HTML('<HR><H4><Center>Savings Per Episode</center></h4>'),
                            plotlyOutput('dotSavings', height='120px')
                        )
                      ),
                      column(4,
                             box(solidHeader=TRUE, title='Physician Measures',
                                 status='success', width=NULL,
                                 HTML('<h4><CENTER>% of Episodes with Admissions</CENTER></h4>'),
                                 plotlyOutput('dotAdmit', height='120px'),
                                 HTML('<HR><H4><Center>% of Episodes with ER Visits</center></h4>'),
                                 plotlyOutput('dotER', height='120px')
                             )
                      ),
                      column(4,
                             box(solidHeader=TRUE, title='Discharge Sites from ER Visits',
                                 status='success', width=NULL, plotlyOutput('ieTree', height='350px')
                             )     
                      )
                    )
                ), # close tabPanel
                #### Quality Tab ####
                tabPanel('Quality',
                  fluidRow(column(3, HTML('</H4>All Episodes</H4>')),
                           valueBoxOutput('ValueQualityIP', width=2),
                           valueBoxOutput('ValueQualityER', width=2),
                           valueBoxOutput('ValueQualityMort', width=2),
                           valueBoxOutput('ValueQualityICU', width=2)
                  ),
                  fluidRow(column(3, HTML('<H4>Select variable to report:</h4>')),
                           column(2, selectInput('Qmeasure1', label=NULL,
                                                 choices=c('HasAdmit', 'ERVisitFlag', 'IPAdmits',
                                                           'ERVisits', 'ICUStays', 'DiedDuringEpisode',
                                                           'HadRadOnc', 'SurgeryFlag', 'Savings'),
                                                 selected='HasAdmit')),
                           column(1, HTML(' ')),
                           column(3, HTML('<H4>Select variable to report:</h4>')),
                           column(2, selectInput('Qmeasure2', label=NULL,
                                                 choices=c('HasAdmit', 'ERVisitFlag', 'IPAdmits',
                                                           'ERVisits', 'ICUStays', 'DiedDuringEpisode',
                                                           'HadRadOnc', 'SurgeryFlag', 'Savings'),
                                                 selected='ERVisitFlag'))
                  ),
                  fluidRow(column(6, box(solidHeader=FALSE, title='Quality Measure 1', width=NULL, status='success',
                                         plotlyOutput('Quality1', height='500px'))
                           ),
                           column(6, box(solidHeader=FALSE, title='Quality Measure 2', width=NULL, status='success',
                                        plotlyOutput('Quality2', height='500px'))
                           )
                  ),
                  fluidRow(column(3, HTML('<H4>Episodes Ending in Death</H4>')),
                               valueBoxOutput('ValueQualityOCM3', width=3),
                               valueBoxOutput('ValueQuality14Admit', width=3),
                               valueBoxOutput('ValueQualityEOLICU', width=3)
                  ),
                  fluidRow(column(3, HTML('<H4>Select variable to report:</h4>')),
                           column(2, selectInput('Qmeasure3', label=NULL,
                                                 choices=c('HospicePassOCM3', 'AdmitLast14Days',
                                                           'DischargesLast14Days', 'ICULast14Days'),
                                                 selected='HospicePassOCM3')),
                           column(1, HTML(' ')),
                           column(3, HTML('<H4>Select variable to report:</h4>')),
                           column(2, selectInput('Qmeasure4', label=NULL,
                                                 choices=c('HospicePassOCM3', 'AdmitLast14Days',
                                                           'DischargesLast14Days', 'ICULast14Days'),
                                                 selected='AdmitLast14Days'))
                  ),
                  fluidRow(column(6, box(solidHeader=FALSE, title='Quality Measure 3', width=NULL, status='success',
                                         plotlyOutput('Quality3', height='500px'))
                           ),
                           column(6, box(solidHeader=FALSE, title='Quality Measure 4', width=NULL, status='success',
                                plotlyOutput('Quality4', height='500px'))
                           )
                  )
                  
              ), #close tabPanel for quality
              #### Geography Tab ####
              tabPanel('Geography',
                  fluidRow(
                    column(3, HTML("<H4>Select variable to map: </H4>")),
                    column(2, selectInput("KeyStatVar", label=NULL, 
                                          choices=c('Episodes', 'AdmitsPerEpisode', 'ERVisitsPerEpisode',
                                                    'MeanSavings', 'DrugsPaid', 'DrugSavings', 'InpatPaid',
                                                    'InpatSavings', 'TestingPaid', 'TestingSavings'), 
                                          selected='MeanSavings')),
                    column(1, HTML('')),
                    column(4, HTML('<H4>Mininum Episodes in Geographic Area:')),
                    column(1, numericInput('GeoMinEpis', label=NULL, value = 10))
                  ),
                  fluidRow(
                      column(6,
                          tabBox(title=HTML('<H3>Zip Code Maps'),
                                 side='left', width=12,
                                 selected='Zip Code Map',
                                 tabPanel('Zip Code Map',   leafletOutput('KeyStatZipMap', height='600px')),
                                 tabPanel('Zip Code Table', DT::dataTableOutput('KeyStatTable', height='600px'))
                          )
                      ),
                      column(6,
                             tabBox(title=HTML('<H3>County Maps'),
                                    side='left', width=12,
                                    selected='Map',
                                    tabPanel('Map', leafletOutput('KeyStatCountyMap', height='600px')),
                                    tabPanel('Table', DT::dataTableOutput('KeyStatCountyTable', height='600px'))
                             )
                      )
                  )
              ),
              #### Pricing Tab ####
              tabPanel('Pricing',
                       fluidRow(
                         infoBoxOutput('IBEpis'), 
                         infoBoxOutput('IBPrice'),
                         infoBoxOutput('IBWinsor')
                       ),
                       fluidRow(
                         column(6, 
                                box(solidHeader=TRUE, title='Average Price by Cancer Type',
                                    status='primary', width=NULL,
                                    plotlyOutput('priceCTSum', height='680px') )
                         ),
                         column(6, 
                                box(solidHeader=TRUE, title='Target Price Distribution',
                                    status='primary', width=NULL,
                                    plotlyOutput('histPrice', height='300px')),
                                box(solidHeader=TRUE, title='Target Price Distribution',
                                    status='primary', width=NULL,
                                    plotlyOutput('PriceScatter', height='300px'))
                         )
                       ) # close fluidRow
              ), # close Pricing tabPanel
              #### Price Model Tab ####
              tabPanel('Price Model',
                     column(6, 
                            box(solidHeader=FALSE, title='Base (Starting Price) by Cancer Type',
                                status='primary', width=NULL,
                                plotlyOutput('PMBase', height='680px') )
                     ), # close column
                     column(6,
                            tabBox(
                              title='Price Modifiers',
                              side='left', width=12,
                              selected='Age-Sex',
                              tabPanel('Age-Sex', plotlyOutput('PMAgeSex', height='680px')),
                              tabPanel('Other', plotlyOutput('PMOther', height='680px'))
                            )
                     )
            ), # close tabPanel for Price model
            #### Pricing Factors Tab ####
            tabPanel('Pricing Factors', 
                     fluidRow(
                       column(6, 
                               box(solidHeader=FALSE, title='Clinical Trial Coding',
                                   status='primary', width=NULL,
                                   plotlyOutput('TrialsStacked', height='570px') )
                       ),
                       column(6, 
                              box(solidHeader=FALSE, title='HCC Coding',
                                 status='primary', width=NULL,
                                 plotlyOutput('HCCChart', height='570px') )
                       )
                     )
                     ,
                     fluidRow(
                       column(6,
                              box(solidHeader=FALSE, title='Patients in Clinical Trials',
                                  status='primary', width=NULL,
                                  DT::dataTableOutput('TableTrials', height='570px') 
                              )
                       ),
                       column(6, 
                              box(solidHeader=FALSE, title='Drug Clean Periods',
                                  status='primary', width=NULL,
                                  plotlyOutput('CleanPeriod', height='570px'))
                       )
                     )
            ),
            #### Outlier Tab ####
            tabPanel('Outliers',
                     tabBox(title=HTML('<H3>Outlier Episodes</h3>'),
                        side='left', width=12, selected='Chart',
                        tabPanel('Chart', plotlyOutput('OutlierBar')),
                        tabPanel('Patient List', DT::dataTableOutput('OutlierTable'))
                     )
            ),
            #### Type of Service Tab ####
            tabPanel('Type of Service',
                     column(3,
                            box(solidHeader=FALSE, title='Type of Service Summary',
                                status='primary', width=NULL,
                                plotlyOutput('TOSPie')
                            )
                     ),
                     column(9, 
                            box(solidHeader=FALSE, title='Cost by Type of Service',
                                status='primary', width=NULL,
                                plotlyOutput('TOSStack', height='680px') )
                     ) # close column
            ),
            #### ER Tab ####
            tabPanel('ER',
                     fluidRow(
                       valueBoxOutput('ValueERVisits', width=2),
                       valueBoxOutput('ValueERPaid',   width=2),
                       valueBoxOutput('ValueERPaidPerVisit', width=2),
                       valueBoxOutput('ValueERPctHome', width=2),
                       valueBoxOutput('ValueERAdmit',  width=2),
                       valueBoxOutput('ValueERTrans',  width=2)
                     ),
                     fluidRow(
                       column(2, style = "background-color:#ffe6cc;",
                              selectInput('ieHosp', 'Hospital to Include', potentialHosp, 
                                          selected=NULL, multiple=TRUE),
                              selectInput('ieCCS', 'Diag Group Filter', unique(dfie$CCS_lbl), 
                                          selected=NULL, multiple = TRUE),
                              selectInput('ieGrp', 'Variable to Group Reports By:', 
                                          choices=c('CCS_lbl','CCN_lbl', 
                                                    'AttributedPhysicianName', 'CancerTypeDetailed'),
                                          selected='CCS_lbl')
                       ),
                       column(6, 
                              tabBox(
                                title='Discharge Site Mix',
                                side='left', width=12,
                                selected='Summary Chart',
                                tabPanel('Summary Chart', plotlyOutput('ERDischargeMix', height='680px')),
                                tabPanel('Summary Table', DT::dataTableOutput('ERDischargeTab', height='680px')),
                                tabPanel('Claims', DT::dataTableOutput('ERPatientsTab', height='680px'))
                              )
                       ),
                       column(4, 
                              tabBox(
                                title='ER Statistics',
                                side='left', width=12,
                                tabPanel('Map', leafletOutput('ERMap', height='680px')),
                                tabPanel('Table', DT::dataTableOutput('ERSumTab', height='680px'))
                              )
                       )
                     )
            ),
            #### Inpatient Tab ####
            tabPanel('Inpatient',
                     column(6,
                            tabBox(title=HTML('<H3>Admission Rate</h3>'),
                                   side='left', width=12, selected='# of Admits', height='560px',
                                   tabPanel('# of Admits',
                                       tabBox(title='# of Admits', side='left', width=12, selected='Pie of Count',
                                          tabPanel('Pie of Count', plotlyOutput('IPPie')),
                                          tabPanel('Table', DT::dataTableOutput('IPAdmitsTable')))
                                   ),
                                   tabPanel('Benchmark Comparison', 
                                        tabBox(title='Benchmarks', side='left', width=12, 
                                               selected='Bar Chart',
                                           tabPanel('Bar Chart', plotlyOutput('IPAdmitBenchmark')),
                                           tabPanel('Table', DT::dataTableOutput('IPBenchTable')))
                                   ),
                                   tabPanel('Admit Count', 
                                        tabBox(title='# of Admits', side='left', width=12, selected='Chart',
                                            tabPanel('Chart', plotlyOutput('IPAdmitsPerEpisode')),
                                            tabPanel('Table', DT::dataTableOutput('IPAPEtable')))
                                   ),
                                   tabPanel('Cancer Type',
                                        tabBox(title='Cancer Type', side='left', width=12, selected='Chart',
                                               tabPanel('Chart', plotlyOutput('IPCTbar')),
                                               tabPanel('Table', DT::dataTableOutput('IPCTtab')))
                                   )
                            )
                     ),
                     column(6, 
                            tabBox(title=HTML('<H3>Hospital Stats</h3>'),
                                   side='left', width=12, selected='Chart', height='560px',
                               tabPanel('Chart', plotlyOutput('IPHospBar')),
                               tabPanel('Table', DT::dataTableOutput('IPHospTab'))
                            )
                     ) # close column
            ),
            tabPanel('Inpat 2',
                column(7,
                       box(solidHeader=TRUE, title='Diagnosis Analysis',
                           status='warning', width=NULL, rpivotTableOutput('IPPivotDiag'))
                )
            ),
            #### Drugs Tab ####
            tabPanel('Drugs',
                     column(7,
                            tabBox(title='Drug Regimens', side='left', width=12, height='580px',
                              tabPanel('Regimen Pie', 
                                       HTML('<H5><center>We recommend that you filter these reports by cancer type</center></h5><hr>'),
                                       plotlyOutput('DrugRegimenPie')
                              ),
                              tabPanel('Regimen Table', dataTableOutput('DrugRegimenTable')),
                              tabPanel('Top Drug Table', dataTableOutput('DrugMostExpensiveTable')),
                              tabPanel('Top Chemo Table', dataTableOutput('DrugMostExpensiveChemoTable'))
                            )
                     ),
                     column(5, 
                            tabBox(title='Cost vs. Benchmark', side='left', width=12,
                                   height='540px',
                              tabPanel('Bar Chart', plotlyOutput('DrugVsBenchmark'))
                            )
                     ) # close column
            ),
            #### End of Life Tab ####
            tabPanel('End of Life',
                fluidRow(
                    valueBoxOutput('ValueEOLDeaths', width=2),
                    valueBoxOutput('ValueEOLPctHospice', width=2),
                    valueBoxOutput('ValueEOLPassOCM3', width=2),
                    valueBoxOutput('ValueEOLHospicePaid', width=2),
                    valueBoxOutput('ValueEOLHospiceSavings0', width=2),
                    valueBoxOutput('ValueEOLHospiceSavings3', width=2)
                ),
                fluidRow(
                     column(6, 
                        tabBox(title='Deaths', side='left', width=12, height='500px', selected='Bar Chart',
                           tabPanel('Bar Chart', plotlyOutput('EOLDeathCount')),
                           tabPanel('Pie Chart', plotlyOutput('EOLDeathCountPie')),
                           tabPanel('Summary Table', DT::dataTableOutput('EOLDeathCountTab')),
                           tabPanel('Patient List', DT::dataTableOutput('EOLDeathPatientList'))
                        )
                     ),
                     column(6,
                        tabBox(title='Hospice Quality Measure', side='left', width=12, height='500px', selected='Chart',
                           tabPanel('Chart', plotlyOutput('EOLHospiceDistBar')),
                           tabPanel('Table', DT::dataTableOutput('EOLHospiceDistTab'))
                        )   
                     )
                ),
                fluidRow(
                    infoBoxOutput('IBEOLQualityAdmit'),
                    infoBoxOutput('IBEOLQualityICU'),
                    infoBoxOutput('IBEOLQualityChemo')
                ),
                fluidRow(
                    tabBox(title='Care in Last 14 Days of Life', side='left', width=12, height='530px', selected='Admits',
                       tabPanel('Admits', plotlyOutput('EOLQualityAdmit', height='450px')),
                       tabPanel('ICU',    plotlyOutput('EOLQualityICU', height='450px')),
                       tabPanel('Chemo', plotlyOutput('EOLQualityChemo', height='450px'))
                    )
                )
            ),
            #### Simulation Tab ####
            tabPanel('Simulation',
                     # Settings and instructions ####
                     fluidRow(column(1, numericInput('BootSize', label='Sample Size', value = round(nrow(dfep)/6,0))),
                              column(11, HTML('<H3><center>About this Tab</center></h3>This tab presents the results of a "bootstrap" 
                                              simulation.  The simulation mimics the random variation that a practice 
                                              is likely to experience under OCM.  Essentially, the simulation involves
                                              computing the costs, target prices, and savings associated with a random 
                                              sample of episodes from your baseline data.  This tab repeats that random 
                                              sampling 100 times, with each sample having a different set of episodes.<br>br>
                                              The results show simulated reconciliations as they will occur in the OCM 
                                              program.  You will almost certainly see some reconciliations where you have 
                                              savings and some where you have losses.  The key is to understand the 
                                              volatility that is almost sure to occur with your program.  <b>The results 
                                              assume that you make no real changes to the way that you treat patients.</b>  
                                              We realize that assumption is absurd, but the volatility will persist anyway.
                                              <br>To use this page, type a sample size (number of episodes) into the box 
                                              on the left.  (The default is the average number of episodes that you had per 
                                              half-year during the baseline period.)  Each time you change the number, the 
                                              software will create 100 new samples.  <i>If you leave this page and return 
                                              to it, the simulation will be repeated from scratch and you will see new 
                                              results.<i>')
                              )
                              
                    ), 
                    # Reports ####
                    fluidRow(
                        tabBox('Volatility', side='left', width=12, height='510px', 
                               selected='First 25 Reconciliations',
                            tabPanel('First 25 Reconciliations', plotlyOutput('BootBar')),
                            tabPanel('Histogram', plotlyOutput('BootHist')),
                            tabPanel('Control Chart', plotlyOutput('BootControlChart'))
                        )
                    )
            )
            
    ) # close tabsetPanel
  ) # close dashboardBody
) # close dashboardPage

           
           

#### Server Section ####
server <- function(input, output) {  
  #### Functions to read data files ####
  ZipShape <- reactive( { 
      load('/AdvAnalytics/Reference/shapefile_zipcode.RData')
      return(subdat)
  })
  CountyShape <- reactive( {
    load('/AdvAnalytics/Reference/shapefile_county.RData')
    return(dat)
  })    
  #### Filter Functions ####
  filterdfepi <- function(df) {
    # dfep <- dfepi %>% 
    #   filter(EpiStart>=input$filterDates[1]) %>%  
    #   filter(EpiStart<=input$filterDates[2]) 
    dfq <- df
    if (!is.null(input$Docs)) {
      dfq <- dfq %>% filter(AttributedPhysicianName %in% input$Docs)
    }
    if (!is.null(input$CT)) {
      dfq <- dfq %>% filter(CancerTypeDetailed %in% input$CT)
    }
    return(dfq)
  }

  filterER <- function(df) {
    # dfep <- dfepi %>% 
    #   filter(EpiStart>=input$filterDates[1]) %>%  
    #   filter(EpiStart<=input$filterDates[2]) 
    dfi <- dfie
    if (!is.null(input$ieHosp)) {
      dfi <- dfi %>% filter(CCN_lbl %in% input$ieHosp)
    }
    if (!is.null(input$ieCCS)) {
      dfi <- dfi %>% filter(CCS_lbl %in% input$ieCCS)
    }
    return(dfi)
  }
  
  #### KPI Tab Outputs ####
  output$ValueEpis <- renderValueBox( {
    dfep <- filterdfepi(dfepi)
    Episodes = nrow(dfep)
    valueBox(value=Episodes, subtitle='# of Episodes', color='light-blue', icon=icon('hand-left', lib='glyphicon'))
  })
  output$ValueUniquePatients <- renderValueBox( {
    dfep <- filterdfepi(dfepi)
    UniquePats <- length(unique(dfep$BeneSK))
    valueBox(value=UniquePats, subtitle='# of Patients', color='light-blue', icon=icon('venus-marsr'))
  })
  output$ValueMeanAge <- renderValueBox( {
    dfep <- filterdfepi(dfepi)
    MeanAge <- round(mean(dfep$Age),1)
    valueBox(value=MeanAge, subtitle='Average Age', color='light-blue', icon=icon('calendar'))
  })
  output$ValueUniqueProv <- renderValueBox( {
    dfep <- filterdfepi(dfepi)
    UniqueDocs <- length(unique(dfep$AttributedNPI))
    valueBox(value=UniqueDocs, subtitle='# of Providers', color='light-blue', icon=icon('users'))
  })
  output$ValueTotalSpend <- renderValueBox( {
    dfep <- filterdfepi(dfepi)
    totSpend <- format(round(sum(dfep$WinsorizedCost),0), big.mark=",", trim=TRUE)
    valueBox(value=paste('$', round(sum(dfep$WinsorizedCost)/1e6,1), 'M'), subtitle='Winsorized $Spend', color='light-blue', icon=icon('usd'))
  })  
  output$ValueAdmits <- renderValueBox( {
    dfep <- filterdfepi(dfepi)
    Admits <- format(sum(dfep$IPAdmits), big.mark=",", trim=TRUE)
    valueBox(value=Admits, subtitle='Admissions', color='light-blue', icon=icon('ambulance'))
  })  
  output$histActualCost <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    bp = dfep$ActualCost
    maxPrice = max(bp)
    hist <- hist(dfep$ActualCost, 
                 breaks=seq(0,maxPrice+5000,5000), 
                 plot=FALSE)
    hist$BarLabel=paste('$', (hist$mids-2500)/1000, '-', (hist$mids+2500)/1000, 'K')
    plot_ly(x = hist$mids, y = hist$counts) %>% 
      add_trace(type='bar', 
                text=~paste('Range: ', hist$BarLabel, '<BR>Episodes: ', hist$counts),
                hoverinfo='text',
                marker=list(color='#8943c0'),
                opacity=0.6) %>%
      layout(
        xaxis=list(title='Episode Cost (not Winsorized)', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='# of Episodes', titlefont=font1,
                   showgrid=TRUE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=TRUE, 
                   ticks='outside', tickfont=font2,
                   separatethousands=TRUE),
        bargap=0)
  } )
  output$histTargetPrice <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    # target price distribution
    bp = dfep$BaselinePrice
    maxPrice = max(bp)
    hist <- hist(dfep$BaselinePrice, 
                 breaks=seq(0,maxPrice+5000,5000), 
                 plot=FALSE)
    hist$BarLabel=paste('$', (hist$mids-2500)/1000, '-', (hist$mids+2500)/1000, 'K')
    plot_ly(x = hist$mids, y = hist$counts) %>% 
      add_trace(type='bar', 
                text=~paste('Range: ', hist$BarLabel, '<BR>Episodes: ', hist$counts),
                hoverinfo='text',
                marker=list(color='#7ac043'),
                opacity=0.6) %>%
      layout(
        xaxis=list(title='OCM Target Price', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='# of Episodes', titlefont=font1,
                   showgrid=TRUE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=TRUE, 
                   ticks='outside', tickfont=font2,
                   separatethousands=TRUE),
        bargap=0)
  } )
  output$histWinsorized <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    # target price distribution
    bp = dfep$WinsorizedCost
    maxPrice = max(bp)
    hist <- hist(dfep$WinsorizedCost, 
                 breaks=seq(0,maxPrice+5000,5000), 
                 plot=FALSE)
    hist$BarLabel=paste('$', (hist$mids-2500)/1000, '-', (hist$mids+2500)/1000, 'K')
    plot_ly(x = hist$mids, y = hist$counts) %>% 
      add_trace(type='bar', 
                text=~paste('Range: ', hist$BarLabel, '<BR>Episodes: ', hist$counts),
                hoverinfo='text',
                marker=list(color='#c08943'),
                opacity=0.6) %>%
      layout(
        # title='\nHistogram of Target Price',
        xaxis=list(title='Winsorized (Truncated) Cost', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='# of Episodes', titlefont=font1,
                   showgrid=TRUE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=TRUE, 
                   ticks='outside', tickfont=font2,
                   separatethousands=TRUE),
        bargap=0)
  } )
  output$scatter2 <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    plot_ly(dfep, x=~WinsorizedCost, y=~BaselinePrice, color=~CancerType) %>%  
        add_trace(type='scatter',
            text=~paste(PatientName, '<BR>', 
                        CancerType, '<br>',
                        'Episode Start Date:', EpiStart,
                        '<br>Target Price: $', format(round(BaselinePrice,0), big.mark=",", trim=TRUE),
                        '<br>Actual Cost: $', format(round(ActualCost,0), big.mark=",", trim=TRUE),
                        '<BR>Winsorized Cost: $', format(round(WinsorizedCost,0), big.mark=",", trim=TRUE)),
            hoverinfo='text') %>% 
      layout(
        # title='Target Price v. Episode Cost',
        xaxis=list(title='Winsorized Episode Cost', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='Baseline Target Price', 
                   titlefont=font1,
                   tickfont=font2,
                   showgrid=TRUE),
        legend=list(font=fontNarrow)
      )
  })
  output$dotHospice <- renderPlotly( {
    dfdied <- filterdfepi(dfdied)
    dfOCM3 <- dfdied %>% 
      group_by(AttributedPhysicianName) %>% 
      summarise(
        Deaths=n(),
        ComplianceRate=mean(HospicePassOCM3)
      )
    dfOCM3$DeathSize = round(sqrt(dfOCM3$Deaths*3), 0)
    dfOCM3$ComplianceRate = dfOCM3$ComplianceRate*100
    plot_ly(dfOCM3,
            x=~ComplianceRate,
            y=1,
            type='scatter',
            marker=list(size=~DeathSize, opacity=.5),
            color=~AttributedPhysicianName,
            text=~paste('Provider: ', AttributedPhysicianName, '   Deaths', Deaths,
                        '   Hospice Rate', round(ComplianceRate,1), '%'),
            hoverinfo='text',
            height=100) %>%
      layout(
        xaxis=list(title='% of Deaths with 3+ Days Hospice', 
                   showticklabels=TRUE,  
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE, zeroline=FALSE), 
        yaxis=list(showgrid=FALSE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=FALSE),
        showlegend=FALSE)
    } )  
  output$dotSavings <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    dfep$NetAmount <- dfep$BaselinePrice - dfep$WinsorizedCost
    dfw <- dfep %>% 
      group_by(AttributedPhysicianName) %>% 
      summarise(
        Episodes=n(),
        MeanSavings=mean(NetAmount)
      )
    dfw$DotSize = round(sqrt(dfw$Episodes), 0)
    plot_ly(dfw,
            x=~MeanSavings,
            y=1,
            type='scatter',
            marker=list(size=~DotSize, opacity=.5),
            color=~AttributedPhysicianName,
            text=~paste(AttributedPhysicianName, '   Episodes:', Episodes,
                        '   Average Savings/Losses', round(MeanSavings,0)),
            hoverinfo='text',
            height=100) %>%
      layout(
        xaxis=list(title='Savings/Losses Per Episode', 
                   showticklabels=TRUE,  
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE, 
                   zeroline=FALSE), 
        yaxis=list(showgrid=FALSE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=FALSE),
        showlegend=FALSE)
  } )  
  output$dotAdmit <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    dfw <- dfep %>% 
      group_by(AttributedPhysicianName) %>% 
      summarise(
        Episodes=n(),
        AdmitRate=mean(sign(IPAdmits)) * 100
      )
    dfw$DotSize = round(sqrt(dfw$Episodes), 0)
    plot_ly(dfw,
            x=~AdmitRate,
            y=1,
            type='scatter',
            marker=list(size=~DotSize, opacity=.5),
            color=~AttributedPhysicianName,
            text=~paste(AttributedPhysicianName, '   Episodes:', Episodes,
                        '   Admit Rate', round(AdmitRate,1), '%'),
            hoverinfo='text',
            height=100) %>%
      layout(
        xaxis=list(title='% of Episodes with 1+ Admissions', 
                   showticklabels=TRUE,  
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE, 
                   zeroline=FALSE), 
        yaxis=list(showgrid=FALSE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=FALSE),
        showlegend=FALSE)
    })  
  output$dotER <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    dfw <- dfep %>% 
      group_by(AttributedPhysicianName) %>% 
      summarise(
        Episodes=n(),
        AdmitRate=mean(sign(ERVisits)) * 100
      )
    dfw$DotSize = round(sqrt(dfw$Episodes), 0)
    plot_ly(dfw,
            x=~AdmitRate,
            y=1,
            type='scatter',
            marker=list(size=~DotSize, opacity=.5),
            color=~AttributedPhysicianName,
            text=~paste(AttributedPhysicianName, '   Episodes:', Episodes,
                        '   ER Visit Rate', round(AdmitRate,1), '%'),
            hoverinfo='text',
            height=100) %>%
      layout(
        xaxis=list(title='% of Episodes with 1+ ER Visits', 
                   showticklabels=TRUE,  
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE, 
                   zeroline=FALSE), 
        yaxis=list(showgrid=FALSE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=FALSE),
        showlegend=FALSE)
  })    
  output$ieTree <- renderPlotly( {
    dfi <- filterdfepi(dfie)
    dfi <- dfi %>% group_by(ClaimType) %>%
                   summarise(Discharges=n(),
                             meanCost=mean(Paid))
    colors <- colorRampPalette(brewer.pal(8, 'Set2'))(3)
    plot_ly(dfi, labels=~ClaimType, values=~Discharges, type='pie', opacity=.9,
                textposition='inside', textinfo='label+percent',
                text=~paste(ClaimType, '\n', Discharges, 'Discharges'), 
                hoverinfo='text', rotation=90, 
                textfont=list(color='white', family='Balto', size=13),
                marker=list(color=colors,
                            line=list(width=0)),
                showlegend=FALSE)
    })
  
  #### Geography Tab Outputs ####
  output$KeyStatZipMap <- renderLeaflet({
    dfep <- filterdfepi(dfepi)
    dfw <- dfep %>% group_by(ZipCode) %>% 
      summarise(
        Episodes=n(),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits),
        MeanSavings=mean(Savings),
        DrugsPaid=mean(DrugPaid),
        DrugSavings=mean(DrugPaidBenchmark) - mean(DrugPaid),
        InpatPaid=mean(IPTotalPaid),
        InpatSavings=mean(IPTotalPaidBenchmark) - mean(IPTotalPaid),
        TestingPaid = mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging),
        TestingSavings = mean(PartBTOSPaidLabBenchmark) + mean(PartBTOSPaidImagingBenchmark) - mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging)
      ) %>% 
      filter(Episodes>=input$GeoMinEpis)
    subdat <- ZipShape()
    subdat<-subdat[subdat$GEOID10 %in% dfw$ZipCode,]
    dfw <- merge(subdat, dfw, by.x='GEOID10', by.y='ZipCode')
    zipcodeMap <- zipMap(dfw, dfName='dfw', Var=input$KeyStatVar, digits=0, varlabel='w')
    zipcodeMap
  })
  output$KeyStatTable <- DT::renderDataTable({
    dfep <- filterdfepi(dfepi)
    dfw <- dfep %>% group_by(ZipCode) %>% 
      summarise(
        Episodes=n(),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits),
        MeanSavings=mean(Savings),
        DrugsPaid=mean(DrugPaid),
        DrugSavings=mean(DrugPaidBenchmark) - mean(DrugPaid),
        InpatPaid=mean(IPTotalPaid),
        InpatSavings=mean(IPTotalPaidBenchmark) - mean(IPTotalPaid),
        TestingPaid = mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging),
        TestingSavings = mean(PartBTOSPaidLabBenchmark) + mean(PartBTOSPaidImagingBenchmark) - mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging)
      ) %>% 
      arrange(desc(Episodes))
    keyTable <- OutputTable(dfw, headerCols=c('ZipCode'))
    keyTable
  })
  output$KeyStatCountyMap <- renderLeaflet({
    dfep <- filterdfepi(dfepi)
    dfep <- merge(dfep, xwZip2County, by='ZipCode', all.x=TRUE)
    dfwc <- dfep %>% group_by(County) %>% 
      summarise(
        Episodes=n(),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits),
        MeanSavings=mean(Savings),
        DrugsPaid=mean(DrugPaid),
        DrugSavings=mean(DrugPaidBenchmark) - mean(DrugPaid),
        InpatPaid=mean(IPTotalPaid),
        InpatSavings=mean(IPTotalPaidBenchmark) - mean(IPTotalPaid),
        TestingPaid = mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging),
        TestingSavings = mean(PartBTOSPaidLabBenchmark) + mean(PartBTOSPaidImagingBenchmark) - mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging)
      ) %>% 
      filter(Episodes>=input$GeoMinEpis)
    dat <- CountyShape()
    dat<-dat[dat$GEOID10 %in% dfwc$County,]
    dfwc <- merge(dat, dfwc, by.x='GEOID10', by.y='County') 
    # MyCountyMap <- countyMap(dfName='dfwc', Var=input$KeyStatVar, digits=0, varlabel='S')
    MyCountyMap <- countyMap(dfwc, dfName='dfwc', Var=input$KeyStatVar, digits=0, varlabel='S')
    MyCountyMap
  })
  output$KeyStatCountyTable <- DT::renderDataTable( {
    dfep <- filterdfepi(dfepi)
    dfep <- merge(dfep, xwZip2County, by='ZipCode', all.x=TRUE)
    dfep <- merge(dfep, codeCounty, by.x='County', by.y='FIPSCounty', all.x=TRUE) %>% 
      rename(CountyName=County_lbl)
    dfwc <- dfep %>% group_by(CountyName) %>% 
      summarise(
        Episodes=n(),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits),
        MeanSavings=mean(Savings),
        DrugsPaid=mean(DrugPaid),
        DrugSavings=mean(DrugPaidBenchmark) - mean(DrugPaid),
        InpatPaid=mean(IPTotalPaid),
        InpatSavings=mean(IPTotalPaidBenchmark) - mean(IPTotalPaid),
        TestingPaid = mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging),
        TestingSavings = mean(PartBTOSPaidLabBenchmark) + mean(PartBTOSPaidImagingBenchmark) - mean(PartBTOSPaidLab) + mean(PartBTOSPaidImaging)
      ) %>% arrange(desc(Episodes))
    dfcountytab <- OutputTable(dfwc, headerCols = c('CountyName'))
    dfcountytab
  })
  
  #### Quality Tab Outputs ####
  output$ValueQualityIP <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      OCM1 <- round(mean(dfep$HasAdmit),3) * 100
      valueBox(value=OCM1, subtitle='% Episodes w/ Admit', color='teal', icon=icon('h-square'))
  })
  output$ValueQualityER <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      OCM2 <- round(mean(dfep$ERVisitFlag),3) * 100
      valueBox(value=OCM2, subtitle='% Episodes w/ ER', color='teal', icon=icon('ambulance'))
  })
  output$ValueQualityMort <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      Mort <- paste0(round(mean(dfep$DiedDuringEpisode),3) * 100, '%')
      valueBox(value=Mort, subtitle='Mortality Rate', color='teal', icon=icon('percent'))
  })
  output$ValueQualityICU <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      ICU <- paste0(sum(dfep$ICUStays))
      valueBox(value=ICU, subtitle='ICU Stays', color='teal', icon=icon('bed'))
  })
  output$ValueQualityOCM3 <- renderInfoBox( {
      dfepdie <- filterdfepi(dfepi)
      dfepdie <- filter(dfepdie, DiedDuringEpisode>0.4)
      OCM3 <- paste0(round(mean(dfepdie$HospicePassOCM3),3) * 100,'%')
      infoBox(value=OCM3, title='OCM-3: % w/ 3+ Hospice Days', color='yellow', icon=icon('hospital-o'))
  })
  output$ValueQuality14Admit <- renderInfoBox( {
      dfepdie <- filterdfepi(dfepi)
      dfepdie <- filter(dfepdie, DiedDuringEpisode>0.4)
      Admit14 <- paste0(round(mean(dfepdie$AdmitLast14Days),3) * 100, '%')
      infoBox(value=Admit14, title='% Admitted Last 14 Days', color='yellow', icon=icon('h-square'))
  })
  output$ValueQualityEOLICU <- renderInfoBox( {
      dfepdie <- filterdfepi(dfepi)
      dfepdie <- filter(dfepdie, DiedDuringEpisode>0.4)
      ICU14 <- paste0(round(mean(dfepdie$ICULast14Days*100),1), '%')
      print(ICU14)
      infoBox(value=ICU14, title='% in ICU in Last 14 Days', color='yellow', icon=icon('bed'))
  })
  output$Quality1 <- renderPlotly( { 
      dfep <- filterdfepi(dfepi)
      xtitle <- titleQuality(input$Qmeasure1)
      print(names(dfep))
      dfb <- aggQuality(df=dfep, var=input$Qmeasure1, grp=input$TOSGroupBy)
      bc <- barVsBenchmark(dfb, var=input$Qmeasure1, grp=input$TOSGroupBy, 
                           color='#7ac043', xlabel=xtitle)
      bc
  })
  output$Quality2 <- renderPlotly( { 
      dfep <- filterdfepi(dfepi)
      xtitle <- titleQuality(input$Qmeasure2)
      dfb <- aggQuality(df=dfep, var=input$Qmeasure2, grp=input$TOSGroupBy)
      bc2 <- barVsBenchmark(dfb, var=input$Qmeasure2, grp=input$TOSGroupBy, 
                           color='#c08943', xlabel=xtitle)
      bc2
  })
  output$Quality3 <- renderPlotly( { 
      dfepdie <- filterdfepi(dfepi)
      dfepdie <- filter(dfepdie, DiedDuringEpisode>0.4)
      xtitle <- titleQuality(input$Qmeasure3)
      dfb <- aggQuality(df=dfepdie, var=input$Qmeasure3, grp=input$TOSGroupBy)
      bc3 <- barVsBenchmark(dfb, var=input$Qmeasure3, grp=input$TOSGroupBy, 
                            color='#437ac0', xlabel=xtitle)
      bc3
  })
  output$Quality4 <- renderPlotly( { 
      dfepdie <- filterdfepi(dfepi)
      dfepdie <- filter(dfepdie, DiedDuringEpisode>0.4)
      xtitle <- titleQuality(input$Qmeasure4)
      print(xtitle)
      dfbd <- aggQuality(df=dfepdie, var=input$Qmeasure4, grp=input$TOSGroupBy)
      bc4 <-  barVsBenchmark(dfbd, var=input$Qmeasure4, grp=input$TOSGroupBy, 
                            color='#8943c0', xlabel=xtitle)
      bc4
  })
  
  
  #### Pricing Tab Outputs ####
  output$IBEpis <- renderInfoBox( {
    dfep <- filterdfepi(dfepi)
    Episodes = nrow(dfep)
    infoBox('# of Episodes', value=Episodes, color='light-blue', fill=TRUE, width=4, 
            icon=icon('hand-left', lib='glyphicon'))
  })
  output$IBPrice <- renderInfoBox( {
    dfep <- filterdfepi(dfepi)
    infoBox('Total Target Price', 
             value=paste('$', round(sum(dfep$BaselinePrice)/1e6,1), 'M'), 
             color='light-blue', fill=TRUE, width=4, icon=icon('bullseye'))
  })  
  output$IBWinsor <- renderInfoBox( {
    dfep <- filterdfepi(dfepi)
    infoBox('$Winsorized Spend', value=paste('$', round(sum(dfep$WinsorizedCost)/1e6,1), 'M'), 
            color='light-blue', fill=TRUE, width=4, icon=icon('usd'))
  })  
  output$priceCTSum <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    dfprice <- dfep %>% group_by(CancerTypeDetailed) %>% summarize(
      Episodes=n(),
      MeanPrice=round(mean(BaselinePrice),0),
      MeanCost=round(mean(WinsorizedCost),0))
    dfprice <- arrange(dfprice, MeanPrice)
    y <- dfprice$CancerTypeDetailed
    buffer = max(dfprice$MeanPrice)* 0.075
    plot_ly(dfprice, x=~MeanPrice, y=~CancerTypeDetailed, orientation='h', type='bar',
            color=~Episodes, 
            text=~paste(CancerTypeDetailed, 
                        '<BR>Episodes: ', Episodes, 
                        '<BR>Mean Price: $', format(MeanPrice, big.mark=",", trim=TRUE),
                        '<BR>Mean Winsorized Cost: $', format(MeanPrice, big.mark=",", trim=TRUE)),
            hoverinfo='text') %>%
      layout(
        title='',
        xaxis=list(title='$ Mean Price', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='', 
                   categoryarray=y, 
                   categoryorder='array',
                   showgrid=FALSE, 
                   showticklabels=TRUE, 
                   tickfont = fontNarrow,
                   ticklen=0,
                   zeroline=FALSE,
                   showline=FALSE),
        margin=list(l=170)
      )  %>%
      add_annotations(xref = 'x', yref = 'y', 
                      x = ~MeanPrice + buffer, y = ~CancerTypeDetailed,
                      text = ~paste('$', format(MeanPrice, big.mark=",", trim=TRUE)),
                      font = fontNarrow, 
                      align='left',
                      showarrow = FALSE) %>%
      colorbar(font=fontNarrow, 
               tickfont=fontNarrow,
               thickness=20,
               title='# of Episodes',
               titlefont=fontNarrow)
  } ) # close PriceCTSum
  output$histPrice <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    # target price distribution
    bp = dfep$BaselinePrice
    maxPrice = max(bp)
    hist <- hist(dfep$BaselinePrice, 
                 breaks=seq(0,maxPrice+5000,5000), 
                 plot=FALSE)
    hist$BarLabel=paste('$', (hist$mids-2500)/1000, '-', (hist$mids+2500)/1000, 'K')
    plot_ly(x = hist$mids, y = hist$counts) %>% 
      add_trace(type='bar', 
                text=~paste('Range: ', hist$BarLabel, '<BR>Episodes: ', hist$counts),
                hoverinfo='text',
                marker=list(color='#7ac043'),
                opacity=0.6) %>%
      layout(
        xaxis=list(title='OCM Target Price', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='# of Episodes', titlefont=font1,
                   showgrid=TRUE, gridwidth=1, gridcolor='#f9f9f9',
                   showticklabels=TRUE, 
                   ticks='outside', tickfont=font2,
                   separatethousands=TRUE),
        bargap=0)
  } )
  output$PriceScatter <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    plot_ly(dfep, x=~WinsorizedCost, y=~BaselinePrice, color=~CancerType) %>%  
      add_trace(type='scatter',
                text=~paste(PatientName, '<BR>', 
                            CancerType, '<br>',
                            'Episode Start Date:', EpiStart,
                            '<br>Target Price: $', format(round(BaselinePrice,0), big.mark=",", trim=TRUE),
                            '<br>Actual Cost: $', format(round(ActualCost,0), big.mark=",", trim=TRUE),
                            '<BR>Winsorized Cost: $', format(round(WinsorizedCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>% 
      layout(
        # title='Target Price v. Episode Cost',
        xaxis=list(title='Winsorized Episode Cost', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='Baseline Target Price', 
                   titlefont=font1,
                   tickfont=font2,
                   showgrid=TRUE),
        legend=list(font=fontNarrow)
      )
  })
  
  #### Pricing Model Tab Outputs ####
  output$PMBase <- renderPlotly( {
      dfpr <- dfpriceCoef %>% filter(BasePrice=='Y')
      dfpr <- arrange(dfpr, Impact)
      y <- dfpr$Variable
      buffer = max(dfpr$Impact)* 0.075
      plot_ly(dfpr, x=~Impact, y=~Variable, orientation='h', type='bar',
              marker=list(color='#437ac0'), 
              text=~paste(Variable, '$', format(round(Impact), big.mark=",", trim=TRUE)),
              hoverinfo='text') %>%
        layout(
          title='',
          xaxis=list(title='$ Base (Starting) Price', 
                     showticklabels=TRUE, 
                     titlefont=font1, 
                     tickfont=font2,
                     separatethousands=TRUE), 
          yaxis=list(title='', 
                     categoryarray=y, 
                     categoryorder='array',
                     showgrid=FALSE, 
                     showticklabels=TRUE, 
                     tickfont = fontNarrow,
                     ticklen=0,
                     zeroline=FALSE,
                     showline=FALSE),
          margin=list(l=160)
        )  %>%
        add_annotations(xref = 'x', yref = 'y', 
                        x = ~Impact + buffer, y = ~Variable,
                        text = ~paste('$', format(round(Impact,0), big.mark=",", trim=TRUE)),
                        font = fontNarrow, 
                        align='left',
                        showarrow = FALSE) 
    }) # close PMBase
  output$PMAgeSex <- renderPlotly( {
    dfpr <- dfpriceCoef %>% filter(AgeSexVariable=='Yes')
    dfpr$Impact <- dfpr$Impact * 100
    buffer = max(dfpr$Impact)* 0.075
    plot_ly(dfpr, x=~Impact, y=~Variable, orientation='h', type='bar',
            marker=list(color='#c0437a'), opacity=0.6, 
            text=~paste(Variable, round(Impact,1), '%'),
            hoverinfo='text') %>%
      layout(
        title='',
        xaxis=list(title='% Increase/Decrease in Target Price', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(title='', 
                   showgrid=FALSE, 
                   showticklabels=TRUE, 
                   tickfont = fontNarrow,
                   ticklen=0,
                   zeroline=FALSE,
                   showline=FALSE),
        margin=list(l=160)
      )  %>%
      add_annotations(xref = 'x', yref = 'y', 
                      x = ~Impact + buffer, y = ~Variable,
                      text = ~paste(round(Impact,1), '%'),
                      font = fontNarrow, 
                      align='left',
                      showarrow = FALSE) 
  }) # close PMBase
  output$PMOther <- renderPlotly( {
    dfpr <- dfpriceCoef %>% filter(AgeSexVariable=='No') %>% filter(BasePrice=='N')
    dfpr$Impact <- dfpr$Impact * 100
    buffer = max(dfpr$Impact)* 0.075
    plot_ly(dfpr, x=~Impact, y=~Variable, orientation='h', type='bar',
            marker=list(color='#c37a43'), opacity=0.6, 
            text=~paste(Variable, round(Impact,1), '%'),
            hoverinfo='text') %>%
      layout(
        title='',
        xaxis=list(title='% Increase/Decrease in Target Price', 
                   showticklabels=TRUE, 
                   titlefont=font1, 
                   tickfont=font2,
                   separatethousands=TRUE), 
        yaxis=list(autorange='reversed',
                   title='', 
                   showgrid=FALSE, 
                   showticklabels=TRUE, 
                   tickfont = fontNarrow,
                   ticklen=0,
                   zeroline=FALSE,
                   showline=FALSE),
        margin=list(l=160)
      )  %>%
      add_annotations(xref = 'x', yref = 'y', 
                      x = ~Impact + buffer, y = ~Variable,
                      text = ~paste(round(Impact,1), '%'),
                      font = fontNarrow, 
                      align='left',
                      showarrow = FALSE) 
  }) # close PMBase
  
  #### Pricing Factors tab outputs ####
  output$TrialsStacked <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    grpVar <- input$TOSGroupBy
    names(dfep)[names(dfep)==grpVar] <- 'YV'
    df1 <- dfep %>% group_by(YV, ClinicalTrialStatus) %>% summarise(RowCount=n())
    df2 <- dfep %>% group_by(YV) %>% summarise(GlobalRowCount=n())
    dfep <- merge(df1, df2, by='YV')
    dfep$RowPercent = dfep$RowCount / dfep$GlobalRowCount * 100
    df2 <- spread(dfep, ClinicalTrialStatus, RowPercent, fill=0)
    # Stacked Bar
    plot_ly(df2, y=~YV, opacity=0.6) %>% 
        # add_trace(type='bar', x=~None, name='No Trial',
        #           hovertext=~paste(YV, "<br>", round(None,1), '% of ', GlobalRowCount, 'Episodes'),
        #           hoverinfo='text') %>% 
        add_trace(type='bar', x=~Diag, name='ICD, Used in OCM',
                  hovertext=~paste(YV, "<br>", round(Diag,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text',
                  text=~paste0(round(Diag,0),'%'), textposition='inside', 
                  textfont=list(color='white', size=12)) %>% 
        add_trace(type='bar', x=~Both, name='Both Methods',
                  hovertext=~paste(YV, "<br>", round(Both,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text',
                  text=~paste0(round(Both,0),'%'), textposition='inside', 
                  textfont=list(color='white', size=12, family='Balto')) %>% 
        add_trace(type='bar', x=~CPT, name='CPT Modifier', 
                  hovertext=~paste(YV, "<br>", round(CPT,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text',
                  text=~paste0(round(CPT,0),'%'), textposition='inside', 
                  insidetextfont=list(color='white', size=18)) %>% 
        layout(    title='',
                 xaxis=list(title='% of Episodes', 
                            showticklabels=TRUE, 
                            titlefont=font1, 
                            tickfont=font2,
                            separatethousands=TRUE), 
                 yaxis=list(title='', 
                            categoryarray=~GlobalRowCount, 
                            categoryorder='array',
                            showgrid=FALSE, 
                            showticklabels=TRUE, 
                            tickfont = fontNarrow,
                            ticklen=0,
                            zeroline=FALSE,
                            showline=FALSE),
                 barmode = 'stack', 
                 margin=list(l=150))
  } )
  output$HCCChart <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    nepis = nrow(dfep)
    dfhc <- merge(dfhcc, dfep, by='EpiNum')
    dfhc <- dfhc %>% group_by(HCC) %>% summarise(Episodes=n())
    dfhc$PercentEpisodes <- dfhc$Episodes/nepis * 100
    dfhc <- merge(dfhc, code_hcc)
    dfhc <- arrange(dfhc, Episodes) %>% tail(20)
    plot_ly(dfhc, y=~HCC_lbl, x=~PercentEpisodes, opacity=0.6, type='bar',
            marker=list(color='#7ac043'),
            text=~paste0(round(PercentEpisodes,1), '%'),
            textposition='auto', textfont=list(color='white'),
            hovertext=~paste(HCC_lbl, 
                        '<br># of Episodes:', Episodes,
                        '<br>% of Episodes:', round(PercentEpisodes,1), '%'),
            hoverinfo='text') %>% 
      layout(    title='',
                 xaxis=list(title='% of Episodes', 
                            showticklabels=TRUE, 
                            titlefont=font1, 
                            tickfont=font2,
                            separatethousands=TRUE), 
                 yaxis=list(title='', 
                            categoryarray=~PercentEpisodes, 
                            categoryorder='array',
                            showgrid=FALSE, 
                            showticklabels=TRUE, 
                            tickfont = fontNarrow,
                            ticklen=0,
                            zeroline=FALSE,
                            showline=FALSE),
                 margin=list(l=240))
  } )
  output$CleanPeriod <-renderPlotly( {
    dfep <- filterdfepi(dfepi)
    dfep$CleanShort = ifelse(dfep$CleanPeriod==2, 100, 0)
    dfep$CleanMid = ifelse(dfep$CleanPeriod==1, 100, 0)
    dfep$CleanLong = ifelse(dfep$CleanPeriod==3, 100, 0)
    grpVar <- input$TOSGroupBy
    names(dfep)[names(dfep)==grpVar] <- 'YV'
    dfep <- dfep %>% group_by(YV) %>% 
      summarise(
        Episodes = n(),
        Short = mean(CleanShort),
        Middle = mean(CleanMid),
        Long=mean(CleanLong)
      )
    dfep <- arrange(dfep, Episodes) 
    plot_ly(dfep, y=~YV, opacity=0.6) %>% 
      add_trace(type='bar', x=~Short, name='Short',
                text=~paste0(round(Short,0),'%'), textposition='auto',
                textfont=list(color='white', size=9),
                hovertext=~paste(YV,
                            '<BR>Short', round(Short,1), '%'),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Middle, name='Middle',
                text=~paste0(round(Middle,0),'%'), textposition='auto',
                textfont=list(color='white', size=9),
                hovertext=~paste(YV,
                            '<BR>2 Months - 2 Years', round(Middle,1), '%'),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Long, name='Long',
                text=~paste0(round(Long,0),'%'), textposition='auto',
                textfont=list(color='white', size=9),
                hovertext=~paste(YV,
                          '<BR>> 730 Days', round(Long,1), '%'),
                hoverinfo='text') %>%
      # add_annotations(xref='x', yref='y', showarrow=FALSE,
      #               x=~Short/2, y=y, 
      #               text=paste(round(dfep[, 'Short'],0), '%'),
      #               font=list(family='Arial', size=12, color='white') ) %>% 
      layout(    title='',
                 xaxis=list(title='% of Episodes', 
                            showticklabels=TRUE, 
                            titlefont=font1, 
                            tickfont=font2,
                            separatethousands=TRUE), 
                 yaxis=list(title='', 
                            categoryarray=~Episodes, 
                            categoryorder='array',
                            showgrid=FALSE, 
                            showticklabels=TRUE, 
                            tickfont = fontNarrow,
                            ticklen=0,
                            zeroline=FALSE,
                            showline=FALSE),
                 barmode='stack',
                 margin=list(l=240))
  } )
  output$TableTrials <- renderDataTable( {
    dfep <- filterdfepi(dfepi)
    dfout <- filter(dfep, ClinicalTrialStatus!='None')
    # Select columns and put them in order
    dfout <- select(dfout, 'PatientName', 'EpiStart', 'BaselinePrice', 'ActualCost', 
                    'ClinicalTrialStatus', 'CancerTypeDetailed',
                    'AttributedPhysicianName')
    outputTable <- OutputTable(dfout, headerCols = c('PatientName'))
    outputTable
  })
  #### Outliers Tab ####
  output$OutlierBar <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfbb <- aggQuality(df=dfep, var='IsOutlier', grp=input$TOSGroupBy)
      ob <- barVsBenchmark(dfbb, var='IsOutlier', grp=input$TOSGroupBy, 
                           color='#c0437a', xlabel='% of Episodes')
      ob
  } )
  output$OutlierTable <- DT::renderDataTable( {
      dfep <- filterdfepi(dfepi)  %>% filter(IsOutlier>=1)
      dfep <- arrange(dfep, desc(OutlierPaid)) %>% 
          select(EpiNum, PatientName, CancerType, AttributedPhysicianName,
                 EpiStart, ActualCost, BaselinePrice, IPTotalPaid, DrugPaid)
      outTab <- OutputTable(dfep, headerCols = c('EpiNum', 'PatientName')) 
      outTab
  })
  #### TOS Tab Outputs ####
  output$TOSStack <- renderPlotly( { 
    grpVar <- input$TOSGroupBy
    srtVar <- input$TOSSortBy
    dfep <- filterdfepi(dfepi)
    dftos <- select(dfep, "Sex", "Age", "DeathDate", "ZipCode", "EpiNum", 'ActualCost',
                    "EpiStart", "EpiEnd", "CancerTypeDetailed", "ReconciliationEligible",
                    "BaselinePrice", "PatientName", "Procs" = "PartBTOSPaidProcedures",
                    "PartBDrugs"="PartBTOSPaidDrugs", "PartBTOSPaidImaging", "PartBTOSPaidLab",
                    "E_M"="PartBTOSPaidE&M", "PartBTOSPaidOther", "PartBTOSPaidChemo", 
                    "RadOnc"="PartBTOSPaidRadOnc", "PartBTOSPaidDME", "PartBTOSPaidEmerg",
                    "Inpat"="IPTotalPaid", "SNFPaid", "HHAPaid", "Hospice"="HospicePaid", "PartDChemoPaid", 
                    "PartDNonChemoPaid", "DMEDrugPaid", "DMENonDrugPaid", "AttributedPhysicianName",
                    "NumberHCCs")
    dftos <- dftos %>% as_tibble() %>% mutate(
      PartBDrugs = PartBDrugs + DMEDrugPaid + PartBTOSPaidChemo,
      Testing = PartBTOSPaidLab + PartBTOSPaidImaging,
      Other = PartBTOSPaidEmerg + PartBTOSPaidDME + PartBTOSPaidOther + DMENonDrugPaid,
      PostAcute = SNFPaid + HHAPaid,
      Orals = PartDChemoPaid + PartDNonChemoPaid,
      CalendarYear = format(EpiStart, '%Y')
    )
    dropCols = c("DMEDrugPaid", "PartBTOSPaidDME", "PartBTOSPaidChemo",
                 "PartBTOSPaidImaging", "PartBTOSPaidEmerg", "PartBTOSPaidLab", "DMENonDrugPaid",
                 "PartBTOSPaidOther", "SNFPaid", "HHAPaid", "PartDChemoPaid", "PartDNonChemoPaid")
    dftos <- dftos %>% dplyr::select(-one_of(dropCols))
    dftos <- dftos %>% replace_na(list(Procs=0, PartBDrugs=0, Inpat=0, Hospice=0, Testing=0, 
                                       Other=0, PostAcute=0, Orals=0, E_M=0, RadOnc=0))
    names(dftos)[names(dftos)==grpVar] <- 'YV'
    dfsum <- dftos %>% group_by(YV) %>% 
      summarize(E_M = mean(E_M),
                PartBDrugs=mean(PartBDrugs),
                Orals=mean(Orals),
                Inpat=mean(Inpat),
                Testing=mean(Testing),
                RadOnc=mean(RadOnc),
                Procs=mean(Procs),
                Hospice=mean(Hospice),
                PostAcute=mean(PostAcute),
                Other=mean(Other),
                Episodes=n(),
                TotalCost=sum(ActualCost),
                MeanCost=mean(ActualCost)
      )
    dfsum <- arrange(dfsum, get(srtVar))# desc(MeanCost))
    plot_ly(dfsum, y=~YV, alpha=0.6) %>%
      add_trace(type='bar', x=~E_M, name='E&M',
                text=~paste(YV,
                            '<BR>E&M', 
                            '<BR>Mean Cost: $', format(round(E_M,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~PartBDrugs, name='Infused Drugs',
                text=~paste(YV,
                            '<BR>Infused and Injected Drugs (Part B)', 
                            '<BR>Mean Cost: $', format(round(PartBDrugs,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Orals, name='Oral Drugs',
                text=~paste(YV,
                            '<BR>Oral Drugs', 
                            '<BR>Mean Cost: $', format(round(Orals,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Inpat, name='Inpatient',
                text=~paste(YV,
                            '<BR>Inpatient (Hosp. Bills only)', 
                            '<BR>Mean Cost: $', format(round(Inpat,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Testing, name='Lab/Imaging',
                text=~paste(YV,
                            '<BR>Lab and Imaging', 
                            '<BR>Mean Cost: $', format(round(Testing,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~RadOnc, name='Radiation',
                text=~paste(YV,
                            '<BR>Radiation Oncology', 
                            '<BR>Mean Cost: $', format(round(RadOnc,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Procs, name='Procedures',
                text=~paste(YV,
                            '<BR>Procedures, including infusion', 
                            '<BR>Mean Cost: $', format(round(Procs,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Hospice, name='Hospice',
                text=~paste(YV,
                            '<BR>Hospice', 
                            '<BR>Mean Cost: $', format(round(Hospice,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~PostAcute, name='Post-Acute',
                text=~paste(YV,
                            '<BR>SNF and Home Health', 
                            '<BR>Mean Cost: $', format(round(PostAcute,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Other, name='Other',
                text=~paste(YV,
                            '<BR>Other Costs', 
                            '<BR>Mean Cost: $', format(round(Other,0), big.mark=",", trim=TRUE),
                            '<BR>Episodes: ', Episodes,
                            '<BR>Mean Episode Cost: $', format(round(MeanCost,0), big.mark=",", trim=TRUE),
                            '<BR>Total Spend: $', format(round(TotalCost,0), big.mark=",", trim=TRUE)),
                hoverinfo='text') %>%
      layout(    title='',
                 xaxis=list(title='$Cost Per Episode', 
                            showticklabels=TRUE, 
                            titlefont=font1, 
                            tickfont=font2,
                            separatethousands=TRUE), 
                 yaxis=list(title='', 
                            categoryarray=~MeanCost, 
                            categoryorder='array',
                            showgrid=FALSE, 
                            showticklabels=TRUE, 
                            tickfont = fontNarrow,
                            ticklen=0,
                            zeroline=FALSE,
                            showline=FALSE),
                 barmode = 'stack', 
                 margin=list(l=170))
  }) # close TOSstack
  output$TOSPie <- renderPlotly( { 
    dfep = filterdfepi(dfepi)
    dfpie <- select(dfep, "Sex", "Age", "DeathDate", "ZipCode", "EpiNum", 
                    "EpiStart", "EpiEnd", "CancerType", "ReconciliationEligible",
                    "BaselinePrice", "PatientName", "Procs" = "PartBTOSPaidProcedures",
                    "PartBDrugs"="PartBTOSPaidDrugs", "PartBTOSPaidImaging", "PartBTOSPaidLab",
                    "E_M"="PartBTOSPaidE&M", "PartBTOSPaidOther", "PartBTOSPaidChemo", 
                    "RadOnc"="PartBTOSPaidRadOnc", "PartBTOSPaidDME", "PartBTOSPaidEmerg",
                    "Inpat"="IPTotalPaid", "SNFPaid", "HHAPaid", "Hospice"="HospicePaid", "PartDChemoPaid", 
                    "PartDNonChemoPaid", "DMEDrugPaid", "DMENonDrugPaid", "AttributedPhysicianName",
                    "NumberHCCs")
    dfpie <- dfpie %>% na.omit()
    dfpie <- dfpie %>% as_tibble() %>% mutate(
      PartBDrugs = PartBDrugs + DMEDrugPaid + PartBTOSPaidChemo,
      Testing = PartBTOSPaidLab + PartBTOSPaidImaging,
      Other = PartBTOSPaidEmerg + PartBTOSPaidDME + PartBTOSPaidOther + DMENonDrugPaid,
      PostAcute = SNFPaid + HHAPaid,
      Orals = PartDChemoPaid + PartDNonChemoPaid
    )
    dropCols = c("DMEDrugPaid", "PartBTOSPaidDME", "PartBTOSPaidChemo",
                 "PartBTOSPaidImaging", "PartBTOSPaidEmerg", "PartBTOSPaidLab", "DMENonDrugPaid",
                 "PartBTOSPaidOther", "SNFPaid", "HHAPaid", "PartDChemoPaid", "PartDNonChemoPaid")
    dfpie <- dfpie %>% dplyr::select(-one_of(dropCols))
    dfpie <- dfpie %>% replace_na(list(Procs=0, PartBDrugs=0, Inpat=0, Hospice=0, Testing=0, 
                                       Other=0, PostAcute=0, Orals=0, E_M=0, RadOnc=0))
    dfpie2 <- gather(dfpie, key=CostType, value=AmountPaid, 
                     Procs, PartBDrugs, Inpat, Hospice, Testing, Other, PostAcute, Orals, E_M, RadOnc)
    dfpie2 <- dfpie2 %>% group_by(CostType) %>% summarize(AmountPaid = mean(AmountPaid))
    colors <- colorRampPalette(brewer.pal(8, 'Set2'))(10)
    plot_ly(dfpie2, labels=~CostType, values=~AmountPaid, type='pie', opacity=.9,
            textposition='inside', textinfo='label+percent',
            text=~paste('$', AmountPaid), hoverinfo='text', rotation=90, 
            textfont=list(color='white', family='Balto', size=13),
            marker=list(color=colors,
                        line=list(width=0)),
            showlegend=FALSE)
  }) # close TOSPie
  
  #### ER Tab Outputs ####
  output$ERDischargeMix <- renderPlotly( {
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    grpVar <- input$ieGrp
    names(dfi)[names(dfi)==grpVar] <- 'YV'
    dfwrk <- dfi %>% group_by(YV) %>% 
      summarise(
        PctHome = mean(SentHome),
        PctAdmit= mean(Admitted),
        PctTrans= mean(Transfer),
        MeanActualCost = mean(ActualCost),
        ERVisits = n()
      )
    dfwrk <- arrange(dfwrk, desc(ERVisits)) %>% head(20)
    plot_ly(dfwrk, y=~YV, opacity=0.6) %>% 
      add_trace(type='bar', x=~PctHome, name='Sent Home',
                text=~paste(YV, "<br>", round(PctHome,1), '% of ', ERVisits, 'ER Visits'),
                hoverinfo='text') %>% 
      add_trace(type='bar', x=~PctAdmit, name='Admitted',
                text=~paste(YV, "<br>", round(PctAdmit,1), '% of ', ERVisits, 'ER Visits'),
                hoverinfo='text') %>% 
      add_trace(type='bar', x=~PctTrans, name='Transferred', 
                text=~paste(YV, "<br>", round(PctTrans,1), '% of ', ERVisits, 'ER Visits'),
                hoverinfo='text') %>% 
      layout(    title='',
                 xaxis=list(title='% of ER Visits', 
                            showticklabels=TRUE, 
                            titlefont=font1, 
                            tickfont=font2,
                            separatethousands=TRUE), 
                 yaxis=list(title='', 
                            autorange='reversed',
                            categoryarray=~ERVisits, 
                            categoryorder='array',
                            showgrid=FALSE, 
                            showticklabels=TRUE, 
                            tickfont = fontNarrow,
                            ticklen=0,
                            zeroline=FALSE,
                            showline=FALSE),
                 barmode = 'stack', 
                 margin=list(l=150))
  } )
  output$ERDischargeTab <- DT::renderDataTable( {
    dfie <- filterdfepi(dfie)
    dfie2 <- filterER(dfie2)
    # grpVar <- input$ieGrp
    names(dfi)[names(dfi)==input$ieGrp] <- 'YV'
    dfwrk <- dfi %>% group_by(YV) %>% 
      summarise(
        PctHome = mean(SentHome)/100,
        PctAdmit= mean(Admitted)/100,
        PctTrans= mean(Transfer)/100,
        MeanActualCost = mean(ActualCost),
        ERVisits = n()
      )
    dfwrk <- arrange(dfwrk, desc(ERVisits)) %>% head(30)
    ertable <- OutputTable(dfwrk, headerCols = c('YV'))
    ertable
  })
  output$ERPatientsTab <- DT::renderDataTable( {
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    dfw <- select(dfi, 'PatientName', 'RevCodeDate', 'CCN_lbl', 'CCS_lbl', 'ClaimType', 
                         'AttributedPhysicianName', 'CancerTypeDetailed')
    dfw <- arrange(dfw, PatientName, RevCodeDate) 
    ertable <- OutputTable(dfw, headerCols = c('PatientName', 'RevCodeDate'))
    ertable
  })
  output$ERSumTab <- DT::renderDataTable( {
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    names(dfi)[names(dfi)==input$ieGrp] <- 'YV'
    dfw <- dfi %>% group_by(YV) %>% 
      summarise(
        ERVisits=n(),
        PctHome=mean(SentHome)/100,
        MeanERPaid=mean(Paid, na.rm=T)
      )
    dfw <- arrange(dfw, desc(ERVisits)) 
    erSumTab <- OutputTable(dfw, headerCols=c('YV'))
    erSumTab    
  })
  output$ValueERVisits <- renderValueBox({
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    ERvis <- nrow(dfi)
    valueBox(value=ERvis, subtitle='# of ER Visits', color='light-blue', 
             icon=icon('ambulance'))
  })
  output$ValueERPaid <- renderValueBox({
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    valueBox(value=paste('$', round(sum(dfi$Paid, na.rm=T)/1e6,1), 'M'), 
             subtitle='$Paid, includes Admit', color='light-blue', icon=icon('money'))
  })
  output$ValueERPaidPerVisit <- renderValueBox({
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    ERvalPaidVisit <- paste('$', round(mean(dfi$Paid, na.rm=T)))
    valueBox(value=ERvalPaidVisit, subtitle='$Paid Per Visit', color='light-blue', 
             icon=icon('usd'))
  })
  output$ValueERPctHome <- renderValueBox({
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    ERvalHome <- round(mean(dfi$SentHome, na.rm=TRUE),1)
    ERvalHome <- paste(as.character(ERvalHome), '%')
    valueBox(value=ERvalHome, subtitle='% Discharged Home', color='light-blue', 
             icon=icon('car'))
  })
  output$ValueERAdmit <- renderValueBox({
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    ERvalAdmit <- round(mean(dfi$Admitted),1)
    ERvalAdmit <- paste(as.character(ERvalAdmit), '%')
    valueBox(value=ERvalAdmit, subtitle='% Admitted', color='light-blue', 
             icon=icon('bed'))
  })
  output$ValueERTrans <- renderValueBox({
    dfie2 <- filterdfepi(dfie)
    dfi <- filterER(dfie2)
    ERvalTrans <- round(mean(dfi$Transfer),1)
    ERvalTrans <- paste(as.character(ERvalTrans), '%')
    valueBox(value=ERvalTrans, subtitle='% Transferred', color='light-blue', 
             icon=icon('road'))
  })
  output$ERMap <- renderLeaflet({
    dfi <- filterdfepi(dfie)
    dfi <- filterER(dfi)
    dfmap <- dfi %>% group_by(CCN) %>% 
      summarise(
        ERVisits=n(),
        PctHome=mean(SentHome),
        PctAdmit=mean(Admitted),
        PctTrans=mean(Transfer),
        PaidPerVisit=mean(Paid, na.rm=TRUE)
      ) %>% filter(ERVisits>=10)
    dfCCNGeo <- read_feather('/AdvAnalytics/Reference/xw_CCN2LatLong.feather')
    dfxwCCN <- read_feather('/AdvAnalytics/Reference/code_CCN.feather')
    dfmap <- merge(dfmap, dfCCNGeo, by='CCN', all.x=TRUE)
    dfmap <- merge(dfmap, dfxwCCN,  by='CCN', all.x=TRUE)
    dfmap$Lab <- paste('<H4>', dfmap$CCN_lbl, 
                       '</H4><BR># of ER Visits: ', dfmap$ERVisits)
    pal <- colorNumeric(palette='RdYlGn', domain=dfmap$PctHome)
    leaflet(data=dfmap) %>% 
      addProviderTiles(providers$OpenMapSurfer.Roads) %>% 
      addCircleMarkers(lng=~Longitude, lat=~Latitude, radius=round(dfmap$ERVisits^.6/2, 1),
                       fillOpacity=0.7, fillColor=~pal(PctHome), weight=0, layerId=~CCN,
                       popup=~paste('<H4>', CCN_lbl, '</H4>',
                                    'ER Visits  : <b>', ERVisits,
                                    '</b><BR>% Sent Home: <b>', round(PctHome,1), '%',
                                    '</b><BR>% Admitted : <b>', round(PctAdmit,1), '%',
                                    '</b><BR>% Transfer : <b>', round(PctTrans,1), '%',
                                    '</b><BR>$ Per Visit: <b>$', round(PaidPerVisit,0))
      )
  })
  
  #### Inpatient Tab Outputs ####
  output$IPPie <- renderPlotly( {
      grpVar <- input$TOSGroupBy
      dfep <- filterdfepi(dfepi)
      names(dfep)[names(dfep)==grpVar] <- 'YV'
      dfpie <- dfep %>% group_by(YV) %>% 
          summarise(
              Admits = sum(IPAdmits),
              Episodes = n()
          ) %>% 
          arrange(desc(Admits))
      dfpie$RowNumber <- seq(1:nrow(dfpie))
      dfpie$GV <- ifelse(dfpie$RowNumber<=10, dfpie$YV, 'Other')
      dfpie2 <- dfpie %>% group_by(GV) %>% 
          summarise(
              Admits = sum(Admits),
              Episodes = sum(Episodes)
          )
      print(head(dfpie2))
      colors <- colorRampPalette(brewer.pal(8, 'Set2'))(11)
      plot_ly(dfpie2, labels=~GV, values=~Admits, type='pie', opacity=.9,
              textposition='inside', textinfo='label+percent',
              hovertext=~paste0('$ of Admits:<b>', Admits, '<br></b>Episodes: <b>', Episodes), hoverinfo='text', rotation=90,
              marker=list(color=colors,
                          line=list(width=0)),
              showlegend=FALSE)
  })
  output$IPAdmitsTable <- DT::renderDataTable( {
      grpVar <- input$TOSGroupBy
      dfep <- filterdfepi(dfepi)
      names(dfep)[names(dfep)==grpVar] <- 'YV'
      dfpie <- dfep %>% group_by(YV) %>% 
          summarise(
              Admits = sum(IPAdmits),
              InpatPaidPerEpisode = mean(IPTotalPaid),
              InpatPaidPerEpisBenchmark = mean(IPTotalPaidBenchmark),
              Episodes = n()
          ) %>% 
          arrange(desc(Admits))
      iptable <- OutputTable(dfpie, headerCols = c('YV'))
      iptable
  })
  output$IPAdmitBenchmark <- renderPlotly({
      grpVar <- input$TOSGroupBy
      dfep <- filterdfepi(dfepi)
      dfben <- aggQuality(dfep, var='IPAdmits', grp=input$TOSGroupBy)
      benchart <- barVsBenchmark(df=dfben, var='IPAdmits', grp=input$TOSGroupBy, color='#7ac043', xlabel='# of Admits')
      benchart
  })
  output$IPBenchTable <- DT::renderDataTable( {
      grpVar <- input$TOSGroupBy
      dfep <- filterdfepi(dfepi)
      dfben <- aggQuality(dfep, var='IPAdmits', grp=grpVar) %>% 
          arrange(desc(Episodes))
      if (max(dfben$IPAdmits)  > 15) {
        dfben$IPAdmits <- round(dfben$IPAdmits /100,3)
      }
      if (max(dfben$Benchmark) > 15) {
        dfben$Benchmark <- round(dfben$Benchmark /100,3)
      }
      dfben <- rename(dfben, AdmitsPerEpisode=IPAdmits)
      bentable <- OutputTable(dfben, headerCols = c(grpVar))
      bentable
  })
  output$IPAdmitsPerEpisode <- renderPlotly({
      dfep <- filterdfepi(dfepi)
      dfAPE <- dfep %>% group_by(IPAdmits) %>% 
          summarise(
              Episodes=n(),
              TotalPaid=sum(IPTotalPaid),
              MeanPaid =mean(IPTotalPaid),
              MeanEpisodePaid = mean(ActualCost),
              MeanEpisodePaidBenchmark = mean(ActualCostBenchmark)
          )
      plot_ly(dfAPE, x=~IPAdmits, y=~Episodes, 
              text=~paste0('n=', Episodes), textposition='auto', 
              insidetextfont=list(family='Balto', size=11, color='white'), 
              hovertext=~paste0('Episodes: <b>', Episodes, 
                                '</b><br>$Paid for Admits: <b>$', format(round(TotalPaid, 0), big.mark=',', trim=TRUE),
                                '</b><br>$Paid per Admission: <b>$', format(round(MeanPaid, 0), big.mark=',', trim=TRUE), 
                                '</b><br>Mean $Paid per Episode: <b>$', format(round(MeanEpisodePaid,0), big.mark=',', trim=TRUE),
                                '</b><br>Benchmark $Paid per Episode: <b>$', format(round(MeanEpisodePaidBenchmark,0), big.mark=',', trim=TRUE)),
              hoverinfo='text',
              marker=list(color='#4a43c0'), opacity=0.6, type='bar') %>% 
          layout(xaxis=list(title='# of Admits in Episode'))
      
  })
  output$IPAPEtable <- DT::renderDataTable( {
      dfep <- filterdfepi(dfepi)
      dfAPE <- dfep %>% group_by(IPAdmits) %>% 
          summarise(
              Episodes=n(),
              TotalPaid=sum(IPTotalPaid),
              MeanPaid =mean(IPTotalPaid),
              MeanEpisodePaid = mean(ActualCost),
              MeanEpisodePaidBenchmark = mean(ActualCostBenchmark)
          )
      dfAPE <- rename(dfAPE, 
                      AdmitCount=IPAdmits,
                      TotalIPPaid=TotalPaid,
                      MeanIPPaid=MeanPaid,
                      PaidPerEpisode=MeanEpisodePaid,
                      BenchmarkPaid = MeanEpisodePaidBenchmark)
      dfAPEtab <- OutputTable(dfAPE, headerCols = c('AdmitCount'))
      dfAPEtab
  })
  output$IPCTbar <- renderPlotly({
      dfep <- filterdfepi(dfepi)
      dfct <- dfep %>% group_by(CancerTypeDetailed) %>% 
          summarise(
              Episodes=n(),
              AdmitsPerEpisode=mean(IPAdmits),
              MeanIPPaid=mean(IPTotalPaid),
              MeanEpisodePaid=mean(ActualCost)
          ) %>% 
          arrange(desc(AdmitsPerEpisode))
      y <- dfct$CancerTypeDetailed
      plot_ly(dfct, x=~AdmitsPerEpisode, y=~CancerTypeDetailed, orientation='h', type='bar',
              color=~Episodes, 
              text=~paste(round(AdmitsPerEpisode,2)),
              textposition='auto', 
              insidetextfont = list(color='white', size=11, family='Balto'),
              hovertext=~paste(CancerTypeDetailed, 
                          '<BR>Episodes: ', Episodes, 
                          '<BR>Mean IP Paid: $', format(MeanIPPaid, big.mark=",", trim=TRUE),
                          '<BR>Mean Episode Cost: $', format(MeanEpisodePaid, big.mark=",", trim=TRUE)),
              hoverinfo='text') %>%
          layout(
              title='',
              xaxis=list(title='Admits Per Episode', 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE), 
              yaxis=list(title='', 
                         autorange='reversed',
                         categoryarray=y, 
                         categoryorder='array',
                         showgrid=FALSE, 
                         showticklabels=TRUE, 
                         tickfont = fontNarrow,
                         ticklen=0,
                         zeroline=FALSE,
                         showline=FALSE),
              margin=list(l=170)
          )
  })
  output$IPCTtab <- DT::renderDataTable ({
      dfep <- filterdfepi(dfepi)
      dfct <- dfep %>% group_by(CancerTypeDetailed) %>% 
          summarise(
              Episodes=n(),
              AdmitsPerEpisode=mean(IPAdmits),
              MeanIPPaid=mean(IPTotalPaid),
              PaidPerEpisode=mean(ActualCost)
          ) %>% 
          arrange(desc(AdmitsPerEpisode))
      dftabct <- OutputTable(dfct, headerCols = c('CancerTypeDetailed'))
      dftabct
  })
  output$IPHospTab <- DT::renderDataTable({ 
      dfip <- merge(dfip, refCCN, all.x=TRUE)
      dfh <- dfip %>% group_by(CCN_lbl, EmergencyAdmit) %>% 
          summarize(
              Admits = n(),
              MeanIPPaid = mean(Paid)
          )
      dfh$EmergencyAdmit = ifelse(dfh$EmergencyAdmit=='Emerg', 'ER', 'NotER')
      dfhw1 <- dfh %>% 
          select(CCN_lbl, EmergencyAdmit, Admits) %>% 
          spread(key=EmergencyAdmit, value=Admits, fill=0) %>% 
          rename(EmergencyAdmits=ER,
                 NonEmergAdmits=NotER)
      dfhw2 <- dfh %>% 
          select(CCN_lbl, EmergencyAdmit, MeanIPPaid) %>% 
          spread(key=EmergencyAdmit, value=MeanIPPaid, fill=0) %>% 
          rename(EmergencyPaid=ER,
                 NonEmergPaid=NotER)
      dfhw <- merge(dfhw1, dfhw2) %>%  arrange(desc(EmergencyAdmits))
      hwtab <- OutputTable(dfhw, headerCols = c('CCN_lbl'))
      hwtab
  })
  output$IPHospBar <- renderPlotly({
      dfip <- merge(dfip, refCCN, all.x=TRUE)
      dfh <- dfip %>% group_by(CCN_lbl, EmergencyAdmit) %>% 
          summarize(
              Admits = n(),
              MeanIPPaid = mean(Paid)
          )
      dfh$EmergencyAdmit = ifelse(dfh$EmergencyAdmit=='Emerg', 'ER', 'NotER')
      dfhw <- dfh %>% 
          select(CCN_lbl, EmergencyAdmit, Admits) %>% 
          spread(key=EmergencyAdmit, value=Admits, fill=0) %>% 
          mutate(TotalAdmits = ER + NotER) %>% 
          arrange(desc(TotalAdmits)) %>% 
          head(15)
      dfhw$BarSort <- seq(1:nrow(dfhw))
      # Stacked Bar
      plot_ly(dfhw, y=~CCN_lbl, opacity=0.6) %>% 
          add_trace(type='bar', x=~ER, name='Emergency',
                    hovertext=~paste('<h4>', CCN_lbl, "</h4><br>Emergency Admits: <b>", ER),
                    hoverinfo='text',
                    text=~paste0(ER), textposition='inside', 
                    textfont=list(color='white', size=12)) %>% 
          add_trace(type='bar', x=~NotER, name='Non-Emerg',
                    hovertext=~paste('<h4>', CCN_lbl, "<br>Non-Emergency Admits: <b>", NotER),
                    hoverinfo='text',
                    text=~paste0(NotER), textposition='inside', 
                    textfont=list(color='white', size=12, family='Balto')) %>% 
          layout(    title='',
                     xaxis=list(title='# of Admissions', 
                                showticklabels=TRUE, 
                                titlefont=font1, 
                                tickfont=font2,
                                separatethousands=TRUE), 
                     yaxis=list(title='', 
                                autorange='reversed',
                                categoryarray=~CCN_lbl, 
                                categoryorder='array',
                                showgrid=FALSE, 
                                showticklabels=TRUE, 
                                tickfont = fontNarrow,
                                ticklen=0,
                                zeroline=FALSE,
                                showline=FALSE),
                     barmode = 'stack', 
                     margin=list(l=165))
  })
  output$IPPivotDiag <- renderRpivotTable({
      dfip <- merge(dfip, refCCN, all.x=TRUE)
      dfip <- select(dfip, CCN_lbl, CCS_lbl, EmergencyAdmit, Sex, CancerType, DRG_lbl, Paid, LOS, ICUDays)
      rpivotTable(data=dfip, rows='CCS_lbl', cols='EmergencyAdmit',
                  values='Paid', aggregatorName = 'Count', rendererName='Table',
                  width="100%", height="620px")
  })
  
  #### Drugs Output Tab ####
  output$DrugVsBenchmark <- renderPlotly({
    grpVar <- input$TOSGroupBy
    dfep <- filterdfepi(dfepi)
    names(dfep)[names(dfep)==grpVar] <- 'YV'
    dfw <- dfep %>% group_by(YV) %>% 
      summarize(
        Episodes=n(),
        MeanPaid=mean(DrugPaid, na.rm=T),
        MeanPaidBenchmark=mean(DrugPaidBenchmark, na.rm=T)
      ) %>% filter(Episodes>=10) %>% arrange(desc(Episodes))
    plot_ly(dfw, y=~YV, opacity=0.7, height='510px') %>% 
      add_trace(x=~MeanPaid, name='$Actual', type='bar', marker=list(color='#7ac043'),
                hovertext=~paste(YV, 
                                 '<br>', Episodes, 'Episodes',
                                 '<br>Actual $', round(MeanPaid,0), ', Benchmark $', round(MeanPaidBenchmark,0)),
                hoverinfo='text') %>% 
      add_trace(x=~MeanPaidBenchmark, name='$Benchmark', marker=list(color='#cbe6b5'), type='bar',
                hovertext=~paste(YV, 
                                 '<br>', Episodes, 'Episodes',
                                 '<br>Actual $', round(MeanPaid,0), ', Benchmark $', round(MeanPaidBenchmark,0)),
                hoverinfo='text') %>% 
      add_annotations(text = ~paste('$', round(MeanPaid,0), '<BR>$', round(MeanPaidBenchmark,0)),
                      x=~MeanPaid, y=~YV,
                      xref="x", yref='y', showarrow=FALSE, align='left', 
                      xanchor='left', valign='top',
                      font=list(family='PT Sans Narrow',
                                size=9, 
                                color='#878787')) %>% 
      layout(    title='',
                 xaxis=list(title='$Paid Per Episode', 
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
                 margin=list(l=170, t=0, b=0),
                 legend=list(orientation='h', font=fontNarrow, x=0, xanchor='center',
                             y=1.08, yanchor='auto')
      )
  })
  output$DrugRegimenPie <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfpie3a <- dfep %>% group_by(Regimen_lbl) %>% 
        summarize(
          Episodes=n(),
          MeanCost=mean(ActualCost, na.rm=TRUE)
        ) 
      plot_ly(dfpie3a, labels=~Regimen_lbl, values=~Episodes, type='pie', opacity=.9,
              textposition='inside', textinfo='label+percent',
              hovertext=~paste(Regimen_lbl,
                               '<BR>', Episodes, 'Episodes',
                               '<BR>Mean Cost: $', round(MeanCost,0)), 
              hoverinfo='text', rotation=90,
              marker=list(color=colors,
                          line=list(width=0)),
              showlegend=FALSE) %>% 
        layout(title='')
  })
  output$DrugRegimenTable <- DT::renderDataTable( {
    dfep <- filterdfepi(dfepi)
    dfreg <- dfep %>% group_by(Regimen_lbl) %>% 
      summarize(
        Episodes=n(),
        MeanEpisodeCost=mean(ActualCost, na.rm=TRUE),
        SDEpisodeCost = sd(ActualCost, na.rm=TRUE),
        MeanCostSavings=mean(Savings, na.rm=TRUE),
        AdmitsPerEpisode=mean(IPAdmits, na.rm=TRUE),
        ERVisitsPerEpisode=mean(ERVisits, na.rm=TRUE)
      ) %>% 
      arrange(desc(Episodes))
    mytab <- OutputTable(dfreg, headerCols=c('Regimen_lbl'))
    mytab
  })
  output$DrugMostExpensiveTable <- DT::renderDataTable( {
    dfep <- filterdfepi(dfepi)
    dfmed <- dfep %>% group_by(MostExpensiveDrug) %>% 
      summarise(
        Episodes=n(),
        MeanDrugPaid=mean(DrugPaid),
        MeanEpisodeCost=mean(ActualCost),
        MeanCostSavings=mean(Savings),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits)
      ) %>% arrange(desc(Episodes))
    dfmedtab <- OutputTable(dfmed, headerCols=c('Episodes'))
    dfmedtab
  })
  output$DrugMostExpensiveChemoTable <- DT::renderDataTable( {
    dfep <- filterdfepi(dfepi)
    dfchem <- dfep %>% group_by(MostExpensiveAntiCancerDrug) %>% 
      summarise(
        Episodes=n(),
        TotalPaid=sum(DrugPaid),
        MeanDrugPaid=mean(DrugPaid),
        MeanEpisodeCost=mean(ActualCost),
        MeanCostSavings=mean(Savings),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits)
      ) %>% arrange(desc(TotalPaid))
    dfchemtab <- OutputTable(dfchem, headerCols=c('Episodes'))
    dfchemtab
  })
  #### End of Life tab ####
  output$ValueEOLDeaths <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, DiedDuringEpisode==1)
      Deaths = nrow(dfep)
      valueBox(value=Deaths, subtitle='# of Deaths', color='light-blue', icon=icon('hand-left', lib='glyphicon'))
  })
  output$ValueEOLPctHospice <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, DiedDuringEpisode==1)
      dfep$HasHosp <- ifelse(dfep$HospiceLOS==0, 0, 1)
      PctHosp = mean(dfep$HasHosp)
      valueBox(value=paste0(round(100.0*PctHosp, 1), '%'), subtitle='% with Hospice', color='light-blue', 
                            icon=icon('hospital-o'))
  })
  output$ValueEOLPassOCM3 <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, DiedDuringEpisode==1)
      dfep$HasHosp3 <- ifelse(dfep$HospiceLOS>=3, 1, 0)
      PctHosp3 = mean(dfep$HasHosp3)
      valueBox(value=paste0(round(100.0*PctHosp3, 1), '%'), subtitle='Pass OCM-3 (3+ Days)', color='light-blue', 
               icon=icon('thumbs-up'))
  })
  output$ValueEOLHospicePaid <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, DiedDuringEpisode==1)
      MeanHospPaid = mean(dfep$HospicePaid)
      valueBox(value=paste0('$', round(MeanHospPaid,0)), subtitle='$Hospice Per Patient', 
               color='light-blue', icon=icon('usd'))
  })
  output$ValueEOLHospiceSavings0 <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, DiedDuringEpisode==1)
      dfep = filter(dfep, HospiceLOS==0)
      dfep$Savings = dfep$BaselinePrice - dfep$WinsorizedCost
      Savings0 = mean(dfep$Savings)
      valueBox(value=paste0('$', round(Savings0,0)), subtitle='Avg. Savings w/o Hospice', 
               color='light-blue', icon=icon('money'))
  })
  output$ValueEOLHospiceSavings3 <- renderValueBox( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, DiedDuringEpisode==1)
      dfep = filter(dfep, HospiceLOS>=3)
      dfep$Savings = dfep$BaselinePrice - dfep$WinsorizedCost
      Savings3 = mean(dfep$Savings)
      valueBox(value=paste0('$', round(Savings3,0)), subtitle='Avg. Savings w/ 3+ Days', 
               color='light-blue', icon=icon('money'))
  })
  output$EOLDeathCount <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1)
      HBarCount(dfep, grp=input$TOSGroupBy, color='#44597e', xtitle='# of Deaths')
  })
  output$EOLDeathCountTab <- DT::renderDataTable( {
      grpVar = input$TOSGroupBy
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1)
      names(dfep)[names(dfep)==grpVar] = 'gV'
      dfw <- dfep %>% group_by(gV) %>% 
          summarise(Deaths = n()) %>% 
          arrange(desc(Deaths))
      print(names(dfw))
      names(dfw)[names(dfw)=='gV'] <- grpVar
      print(names(dfw))
      tab12 <- OutputTable(dfw, headerCols = c(grpVar))
      tab12
  })
  output$EOLDeathCountPie <- renderPlotly( {
      grpVar = input$TOSGroupBy
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1)
      names(dfep)[names(dfep)==grpVar] = 'gV'
      dfpie <- dfep %>% group_by(gV) %>% 
          summarize(Count=n()) %>% 
          arrange(desc(Count))
      dfpie$RowCount = seq(1:nrow(dfpie))
      dfpie$gV <- ifelse(dfpie$RowCount<8, dfpie$gV, 'Other')
      dfpie2 <- dfpie %>% group_by(gV) %>% 
          summarize(Deaths=sum(Count)) %>% 
          arrange(desc(Deaths))
      colors <- colorRampPalette(brewer.pal(8, 'Pastel1'))(8)
      plot_ly(dfpie2, labels=~gV, values=~Deaths, type='pie', opacity=.7,
              textposition='inside', textinfo='label+percent', textfont=list(color='white'),
              text=~paste('Deaths:', Deaths), hoverinfo='text', rotation=90,
              marker=list(color=colors,
                          line=list(width=0)),
              showlegend=FALSE)
  })
  output$EOLDeathPatientList <- DT::renderDataTable( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1)
      dfep$OCM3Pass <- ifelse(dfep$HospiceLOS>=3,'Yes', "No'")
      dfep$Savings = dfep$BaselinePrice - dfep$WinsorizedCost
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1) %>% 
          select(PatientName, CancerTypeDetailed, AdmitLast14Days, ICULast14Days, OCM3Pass, HospiceLOS,
                 Savings)
      dfep$AdmitLast14Days <- ifelse(dfep$AdmitLast14Days == 1, 'Yes', 'No')
      dfep$ICULast14Days <- ifelse(dfep$ICULast14Days==1, 'Yes', 'No')
      dfpat <- OutputTable(dfep, headerCols = c('PatientName'))
      dfpat
  })
  output$EOLHospiceDistBar <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1)
      names(dfep)[names(dfep)==input$TOSGroupBy] <- 'gV'
      dfep$HospiceDaysGroup <- cut(dfep$HospiceLOS, 
                                   breaks=c(-Inf, 1, 3, 10, Inf),
                                   labels=c('Days.0', 'Days.1_2', 'Days.3_9', 'Days.10'),
                                   right=FALSE)
      dfw <- dfep %>% group_by(gV, HospiceDaysGroup) %>% 
          summarize(Patients=n()) %>% 
          spread(key=HospiceDaysGroup, value=Patients, fill=0)
      # Add columns 2-5
      dfw$Patients = rowSums(dfw[, 2:5])
      dfw <- filter(dfw, Patients>=3)
      dfw <- arrange(dfw, desc(Patients))
      dfw$RowNum <- seq(1:nrow(dfw))
      # Convert to percent of row total
      for(c in seq(2,5)) {
          dfw[, eval(c)] <- dfw[, eval(c)] / dfw$Patients * 100.0
      }
      dfw$gV <- paste0(dfw$gV, ' (N=', dfw$Patients, ')')
      print(head(dfw))
      # dfw$RowNum <- seq(1, nrow(dfw))
      # Specify chart ####
      plot_ly(dfw, y=~gV, opacity=0.6) %>% 
          add_trace(type='bar', x=~Days.0, name='0 Days',
                hovertext=~paste0('', gV, '<br>', round(Days.0,1), '% of ', Patients, ' patients'),
                hoverinfo='text',
                marker=list(color='#8943c0'),
                text=~paste0(round(Days.0,0),'%'), textposition='inside', 
                textfont=list(color='white', size=12)) %>% 
          add_trace(type='bar', x=~Days.1_2, name='1-2 Days',
                    hovertext=~paste0('', gV, '<br>', round(Days.1_2,1), '% of ', Patients, ' patients'),
                    hoverinfo='text',
                    marker=list(color='#c08943'),
                    text=~paste0(round(Days.1_2,0),'%'), textposition='inside', 
                    textfont=list(color='white', size=12)) %>% 
          add_trace(type='bar', x=~Days.3_9, name='3-9 Days',
                    hovertext=~paste0('', gV, '<br>', round(Days.3_9,1), '% of ', Patients, ' patients'),
                    hoverinfo='text',
                    marker=list(color='#437ac0'),
                    text=~paste0(round(Days.3_9,0),'%'), textposition='inside', 
                    textfont=list(color='white', size=12)) %>% 
          add_trace(type='bar', x=~Days.10, name='10+ Days',
                    hovertext=~paste0('', gV, '<br>', round(Days.10,1), '% of ', Patients, ' patients'),
                    hoverinfo='text',
                    marker=list(color='#7ac043'),
                    text=~paste0(round(Days.10,0),'%'), textposition='inside', 
                    textfont=list(color='white', size=12)) %>% 
          layout(    title='Days of Hospice Care by Physician',
                     xaxis=list(title='% of Deaths', 
                                showticklabels=TRUE, 
                                titlefont=font1, 
                                tickfont=font2,
                                separatethousands=TRUE), 
                     yaxis=list(title='',
                                autorange='reversed',
                                categoryarray=~RowNum, 
                                categoryorder='array',
                                showgrid=FALSE, 
                                showticklabels=TRUE, 
                                tickfont = fontNarrow,
                                ticklen=0,
                                zeroline=FALSE,
                                showline=FALSE),
                     barmode = 'stack', 
                     margin=list(l=160))
      
  })
  output$EOLHospiceDistTab <- DT::renderDataTable( {
      dfep <- filterdfepi(dfepi)
      dfep <- filter(dfep, dfep$DiedDuringEpisode==1)
      names(dfep)[names(dfep)==input$TOSGroupBy] <- 'gV'
      dfep$HospiceDaysGroup <- cut(dfep$HospiceLOS, 
                                   breaks=c(-Inf, 1, 3, 10, Inf),
                                   labels=c('0 Days', '1-2 Days', '3-9 Days', '10+ Days'),
                                   right=FALSE)
      dfw <- dfep %>% group_by(gV, HospiceDaysGroup) %>% 
          summarise(
              Patients=n(),
              MeanHospicePaid=mean(HospicePaid),
              MeanHospicePaidBenchmark=mean(HospicePaidBenchmark),
              MeanEpisodeCost=mean(ActualCost),
              MeanEpisodeCostBenchmark=mean(ActualCostBenchmark)
          ) 
      dfw1 <- dfw %>% group_by(gV) %>% summarise(TotalPatients=sum(Patients))
      dfw <- merge(dfw, dfw1, by='gV') %>% arrange(desc(TotalPatients), gV, HospiceDaysGroup)
      dfw$PctPatients = round(dfw$Patients / dfw$TotalPatients * 1.0, 3)
      # Put columns in order
      dfw <- select(dfw, gV, HospiceDaysGroup, Patients, PctPatients, MeanHospicePaid,
                    MeanHospicePaidBenchmark, MeanEpisodeCost, MeanEpisodeCostBenchmark)
      # Change the column name back
      names(dfw)[names(dfw)=='gV'] <- input$TOSGroupBy
      HospiceTab <- OutputTable(dfw, headerCols = c(input$TOSGroupBy, 'HospiceDaysGroup'))
      HospiceTab
  })
  output$EOLQualityAdmit <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfd <- filter(dfep, DiedDuringEpisode==1)
      names(dfd)[names(dfd)==input$TOSGroupBy] <- 'gV'
      dfqa <- dfd %>% group_by(gV) %>% 
          summarise(
              PctAdmitted=mean(AdmitLast14Days*100),
              Deaths=n()
          ) %>% 
          arrange(desc(PctAdmitted))
      dfqa$RowNum = seq(1:nrow(dfqa))
      dfqa$gV <- paste0(dfqa$gV, ' (N=', dfqa$Deaths, ')')
      dfqa <- filter(dfqa, Deaths>=3)
      # Build Chart ----
      barChart <- plot_ly(dfqa, x=~PctAdmitted, y=~gV, orientation='h', type='bar', opacity=0.7, 
                          marker=list(color='teal'), text=~paste0(round(PctAdmitted,0), '%'), textposition='auto',
                          insidetextfont=list(size=13, family='Balto', color='white')) %>% 
          layout(
              title='',
              xaxis=list(title='% Patients Admitted in Last 14 Days of Life', 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE), 
              yaxis=list(title='',
                         autorange='reversed',
                         categoryarray=~PctAdmitted, 
                         categoryorder='array',
                         showgrid=FALSE, 
                         showticklabels=TRUE, 
                         tickfont = fontNarrow,
                         ticklen=0,
                         zeroline=FALSE,
                         showline=FALSE),
              margin=list(l=170)
          )
      barChart
  })
  output$IBEOLQualityAdmit <- renderInfoBox( {
      dfep <- filterdfepi(dfepi)
      dfd <- filter(dfep, DiedDuringEpisode==1)
      pctAdmit = round(mean(dfd$AdmitLast14Days*100),1)
      infoBox('Admitted Last 14 Days', value=paste0(pctAdmit, '%'), color='teal', fill=TRUE, width=4, 
              icon=icon('h-square'))
      
  })
  output$EOLQualityICU <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfd <- filter(dfep, DiedDuringEpisode==1)
      names(dfd)[names(dfd)==input$TOSGroupBy] <- 'gV'
      dfqa <- dfd %>% group_by(gV) %>% 
          summarise(
              PctAdmitted=mean(ICULast14Days*100),
              Deaths=n()
          ) %>% 
          arrange(desc(PctAdmitted))
      dfqa$gV <- paste0(dfqa$gV, ' (N=', dfqa$Deaths, ')')
      dfqa <- filter(dfqa, Deaths>=3)
      dfqa$RowNum = seq(1:nrow(dfqa))
      # Build Chart ----
      barChart2 <- plot_ly(dfqa, x=~PctAdmitted, y=~gV, orientation='h', type='bar', opacity=0.5, 
                          marker=list(color='#00b3b3'), text=~paste0(round(PctAdmitted,0), '%'), textposition='auto',
                          insidetextfont=list(size=13, family='Balto', color='white')) %>% 
          layout(
              title='',
              xaxis=list(title='% Patients Admitted to ICU in Last 14 Days', 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE), 
              yaxis=list(title='',
                         autorange='reversed',
                         categoryarray=~Deaths, 
                         categoryorder='array',
                         showgrid=FALSE, 
                         showticklabels=TRUE, 
                         tickfont = fontNarrow,
                         ticklen=0,
                         zeroline=FALSE,
                         showline=FALSE),
              margin=list(l=170)
          )
      barChart2
  })
  output$IBEOLQualityICU <- renderInfoBox( {
      dfep <- filterdfepi(dfepi)
      dfd <- filter(dfep, DiedDuringEpisode==1)
      pctICU = round(mean(dfd$ICULast14Days*100,1))
      infoBox('ICU in Last 14 Days', value=paste0(round(mean(dfd$ICULast14Days*100),1), '%'),
              color='aqua', fill=TRUE, width=4, icon=icon('bed'))
      
  })
  output$EOLQualityChemo <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      dfd <- filter(dfep, DiedDuringEpisode==1)
      names(dfd)[names(dfd)==input$TOSGroupBy] <- 'gV'
      dfd$ChemoLast14Days = sign(dfd$PartBLast14PaidChemo)
      dfqa <- dfd %>% group_by(gV) %>% 
          summarise(
              PctChemo=mean(ChemoLast14Days*100),
              Deaths=n()
          ) %>% 
          arrange(desc(Deaths))
      dfqa$RowNum = seq(1:nrow(dfqa))
      # Build Chart ---
      barChart3 <- plot_ly(dfqa, x=~PctChemo, y=~gV, orientation='h', type='bar', opacity=0.5, 
                           marker=list(color='green'), text=~paste0(round(PctChemo,0), '%'), textposition='auto',
                           insidetextfont=list(size=13, family='Balto', color='white')) %>% 
          layout(
              title='',
              xaxis=list(title='% Patients Rec. Chemo Last 14 Days', 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE), 
              yaxis=list(title='',
                         autorange='reversed',
                         categoryarray=~Deaths, 
                         categoryorder='array',
                         showgrid=FALSE, 
                         showticklabels=TRUE, 
                         tickfont = fontNarrow,
                         ticklen=0,
                         zeroline=FALSE,
                         showline=FALSE),
              margin=list(l=170)
          )
      barChart3
  })
  output$IBEOLQualityChemo <- renderInfoBox( {
      dfep <- filterdfepi(dfepi)
      dfd <- filter(dfep, DiedDuringEpisode==1)
      dfd$ChemoLast14Days = sign(dfd$PartBLast14PaidChemo)*100
      #pctChemo = round(mean(dfd$ChemoLast14Days,1))
      infoBox('Chemo in Last 14 Days', value=paste0(round(mean(dfd$ChemoLast14Days),1), '%'), 
              color='green', fill=TRUE, width=4, icon=icon('thermometer'))
  })
  dfeol <- reactive( {
      dfdied <- read_feather('Output/dfdiedEOL.feather')
      dfdied <- filterdfepi(dfdied)
      dfdied <- dfdied %>% 
          select('EpiNum', 'HospiceDaysGroup',  
                 starts_with('TotalPaid'))
      dfdied2 <- dfdied %>% 
          melt(key.name='NumDays', value.name='Paid', 
               id.var=c('EpiNum', 'HospiceDaysGroup')) %>% 
          arrange(EpiNum, desc(variable))
      dfdied2$NumberOfDays <- sub('TotalPaid', '', dfdied2$variable) 
      dfdied2$NumberOfDays <- sub('Days', '', dfdied2$NumberOfDays)
      dfdied2$NumberOfDays <- strtoi(dfdied2$NumberOfDays)
      dfeol <- dfdied2
      return(dfdied2)
  })
  output$EOLCostChart <- renderPlotly( {
      dfeol <- dfeol()
      dfeol <- dfeol %>% group_by(HospiceDaysGroup, NumberOfDays) %>% 
          summarise(MeanPaid=mean(Paid))  
      dfeol <- dfeol %>% spread(key=HospiceDaysGroup, value=MeanPaid)
      names(dfeol)=c('NumberOfDays', 'Days0', 'Days1_2', 'Days3_9', 'Over10')
      dfeol$NumDays = paste0(dfeol$NumberOfDays-2, '-', dfeol$NumberOfDays, ' Days' )
      dfeol <- arrange(dfeol, (desc(NumberOfDays)))
      dfeol$Days0 <- cumsum(dfeol$Days0)
      dfeol$Days1_2 <- cumsum(dfeol$Days1_2)
      dfeol$Days3_9 <- cumsum(dfeol$Days3_9)
      dfeol$Over10 <- cumsum(dfeol$Over10)
      eolc <- plot_ly(dfeol, x=~NumberOfDays) %>% 
          add_trace(y=~Days0, type='scatter', mode='line', name='No Hospice',
                    text=~paste0(NumDays, '<BR>$', round(Days0,0)),
                    line=list(color='#8943c0'),
                    hoverinfo='text') %>% 
          add_trace(y=~Days1_2, type='scatter', mode='line', name='1-2 Days',
                    text=~paste0(NumDays, '<BR>$', round(Days1_2,0)),
                    line=list(color='#c08943'),
                    hoverinfo='text') %>% 
          add_trace(y=~Days3_9, type='scatter', mode='line', name='3-9 Days',
                    text=~paste0(NumDays, '<BR>$', round(Days3_9,0)),
                    line=list(color='#437ac0'),
                    hoverinfo='text') %>% 
          add_trace(y=~Over10, type='scatter', mode='line', name='10+ Days',
                    text=~paste0(NumDays, '<BR>$', round(Over10,0)),
                    line=list(color='#7ac043'),
                    hoverinfo='text') %>% 
          layout(
              xaxis=list(title='Ranges of Days Before Death', 
                         showticklabels=TRUE, 
                         showgrid=FALSE,
                         tick0=2,
                         dtick=3,
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE,
                         autorange='reversed'), 
              yaxis=list(title='Cumulative $Paid in Last 30 Days',
                         showgrid=TRUE, 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont = fontNarrow,
                         ticklen=0,
                         zeroline=FALSE,
                         showline=FALSE,
                         rangemode='tozero'),
              title=''
          )
      eolc
  })
  #### Simulate results ####
  dfboot <- reactive( {
      dfep <- filterdfepi(dfepi)
      dfep2 <- select(dfep, WinsorizedCost, BaselinePrice)
      dfep2$Savings <- dfep2$BaselinePrice - dfep2$WinsorizedCost
      sampsize = input$BootSize
      vecPrice = c()
      vecCost  = c()
      vecSave  = c()
      sampsize=input$BootSize
      for(s in 1:250) {
          dfs <- sample_n(dfep, sampsize, replace=TRUE)
          vecPrice <- append(x=vecPrice, mean(dfs$BaselinePrice))
          vecCost  <- append(x=vecCost,  mean(dfs$WinsorizedCost))
          vecSave  <- append(x=vecSave,  mean(dfs$Savings) / mean(dfs$BaselinePrice) * 100)
      }
      listVec <- list(Price=vecPrice, Cost=vecCost, Savings=vecSave)
      dfboot <- as_tibble(listVec)
      #return(df)
  } )
  # Create volatility reports
  output$BootBar <- renderPlotly( {
      dfep <- filterdfepi(dfepi)
      meanSave <- ( mean(dfep$BaselinePrice) - mean(dfep$WinsorizedCost) ) / 2
      dfb <- head(dfboot(), 25)
      dfb$x <- seq(1:nrow(dfb))
      dfb$BarColor = ifelse(dfb$Savings<0, 'red', 'green')
      bootBar <- plot_ly(dfb, x=~x) %>% 
          add_trace(type='bar', y=~Savings, opacity=0.7,
                    text=~paste0(round(Savings,1),'%'),
                    textposition='auto', textfont=list(family='Balto', color='white', size=12),
                    hoverinfo='text', marker=list(color=~BarColor), name='Savings') %>% 
          add_trace(type='scatter', mode='lines+markers', y=~Price, opacity=0.4, line=list(color='#7ac043', width=1),
                    marker=list(color='#7ac043'), line=list(width=2), name='Price', yaxis='y2') %>% 
          add_trace(type='scatter', mode='lines+markers', y=~Cost,  opacity=0.4, line=list(color='#c0437a', width=1), 
                    marker=list(color='#c0437a'), line=list(width=2), name='Cost',  yaxis='y2') %>% 
          layout(title='Bootstrap Simulation Results',
                 xaxis=list(title=''),
                 yaxis=list(title='% Savings/Loss'),
                 yaxis2=list(tickfont = list(color = "#7ac043"),
                            overlaying = "y",
                            side = "right",
                            title = "Average Cost and Price"))
      bootBar
  })
  output$BootHist <- renderPlotly( {
      dfb <- dfboot()
      hist <- hist(dfb$Savings, 
                   breaks=seq(round(min(dfb$Savings) - 0.5, 0), round(max(dfb$Savings) + 0.5, 0)), 
                   plot=FALSE)
      hist$BarLabel=paste0((hist$mids-0.5), '-', (hist$mids+0.5), '%')
      plot_ly(x = hist$mids, y = hist$counts) %>% 
          add_trace(type='bar', 
                    text=hist$counts, textposition='auto', textfont=list(family='Balto', size=13, color='white'),
                    hovertext=~paste('Range: ', hist$BarLabel, '<BR>Reconciliations: ', hist$counts),
                    hoverinfo='text',
                    marker=list(color='#7ac043'),
                    opacity=0.6) %>%
          layout(
              xaxis=list(title='% Savings/Losses', 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE), 
              yaxis=list(title='# of Reconciliations', titlefont=font1,
                         showgrid=TRUE, gridwidth=1, gridcolor='#f9f9f9',
                         showticklabels=TRUE, 
                         ticks='outside', tickfont=font2,
                         separatethousands=TRUE),
              bargap=0)
  })
  output$BootControlChart <- renderPlotly( {
      dfb <- dfboot()
      dfb$Saved <- dfb$Savings / 100 * dfb$Price
      dfb$x = seq(1:nrow(dfb))
      dfb <- head(dfb, 50)
      meanSave <- mean(dfb$Saved)
      upperBound <- meanSave + 2 * sd(dfb$Saved)
      lowerBound <- meanSave - 2 * sd(dfb$Saved)
      bootControl <- plot_ly(dfb, x=~x) %>% 
          add_trace(type='scatter', mode='lines+markers', y=~Saved, opacity=0.7, line=list(color='#7ac043', width=1),
                    marker=list(color='#7ac043'), line=list(width=2), name='Price') %>% 
          add_trace(type='scatter', mode='lines', y=meanSave,  opacity=0.5, line=list(color='grey', width=1), 
                    marker=list(color='transparent'), line=list(width=2), name='Avg. $Saved/Lost') %>% 
          add_trace(type='scatter', mode='lines', y=upperBound,  opacity=0.3, line=list(color='grey', width=1), 
                    marker=list(color='transparent'), line=list(width=2), name='Upper Control Bound') %>% 
          add_trace(type='scatter', mode='lines', y=lowerBound,  opacity=0.3, line=list(color='grey', width=1), 
                    fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
                    marker=list(color='transparent'), line=list(width=2), name='Lower Control Bound') %>% 
          layout(title='Control Chart of $Saved/Lost',
                 xaxis=list(title='Simulation Number',
                            zeroline=FALSE),
                 yaxis=list(title='$ Saved/Lost',
                            zeroline=FALSE,
                            tickfont = list(color = "#7ac043")))
      bootControl
  
  })
  
} # close server function

shinyApp(ui, server)

