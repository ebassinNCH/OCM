## app.R ##
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

setwd('/AdvAnalytics/OCM/CCSI/BaselineUpdated')
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
dfie  <- read_feather('Output/dfie.feather')
dfie <- distinct(dfie, PatientName, EpiNum, CCN, RevCodeDate, .keep_all=TRUE)
dfie$SentHome <- ifelse(dfie$ClaimType=='Sent Home', 100, 0)
dfie$Admitted <- ifelse(dfie$ClaimType=='Admitted', 100, 0)
dfie$Transfer <- ifelse(dfie$ClaimType=='Transfer', 100, 0)
dfpriceCoef <- read_feather('/AdvAnalytics/OCM/Reference/ref_pricingCoefficients.feather')
dfhcc <- read_feather('Output/dfhcc.feather')
code_hcc <- read_feather('/AdvAnalytics/Reference/code_HCC.feather')
load('/AdvAnalytics/Reference/shapefile_zipcode.RData')
load('/AdvAnalytics/Reference/shapefile_county.RData')
xwZip2County <- read_feather('/AdvAnalytics/Reference/xw_ZipCode2County.feather')
#codeCounty <- read_feather('/AdvAnalytics/Reference/code_FIPSCounty.feather')

potentialdocs <- sort(unique(dfepi$AttributedPhysicianName))
potentialCT <- sort(unique(dfepi$CancerTypeDetailed))
potentialHosp <- sort(unique(dfie$CCN_lbl))
date1 <- min(dfepi$EpiStart)
date2 <- max(dfepi$EpiStart)
dfdied <- filter(dfepi, (DeathDate>=EpiStart) & (DeathDate<=EpiEnd))
maxActual = max(dfepi$ActualCost)
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
OutputTable <- function(df, headerCols) {
  # Like a Python dict, this is a list with mappings of R column names to friendly names.  
  #    If a column name, appears in the list, we use the friendly name.
  #    If the column name does not appear in the list, the column names is used.  
  #    If the list contains a column name that is not in the data frame, that list element is 
  #        unused, so extra list elements are ignored.
  mapCols <- list('EpiNum' = 'Episode #', 
                  'YV' = 'Your Variable',
                  'PatientName'='Patient Name',
                  'Actual Cost'='$Actual',
                  'MostExpensiveDrug'='Most Expensive Agent',
                  'CancerTypeDetailed'='Cancer Type',
                  'AttributedPhysicianName'='Attributed Provider',
                  'EpiStart'='Episode Start',
                  'IPTotalPaid'='$Inpat',
                  'DrugPaid'='$Drugs',
                  'MeanDrugPaid'='$Drugs Per Episode',
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
                  'ERVisitsPerEpisode'='ER Visits Per Episode')
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
  # This section does column formatting.  As in fewSpreadsheets, I use strings in the column name to determine format.
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
      wrkTable <- formatRound(wrkTable, columns=c(var), digits=1)
    } 
  }
  # Format the row headers
  for (var in headerCols) {
    wrkTable <- formatStyle(wrkTable, c(var), backgroundColor='#f9f9f9', fontWeight='bold')
  }
  
  return(wrkTable)
}

zipMap <- function(dfName, Var, varlabel, digits=0, palette='RdYlGn') {
  MyCmd <- paste0("pal <- colorNumeric(palette='", palette, "', domain=", dfName, "$", Var, ")")
  eval(parse(text=MyCmd))
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
                  " addProviderTiles(providers$OpenMapSurfer.AdminBounds)  %>%", 
                  "addPolygons(fillColor=~pal(", var, "), ",
                  "weight=1, ",
                  "opacity=0, ",
                  "fillOpacity=.6, ", Popup, "))")
  eval(parse(text=MyCmd))
  return(myMap)
}


countyMap <- function(dfName, Var, varlabel, digits=0, palette='RdYlGn') {
  MyCmd <- paste0("pal <- colorNumeric(palette='", palette, "', domain=", dfName, "$", Var, ")")
  eval(parse(text=MyCmd))
  Popup<-"popup=~paste0('<H4><center>', NAMELSAD10, '</center></H4>', '</b>Episodes: <b>', Episodes,"
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
                  " addProviderTiles(providers$OpenMapSurfer.AdminBounds)  %>%", 
                  " addPolygons(fillColor=~pal(", var, "), ",
                  "weight=1, ",
                  "opacity=0, ",
                  "fillOpacity=.6, ", Popup, "))")
  eval(parse(text=MyCmd))
  return(myMap)
}

ui <- dashboardPage(
  skin='blue',
  dashboardHeader(title='OCM Baseline Analysis', titleWidth=300),
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
  dashboardBody(
    tabsetPanel(type='tabs',
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
                                 status='success', width=NULL, plotOutput('ieTree', height='350px')
                             )     
                      )
                    )
                ), # close tabPanel
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
                                 selected='Map',
                                 tabPanel('Map',   leafletOutput('KeyStatZipMap', height='600px')),
                                 tabPanel('Table', dataTableOutput('KeyStatTable', height='600px'))
                          )
                      ),
                      column(6,
                             tabBox(title=HTML('<H3>County Maps'),
                                    side='left', width=12,
                                    selected='Map',
                                    tabPanel('Map', leafletOutput('KeyStatCountyMap', height='600px'))
                             )
                      )
                  )
              ),
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
            tabPanel('Pricing Factors', 
                     fluidRow(
                       column(4, 
                               box(solidHeader=FALSE, title='Clinical Trial Coding',
                                   status='primary', width=NULL,
                                   plotlyOutput('TrialsStacked', height='570px', width='100%') 
                               )
                       ),
                       column(4, 
                              box(solidHeader=FALSE, title='HCC Coding',
                                  status='primary', width=NULL,
                                  plotlyOutput('HCC', height='570px', width='100%') )
                       ),
                       column(4, 
                              box(solidHeader=FALSE, title='Drug Clean Periods',
                                  status='primary', width=NULL,
                                  plotlyOutput('CleanPeriod', height='570px', width='100%'))
                       )
                     )
                     ,
                     fluidRow(
                              box(solidHeader=FALSE, title='Patients in Clinical Trials',
                                  status='primary', width=6,
                                  plotlyOutput('TableTrials') 
                              )
                     )
            ),
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
            tabPanel('Inpatient',
                     column(3,
                            box(solidHeader=FALSE, title='Type of Service Summary',
                                status='primary', width=NULL)
                     ),
                     column(9, 
                            box(solidHeader=FALSE, title='Cost by Type of Service',
                                status='primary', width=NULL)
                     ) # close column
            ),
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
            )
    ) # close tabsetPanel
  ) # close dashboardBody
) # close dashboardPage

server <- function(input, output) { 
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
  
  # Outputs on KPI page
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
  output$ieTree <- renderPlot( {
    dfi <- filterdfepi(dfie)
    dfi <- dfi %>% group_by(ClaimType) %>%
      summarise(Discharges=n(),
                meanCost=mean(Paid))
    treemap(dfi, index="ClaimType", vSize="Discharges", type='index',
            fontsize.labels=14, fontface.labels='plain', inflate.labels=FALSE,
            palette=c('#6995cd', '#c08943', '#34629c'), title='')
    })
  
  # outputs on the Geography Page
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
    subdat<-subdat[subdat$GEOID10 %in% dfw$ZipCode,]
    dfw <- merge(subdat, dfw, by.x='GEOID10', by.y='ZipCode')
    zipcodeMap <- zipMap(dfName='dfw', Var=input$KeyStatVar, digits=0, varlabel='Savings / Losses Per Epis: $')
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
      filter(Episodes>=10) #input$GeoMinEpis)
    dat<-dat[dat$GEOID10 %in% dfwc$County,]
    dfwc <- merge(dat, dfwc, by.x='GEOID10', by.y='County') 
    MyCountyMap <- countyMap(dfName='dfwc', Var=input$KeyStatVar, digits=0, varlabel='S')
    #MyCountyMap <- countyMap(dfName='dfwc', Var='MeanSavings', digits=0, varlabel='S')
    MyCountyMap
  })
  
  
    # Outputs on the Pricing tab
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
  
  # Outputs on the Pricing Model tab
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
  
  # Outputs on the Pricing Factors tab
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
        add_trace(type='bar', x=~None, name='No Trial',
                  text=~paste(YV, "<br>", round(None,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text') %>% 
        add_trace(type='bar', x=~Diag, name='ICD, Used in OCM',
                  text=~paste(YV, "<br>", round(Diag,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text') %>% 
        add_trace(type='bar', x=~CPT, name='CPT Modifier', 
                  text=~paste(YV, "<br>", round(CPT,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text') %>% 
        add_trace(type='bar', x=~Both, name='Both Methods',
                  text=~paste(YV, "<br>", round(Both,1), '% of ', GlobalRowCount, 'Episodes'),
                  hoverinfo='text') %>% 
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
                 height=570,
                 margin=list(l=150))
  } )
  output$HCC <- renderPlotly( {
    dfep <- filterdfepi(dfepi)
    nepis = nrow(dfep)
    dfhc <- merge(dfhcc, dfep, by='EpiNum')
    dfhc <- dfhc %>% group_by(HCC) %>% summarise(Episodes=n())
    dfhc$PercentEpisodes <- dfhc$Episodes/nepis * 100
    dfhc <- merge(dfhc, code_hcc)
    dfhc <- arrange(dfhc, Episodes) %>% tail(20)
    plot_ly(dfhc, y=~HCC_lbl, x=~PercentEpisodes, opacity=0.6, type='bar',
            marker=list(color='#7ac043'),
            text=~paste(HCC_lbl, 
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
                text=~paste(YV,
                            '<BR>Short', round(Short,1), '%'),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Middle, name='Middle',
                text=~paste(YV,
                            '<BR>2 Months - 2 Years', round(Middle,1), '%'),
                hoverinfo='text') %>%
      add_trace(type='bar', x=~Long, name='Long',
                text=~paste(YV,
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
  # Outputs on the Cost by TOS tab
  output$TOSStack <- renderPlotly( { 
    grpVar <- input$TOSGroupBy
    srtVar <- input$TOSSortBy
    dfep <- filterdfepi(dfepi)
    dftos <- select(dfep, "Sex", "Age", "DeathDate", "ZipCode", "EpiNum", 'ActualCost',
                    "EpiStart", "EpiEnd", "CancerTypeDetailed", "ReconciliationEligible",
                    "BaselinePrice", "PatientName", "Procs" = "PartBTOSPaidProcedures",
                    "PartBDrugs"="PartBTOSPaidDrugs", "PartBTOSepPaidImaging", "PartBTOSPaidLab",
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
            marker=list(color=colors,
                        line=list(width=0)),
            showlegend=FALSE)
  }) # close TOSstack
  
  # ER outputs
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
  
  # Drug Outputs
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
        MeanDrugPaid=mean(DrugPaid),
        MeanEpisodeCost=mean(ActualCost),
        MeanCostSavings=mean(Savings),
        AdmitsPerEpisode=mean(IPAdmits),
        ERVisitsPerEpisode=mean(ERVisits)
      ) %>% arrange(desc(Episodes))
    dfchemtab <- OutputTable(dfchem, headerCols=c('Episodes'))
    dfchemtab
  })
} # close server function

shinyApp(ui, server)

