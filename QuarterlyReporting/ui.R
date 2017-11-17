#### Get default values and ranges for selects ####
potentialdocs <- sort(unique(dfepi$AttributedPhysicianName))
# Switch to CancerTypeDetailed
potentialCT <- sort(unique(dfepiB$CancerType))
potentialHosp <- sort(unique(dfie$CCN_lbl))
date1 <- min(dfepi$EpiStart)
date2 <- seq(as.Date(cut(as.Date(max(dfepi$EpiStart)), 'month')), length=2, by='-5 month')[2]

#### UI Section ####
ui <- dashboardPage(
  skin='green',
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
    tags$head(tags$style(HTML(".small-box {height: 90px}"))),
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
                           valueBoxOutput('ValueEpisB', width=2),
                           valueBoxOutput('ValueUniquePatientsB', width=2),
                           valueBoxOutput('ValueMeanAgeB', width=2),
                           valueBoxOutput('ValueUniqueProvB', width=2),
                           valueBoxOutput('ValueTotalSpendB', width=2),
                           valueBoxOutput('ValueAdmitsB', width=2)
                         ),
                         fluidRow(
                           column(6,
                                  tabBox(
                                    title='Financial Distributions',
                                    side='left', width=12,
                                    selected='Actual Cost',
                                    tabPanel('Actual Cost',  plotlyOutput('histActualCost', height='500px')),
                                    tabPanel('Winsorized Cost',  plotlyOutput('histWinsorized', height='500px')),
                                    tabPanel('Drug Cost',  plotlyOutput('histDrug', height='500px')),
                                    tabPanel('Inpat Cost',  plotlyOutput('histIP', height='500px'))
                                  )
                           ),
                           column(6,
                                  box(solidHeader=TRUE, title='Claims-Based Quality Measures',
                                      status='success', width=NULL,
                                      HTML('<h4><CENTER>% Patients with 3+ Days of Hospice</CENTER></h4>'),
                                      plotlyOutput('dotHospice', height='130px'),
                                      HTML('<HR><h4><CENTER>% of Episodes with Admissions</CENTER></h4>'),
                                      plotlyOutput('dotAdmit', height='130px'),
                                      HTML('<HR><H4><Center>% of Episodes with ER Visits</center></h4>'),
                                      plotlyOutput('dotER', height='130px')
                                  )
                           )
                         ),
                         fluidRow(box(solidHeader=TRUE, title='Claims-Based Quality Measure Table',
                                      status='success', width=12,
                                      DT::dataTableOutput('KPIQuality')
                                  )
                         )
                ), # close tabPanel
                #### Quality Tab ####
                tabPanel('Quality',
                         fluidRow(column(3, HTML('<H4><i>All Episodes, Perf. Period</i></H4>')),
                                  valueBoxOutput('ValueQualityIP', width=2),
                                  valueBoxOutput('ValueQualityER', width=2),
                                  valueBoxOutput('ValueQualityMort', width=2),
                                  valueBoxOutput('ValueQualityICU', width=2)
                         ),
                         fluidRow(column(3, HTML('<H4><i>Baseline Period</i></H4>')),
                                  valueBoxOutput('ValueQualityIPB', width=2),
                                  valueBoxOutput('ValueQualityERB', width=2),
                                  valueBoxOutput('ValueQualityMortB', width=2),
                                  valueBoxOutput('ValueQualityICUB', width=2)
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
                         fluidRow(column(3, HTML('<H4><i>Episodes Ending in Death</i></H4>')),
                                  valueBoxOutput('ValueQualityOCM3', width=3),
                                  valueBoxOutput('ValueQuality14Admit', width=3),
                                  valueBoxOutput('ValueQualityEOLICU', width=3)
                         ),
                         fluidRow(column(3, HTML('<H4><i>         Baseline Period</i></H4>')),
                                  valueBoxOutput('ValueQualityOCM3B', width=3),
                                  valueBoxOutput('ValueQuality14AdmitB', width=3),
                                  valueBoxOutput('ValueQualityEOLICUB', width=3)
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
                # tabPanel('Pricing Factors', 
                #          fluidRow(
                #            column(6, 
                #                   box(solidHeader=FALSE, title='Clinical Trial Coding',
                #                       status='primary', width=NULL,
                #                       plotlyOutput('TrialsStacked', height='570px') )
                #            ),
                #            column(6, 
                #                   box(solidHeader=FALSE, title='HCC Coding',
                #                       status='primary', width=NULL,
                #                       plotlyOutput('HCCChart', height='570px') )
                #            )
                #          )
                #          ,
                #          fluidRow(
                #            column(6,
                #                   box(solidHeader=FALSE, title='Patients in Clinical Trials',
                #                       status='primary', width=NULL,
                #                       DT::dataTableOutput('TableTrials', height='570px') 
                #                   )
                #            ),
                #            column(6, 
                #                   box(solidHeader=FALSE, title='Drug Clean Periods',
                #                       status='primary', width=NULL,
                #                       plotlyOutput('CleanPeriod', height='570px'))
                #            )
                #          )
                # ),
                #### Outlier Tab ####
                tabPanel('Outliers',
                         tabBox(title=HTML('<H3>Outlier Episodes</h3>'),
                                side='left', width=12, selected='Chart', height='780px',
                                tabPanel('Chart', plotlyOutput('OutlierBar')),
                                tabPanel('Patient List', DT::dataTableOutput('OutlierTable'))
                         )
                ),
                #### Type of Service Tab ####
                tabPanel('Type of Service',
                         column(3,
                                box(solidHeader=FALSE, title='Type of Service Summary',
                                    status='primary', width=NULL, height='800px',
                                    HTML('<H5>Performance Period</H5>'),
                                    plotlyOutput('TOSPie'), 
                                    HTML('<HR><H5>Baseline Period</H5>'),
                                    plotlyOutput('TOSPie2') 
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
                                  selectInput('ieCCS', 'Diag Group Filter', vecCCS, 
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
                           valueBoxOutput('ValueEOLHospiceCost0', width=2),
                           valueBoxOutput('ValueEOLHospiceCost3', width=2)
                         ),
                         fluidRow(
                           valueBoxOutput('ValueEOLDeathsB', width=2),
                           valueBoxOutput('ValueEOLPctHospiceB', width=2),
                           valueBoxOutput('ValueEOLPassOCM3B', width=2),
                           valueBoxOutput('ValueEOLHospicePaidB', width=2),
                           valueBoxOutput('ValueEOLHospiceCost0B', width=2),
                           valueBoxOutput('ValueEOLHospiceCost3B', width=2)
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
                           infoBoxOutput('IBEOLQualityAdmitB'),
                           infoBoxOutput('IBEOLQualityICUB'),
                           infoBoxOutput('IBEOLQualityChemoB')
                         ),
                         fluidRow(
                           tabBox(title='Care in Last 14 Days of Life', side='left', width=12, height='530px', selected='Admits',
                                  tabPanel('Admits', plotlyOutput('EOLQualityAdmit', height='450px')),
                                  tabPanel('ICU',    plotlyOutput('EOLQualityICU', height='450px')),
                                  tabPanel('Chemo', plotlyOutput('EOLQualityChemo', height='450px'))
                           )
                         )
                )#,
                #### Simulation Tab ####
                # tabPanel('Simulation',
                    #      # Settings and instructions ####
                    #      fluidRow(column(1, numericInput('BootSize', label='Sample Size', value = round(nrow(dfep)/6,0))),
                    #               column(11, HTML('<H3><center>About this Tab</center></h3>This tab presents the results of a "bootstrap" 
                    #                               simulation.  The simulation mimics the random variation that a practice 
                    #                               is likely to experience under OCM.  Essentially, the simulation involves
                    #                               computing the costs, target prices, and savings associated with a random 
                    #                               sample of episodes from your baseline data.  This tab repeats that random 
                    #                               sampling 100 times, with each sample having a different set of episodes.<br>br>
                    #                               The results show simulated reconciliations as they will occur in the OCM 
                    #                               program.  You will almost certainly see some reconciliations where you have 
                    #                               savings and some where you have losses.  The key is to understand the 
                    #                               volatility that is almost sure to occur with your program.  <b>The results 
                    #                               assume that you make no real changes to the way that you treat patients.</b>  
                    #                               We realize that assumption is absurd, but the volatility will persist anyway.
                    #                               <br>To use this page, type a sample size (number of episodes) into the box 
                    #                               on the left.  (The default is the average number of episodes that you had per 
                    #                               half-year during the baseline period.)  Each time you change the number, the 
                    #                               software will create 100 new samples.  <i>If you leave this page and return 
                    #                               to it, the simulation will be repeated from scratch and you will see new 
                    #                               results.<i>')
                    #           )
                    #           
                    # ), 
                    # # Reports ####
                    # fluidRow(
                    #     tabBox('Volatility', side='left', width=12, height='510px',
                    #            selected='First 25 Reconciliations',
                    #         tabPanel('First 25 Reconciliations', plotlyOutput('BootBar')),
                    #         tabPanel('Histogram', plotlyOutput('BootHist')),
                    #         tabPanel('Control Chart', plotlyOutput('BootControlChart'))
                    #     )
                    # )
            #)
            
    ) # close tabsetPanel
  ) # close dashboardBody
) # close dashboardPage
