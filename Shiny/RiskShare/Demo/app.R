library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggthemes)
library(plotly)
library(DT)
df = tibble( saveRate = seq(-15, 20, by=0.5) )
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
CalcCosts <- function(df, cut1, cut2, losscut1, losscut2, MV, PS, Prem, Pct1, Pct2, Pct3, LossPct1, LossPct2, LossPct3) {
  df$Loss3 = ifelse(df$EffPct<losscut2, PS * (df$EffPct-losscut2)/100 * LossPct3/100, 0)
  df$Loss2 = ifelse(df$EffPct<losscut1, ifelse(df$EffPct<losscut2, PS*(losscut2 -losscut1)/100*LossPct2/100, 
                                               PS*(df$EffPct-losscut1)/100*LossPct2/100),0)
  df$Loss1 = ifelse(df$EffPct<0, ifelse(df$EffPct<losscut1, PS*(losscut1/100)*LossPct1/100, 
                                        PS*(df$EffPct)/100*LossPct1/100),0)
  df$Gain3 = ifelse(df$EffPct>cut2, PS*(df$EffPct-cut2)/100*Pct3/100, 0)
  df$Gain2 = ifelse(df$EffPct>cut1, ifelse(df$EffPct>cut2, PS*(cut2 -cut1)/100*Pct2/100, 
                                           PS*(df$EffPct-cut1)/100*Pct2/100),0)
  df$Gain1 = ifelse(df$EffPct>0, ifelse(df$EffPct>cut1, PS*(cut1)/100*Pct1/100, 
                                        PS*(df$EffPct)/100*Pct1/100),0)
  df$PracticeNet = df$Loss1 + df$Loss2 + df$Loss3 + df$Gain1 + df$Gain2 + df$Gain3 - Prem + MV
  df$GrossResult = (df$EffPct)/100*PS + MV
  df$NCHNet = df$GrossResult - df$PracticeNet
  return(df)
}
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
                  'ERVisitsPerEpisode'='ER Visits Per Episode',
                  'SavingsPercent'='Gross Savings Rate',
                  'SavingsPercentAfterDiscount'='Saving Rate After Discount',
                  'TotalSavingsPaid'='$Gross Risk',
                  'PracticePaid'='$Practice Share',
                  'NCHPaid'='$NCH Share'
  )
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
    if ( (grepl('paid',var2)) | (grepl('cost',var2)) | (grepl('price',var2)) ) {
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


# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme('simplex'),
   # Application title
   titlePanel("Risk Sharing Model"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(8, HTML("<H4>Program Size in Millions</H4>")),
          column(4, numericInput("ProgSize", label=NULL, value = 10))
        ),
        fluidRow(
          column(8, HTML("<H4>% Discount to Payer</H4>")),
          column(4, numericInput("Discount", label=NULL, value = 4))
        ),
        fluidRow(
          column(8, HTML("<H4>MIPS Value in $Thousands</H4>")), 
          column(4, numericInput("MIPSValue", label=NULL, value = 0))
        ),
        fluidRow(
          column(8, HTML("<h4>Premium to NCH (in $Thous)</H4>")),
          column(4, numericInput('Premium', label=NULL, value=0))
        ),
        fluidRow(
          column(8, 
                 sliderInput("LossRange1", label = h5("Initial Loss Range"), min = 0, step=0.5,
                             max = 20, value = c(0, 3))),
          column(4,
                 numericInput('LossPracPct1', label=h5('% from Practice'), value=75, step=1))
        ),
        fluidRow(
          column(8, 
                 sliderInput("LossRange2", label = h5("Middle Loss Range"), min = 0, 
                             max = 20, step=.5, value = c(3, 8))),
          column(4,
                 numericInput('LossPracPct2', label=h5('% from Practice'), value=25, step=1))
        ),
        fluidRow(
          column(8, 
                 sliderInput("LossRange3", label = h5("Large Loss Range"), min = 0, 
                             max = 20, step=.5, value = c(8,20))),
          column(4,
                 numericInput('LossPracPct3', label=h5('% from Practice'), value=10, step=1))
        ),
        fluidRow(
          column(8, 
                 sliderInput("Range1", label = h5("Initial Savings Range"), min = 0, step=0.5,
                             max = 20, value = c(0, 3))),
          column(4,
                 numericInput('PracPct1', label=h5('% to Practice'), value=75, step=1))
        ),
        fluidRow(
          column(8, 
                 sliderInput("Range2", label = h5("Middle Savings Range"), min = 0, 
                             max = 20, step=.5, value = c(3, 8))),
          column(4,
                 numericInput('PracPct2', label=h5('% to Practice'), value=25, step=1))
        ),
        fluidRow(
          column(8, 
                 sliderInput("Range3", label = h5("Large Savings Range"), min = 0, 
                             max = 20, step=.5, value = c(8,20))),
          column(4,
                 numericInput('PracPct3', label=h5('% to Practice'), value=10, step=1))
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        fluidRow(
          column(12, 
                 tabBox(
                   title=HTML('<H3>Net Results Under Scenario</h3>'),
                   side='left', width=12,
                   selected='Summary Chart',
                   tabPanel('Summary Chart', plotlyOutput('SummaryChart', height='680px')),
                   tabPanel('Summary Table', DT::dataTableOutput('SummaryTable', height='680px'))
                 )
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$SummaryChart <- renderPlotly( {
    cut1 = input$Range1[2]
    cut2 = input$Range2[2]
    losscut1 = -input$Range1[2]
    losscut2 = -input$Range2[2]
    MV =   input$MIPSValue*1000
    PS =   input$ProgSize* 1000000
    Prem = input$Premium * 1000
    Pct1 = input$PracPct1
    Pct2 = input$PracPct2
    Pct3 = input$PracPct3
    LossPct1 = input$LossPracPct1
    LossPct2 = input$LossPracPct2
    LossPct3 = input$LossPracPct3
    df$EffPct = df$saveRate - input$Discount
    df = CalcCosts(df, cut1, cut2, losscut1, losscut2, MV, PS, Prem, Pct1, Pct2, Pct3, LossPct1, LossPct2, LossPct3)
    plot_ly(df) %>% 
       add_trace(type='scatter', x=~saveRate, y=~PracticeNet, 
                 mode='lines', marker=list(color='#437ac0'), name='Practice',
                 hovertext=~paste0(ifelse(EffPct>=0,'% Savings:', '% Losses:'), '<b>', saveRate, '%',
                                  '</b><br>$Net to Practice: $<b>', format(round(PracticeNet,0), big.mark=',', trim=TRUE)),
                 hoverinfo='text',
                 line=list(color='#437ac0', width=8)) %>% 
       add_trace(type='scatter', x=~saveRate, y=~NCHNet, 
                 mode='lines', marker=list(color='#7ac043'), name='NCH',
                 hovertext=~paste0(ifelse(EffPct>=0,'% Savings:', '% Losses:'), '<b>', saveRate, '%',
                                   '</b><br>$Net to NCH: $<b>', format(round(NCHNet,0), big.mark=',', trim=TRUE)),
                 hoverinfo='text',
                 line=list(color='#7ac043', width=8)) %>% 
       add_trace(type='scatter', x=~saveRate, y=~GrossResult, 
                 mode='lines', marker=list(color='#8943c0', size=2), name='Gross Result',
                 hovertext=~paste0(ifelse(EffPct>=0,'% Savings:', '% Losses:'), '<b>', saveRate, '%',
                                   '</b><br>$Net to Practice: $<b>', format(round(GrossResult,0), big.mark=',', trim=TRUE)),
                 hoverinfo='text',
                 line=list(color='#8943c0', width=2, dash='dot')) %>% 
       layout(xaxis=list(title='% of Costs Saved', 
                         showticklabels=TRUE, 
                         titlefont=font1, 
                         tickfont=font2,
                         separatethousands=TRUE), 
              yaxis=list(title='$Net to Practice and NCH', 
                         titlefont=font1,
                         tickfont=font2,
                         showgrid=TRUE),
              legend=list(font=fontNarrow)
       )
  
     
     })
  output$SummaryTable <- DT::renderDataTable({
     cut1 = input$Range1[2]
     cut2 = input$Range2[2]
     losscut1 = -input$Range1[2]
     losscut2 = -input$Range2[2]
     MV =   input$MIPSValue*1000
     PS =   input$ProgSize* 1000000
     Prem = input$Premium * 1000
     Pct1 = input$PracPct1
     Pct2 = input$PracPct2
     Pct3 = input$PracPct3
     LossPct1 = input$LossPracPct1
     LossPct2 = input$LossPracPct2
     LossPct3 = input$LossPracPct3
     df$EffPct = df$saveRate - input$Discount
     df = CalcCosts(df, cut1, cut2, losscut1, losscut2, MV, PS, Prem, Pct1, Pct2, Pct3, LossPct1, LossPct2, LossPct3)
     df <- select(df, 'saveRate', 'EffPct', 'GrossResult', 'PracticeNet', 'NCHNet') %>% 
       rename(
         'SavingsPercent'='saveRate',
         'SavingsPercentAfterDiscount'='EffPct',
         'TotalSavingsPaid'='GrossResult',
         'PracticePaid'='PracticeNet',
         'NCHPaid'='NCHNet'
       ) %>% 
       mutate(
         SavingsPercent = SavingsPercent/100,
         SavingsPercentAfterDiscount = SavingsPercentAfterDiscount/100
       )
     OUTtable <- OutputTable(df, headerCols = c('SavingsPercent'))
     OUTtable
   })

}
# Run the application 
shinyApp(ui = ui, server = server)

