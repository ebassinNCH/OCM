library(shiny)
library(tidyverse)
library(feather)

dfepi = read_feather('/AdvAnalytics/OCM/CCSI/BaselineUpdated/Working/dfepi.feather')

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Target Price Distribution"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("BinWidth", 'Bin Width in $',
                    choices = c('1000', '2000', '2500', '5000', '10000'),  
                    selected = 5000),
        textInput('BarColor', 'Histogram Bar Color', value='#437ac0', width=200)
        # sliderInput("bins",
        #              "Number of bins:",
        #              min = 10,
        #              max = 50,
        #              value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- dfepi$ActualCost
      binwidth = as.numeric(input$BinWidth)
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      ggplot(data=dfepi) + 
        geom_histogram(mapping=aes(x=x), fill=input$BarColor, binwidth=binwidth)
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

