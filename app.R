#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Show Opponents Elo in NBA"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("choiceSeason",
                     "Which Seasons to include:",
                     min = 2013,
                     max = 2019,
                     value = 2019,
                     ticks = FALSE,
                     sep = ""),
         selectInput("choicePlayoffs", "Include:",
                     choices = list("Only Regular Season" = 1,
                                    "Only Playoffs" = 2,
                                    "Both" = 3)),
         checkboxInput("choiceConf", "Split by conference", value = TRUE)
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
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

