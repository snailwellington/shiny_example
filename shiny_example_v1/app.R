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
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 500, min = 1, max = 100),
  plotOutput("hist")
      
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
 output$hist <- renderPlot({
   title <- "Histogram"
   ggplot2::qplot(rnorm(input$num))
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

