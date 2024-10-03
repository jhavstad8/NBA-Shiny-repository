#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NBA 2022-2023 Season Data Viewer"),
    
    tableOutput("nbaTable")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Read in the NBA23.csv file
  nba_data <- read.csv("NBA23.csv")
  
  # Render table to display the data
  output$nbaTable <- renderTable({
    nba_data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
