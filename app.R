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
    titlePanel("Top NBA Players by Points Per Game"),
    
    # Input for number of players to view
    numericInput("numPlayers", 
                 "Number of Players to View:", 
                 value = 10, 
                 min = 1),  # Default is 10, minimum is 1
    
    tableOutput("topPlayersTable")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Read the NBA23.csv file
  nba_data <- read.csv("NBA23.csv")
  
  # Calculate Points Per Game (PPG)
  nba_data$PPG <- nba_data$PTS / nba_data$G
  
  # Sort by PPG (descending), then by G (descending) in case of ties
  top_players <- nba_data[order(-nba_data$PPG, -nba_data$G), ]
  
  # Select only relevant columns to display (e.g., Player, PTS, G, PPG)
  top_players <- top_players[, c("Player", "PPG", "Pos", "Tm", "G")]
  
  # Render the top players table
  output$topPlayersTable <- renderTable({
    # Get the number of players to display from the user input
    num_players <- input$numPlayers
    
    # Check if the requested number is greater than the available players
    if (num_players > nrow(top_players)) {
      num_players <- nrow(top_players)  # Return all players if input is larger
    }
    
    # Display the top 'num_players' players
    head(top_players, num_players)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
