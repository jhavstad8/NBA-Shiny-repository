#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# UI layout
ui <- fluidPage(
  titlePanel("NBA Player Statistics"),  # Main title of the app
  
  # Tabbed Panels
  tabsetPanel(
    tabPanel("Top Players",
             h3("Top Players by Statistic"),  # Add a heading for the section
             
             # Input for number of players to view
             numericInput("numPlayers", 
                          "Number of Players to View:", 
                          value = 10, 
                          min = 1),  # Default is 10, minimum is 1
             
             # Input for minimum number of games
             numericInput("minGames", 
                          "Minimum Games Played:", 
                          value = 0, 
                          min = 0,  # Minimum value is 0
                          max = 82), # Maximum value is 82
             
             # Dropdown menu to choose statistic
             selectInput("statistic", 
                         "Choose Statistic:",
                         choices = c("Points Per Game" = "PTS_per_game",
                                     "Assists Per Game" = "AST_per_game",
                                     "Rebounds Per Game" = "TRB_per_game",
                                     "Steals Per Game" = "STL_per_game",
                                     "Blocks Per Game" = "BLK_per_game",
                                     "Turnovers Per Game" = "TOV_per_game"),
                         selected = "PTS_per_game"),  # Default is Points Per Game
             # Output for the dynamic title
             textOutput("tableTitle"),  # Add text output for dynamic title
             
             tableOutput("topPlayersTable")  # Output for the table
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Read the NBA23.csv file
  nba_data <- read.csv("NBA23.csv")
  
  # Calculate per game stats
  nba_data$PTS_per_game <- nba_data$PTS / nba_data$G
  nba_data$AST_per_game <- nba_data$AST / nba_data$G
  nba_data$TRB_per_game <- nba_data$TRB / nba_data$G
  nba_data$STL_per_game <- nba_data$STL / nba_data$G
  nba_data$BLK_per_game <- nba_data$BLK / nba_data$G
  nba_data$TOV_per_game <- nba_data$TOV / nba_data$G
  
  # Render the dynamic table title
  output$tableTitle <- renderText({
    stat_name <- switch(input$statistic,
                        "PTS_per_game" = "Points Per Game",
                        "AST_per_game" = "Assists Per Game",
                        "TRB_per_game" = "Rebounds Per Game",
                        "STL_per_game" = "Steals Per Game",
                        "BLK_per_game" = "Blocks Per Game",
                        "TOV_per_game" = "Turnovers Per Game")
    
    # Get the number of players from input
    paste("Top", input$numPlayers, "Players by", stat_name)
  })
  
  # Render the top players table
  output$topPlayersTable <- renderTable({
    # Select the chosen statistic
    chosen_stat <- input$statistic
    
    # Filter the data based on minimum games played
    filtered_data <- nba_data[nba_data$G >= input$minGames, ]
    
    # Check if there are any players left after filtering
    if (nrow(filtered_data) == 0) {
      return(data.frame(Player = character(0), G = numeric(0), `Per Game` = numeric(0)))
    }
    
    # Sort by the chosen stat (descending), then by G (games played) to break ties
    top_players <- filtered_data[order(-filtered_data[[chosen_stat]], -filtered_data$G), ]
    
    # Select only relevant columns to display (Player, Games Played, and the chosen statistic)
    top_players <- top_players[, c("Player", "G", chosen_stat, "Pos", "Tm")]
    
    # Rename the chosen_stat column to make it more readable in the table output
    colnames(top_players)[3] <- "Per Game"
    
    # Get the number of players to display from the user input
    num_players <- input$numPlayers
    
    # If the requested number of players is greater than the available players, return all players
    if (num_players > nrow(top_players)) {
      num_players <- nrow(top_players)
    }
    
    # If no players are available, return an empty data frame
    if (nrow(top_players) == 0) {
      return(data.frame(Player = character(0), G = numeric(0), `Per Game` = numeric(0)))
    }
    
    # Display the top 'num_players' players
    head(top_players, num_players)
  })
}

# Run the application
shinyApp(ui = ui, server = server)  # This should be at the top level of your script
