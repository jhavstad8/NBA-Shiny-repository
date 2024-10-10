#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Necessary Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Specify the directory containing the data files
data_directory <- "/Users/joshhavstad/Desktop/NBA Shiny/1997-2024 NBA Player Data"


# Get the list of available seasons (CSV files) in the specified folder
season_files <- list.files(data_directory, pattern = "\\.csv$", full.names = TRUE)

# Extract the year from the file paths
seasons <- gsub(".*/NBA(\\d{4})\\.csv", "\\1", season_files)


# UI layout
ui <- fluidPage(
  titlePanel("NBA Player Statistics"),  # Main title of the app
  
  # Tabbed Panels
  tabsetPanel(
    tabPanel("Top Players", # label for the panel
             
             h3("Top Players by Statistic"),  # Add a heading for the section
             
             # Create a fluid row for the inputs and the table
             fluidRow(
               column(4,  # Adjust width as needed (4 out of 12 columns)
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
                      
                      # Dropdown menu to choose season
                      selectInput("season", 
                                  "Choose Season:", 
                                  choices = paste0("NBA", 1997:2024),
                                  selected = "NBA2024")  # Default is 2024 season
               ),
               
               column(8,  # Adjust width as needed (8 out of 12 columns)
                      # Output for the dynamic title
                      htmlOutput("tableTitle"),  
                      
                      tableOutput("topPlayersTable")  # Output for the table
               )
             )
    ),
    tabPanel("Player Time Series", # label for the panel
             
             h3("Player Statistics Over Time"), # heading for the section
             
             fluidRow(
               # user input to decide the player
               textInput("playerName",  
                         "Search Player:", 
                         value = "LeBron James"),
               
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
             ),

             plotlyOutput("timeSeriesPlot")  # Output for the graph
    )
  )
)


# Server logic
server <- function(input, output) {
  
  # Reactive expression to read the selected season data
  nba_data <- reactive({
    file_path <- paste0("1997-2024 NBA Player Data/", input$season, ".csv")
    
    # Check if the file exists before reading
    if (!file.exists(file_path)) {
      stop(paste("File does not exist:", file_path))
    }
    
    read.csv(file_path)
  })
  
  # Calculate per game stats when the data is loaded
  calculated_data <- reactive({
    data <- nba_data()
    
    # Avoid division by zero
    data$PTS_per_game <- ifelse(data$G > 0, data$PTS / data$G, 0)
    data$AST_per_game <- ifelse(data$G > 0, data$AST / data$G, 0)
    data$TRB_per_game <- ifelse(data$G > 0, data$TRB / data$G, 0)
    data$STL_per_game <- ifelse(data$G > 0, data$STL / data$G, 0)
    data$BLK_per_game <- ifelse(data$G > 0, data$BLK / data$G, 0)
    data$TOV_per_game <- ifelse(data$G > 0, data$TOV / data$G, 0)
    
    return(data)
  })
  
  # Reactive to gather per-game data for the selected player by looping through seasons
  combined_player_data <- reactive({
    # Create an empty dataframe to hold the per-game statistics across seasons
    per_game_stats <- data.frame(Season = integer(), PerGameStat = numeric())
    
    # Loop through each season file and process the data for the selected player
    for (season_file in season_files) {
      # Read the current season's data
      season_data <- read.csv(season_file)
      
      # Extract the season year from the file name
      season_year <- gsub(".*NBA(\\d{4}).csv", "\\1", season_file)
      
      # Filter data for the selected player
      player_data <- season_data[season_data$Player == input$playerName, ]
      
      # If the player did not play in the current season, skip to the next file
      if (nrow(player_data) == 0) {
        next
      }
      
      # Calculate the per-game statistic based on user input
      per_game_value <- 0
      if (input$statistic == "PTS_per_game") {
        per_game_value <- ifelse(player_data$G > 0, player_data$PTS / player_data$G, 0)
      } else if (input$statistic == "AST_per_game") {
        per_game_value <- ifelse(player_data$G > 0, player_data$AST / player_data$G, 0)
      } else if (input$statistic == "TRB_per_game") {
        per_game_value <- ifelse(player_data$G > 0, player_data$TRB / player_data$G, 0)
      } else if (input$statistic == "STL_per_game") {
        per_game_value <- ifelse(player_data$G > 0, player_data$STL / player_data$G, 0)
      } else if (input$statistic == "BLK_per_game") {
        per_game_value <- ifelse(player_data$G > 0, player_data$BLK / player_data$G, 0)
      } else if (input$statistic == "TOV_per_game") {
        per_game_value <- ifelse(player_data$G > 0, player_data$TOV / player_data$G, 0)
      }
      
      # Append the result to the per_game_stats dataframe
      per_game_stats <- rbind(per_game_stats, data.frame(Season = as.integer(season_year), PerGameStat = per_game_value))
    }
    
    return(per_game_stats)
  })
  
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
    paste("<strong>Top", input$numPlayers, "Players by", stat_name, "</strong>")
  })
  
  # Render the top players table
  output$topPlayersTable <- renderTable({
    # Select the chosen statistic
    chosen_stat <- input$statistic
    
    # Get the corresponding readable statistic name
    stat_name <- switch(input$statistic,
                        "PTS_per_game" = "Points Per Game",
                        "AST_per_game" = "Assists Per Game",
                        "TRB_per_game" = "Rebounds Per Game",
                        "STL_per_game" = "Steals Per Game",
                        "BLK_per_game" = "Blocks Per Game",
                        "TOV_per_game" = "Turnovers Per Game")
    
    # Get the current data and filter based on minimum games played
    filtered_data <- calculated_data()[calculated_data()$G >= input$minGames, ]
    
    # Ensure the chosen statistic exists and is numeric
    if (!chosen_stat %in% colnames(filtered_data)) {
      return(data.frame(Player = character(0), G = numeric(0), `Per Game` = numeric(0)))
    }
    
    # Check for NA values and remove them
    filtered_data <- filtered_data[!is.na(filtered_data[[chosen_stat]]), ]
    
    # Check if there are any players left after filtering
    if (nrow(filtered_data) == 0) {
      return(data.frame(Player = character(0), G = numeric(0), `Per Game` = numeric(0)))
    }
    
    # Sort by the chosen stat (descending), then by G (games played) to break ties
    top_players <- filtered_data[order(-filtered_data[[chosen_stat]], -filtered_data$G), ]
    
    # Select only relevant columns to display (Player, Games Played, and the chosen statistic)
    top_players <- top_players[, c("Player", "G", chosen_stat, "Pos", "Tm"), drop = FALSE]
    
    # Rename columns to make them more readable in the table output
    colnames(top_players)[2] <- "Games"
    colnames(top_players)[3] <- stat_name # Use the readable statistic name
    colnames(top_players)[4] <- "Position"
    colnames(top_players)[5] <- "Team"
    
    # Get the number of players to display from the user input
    num_players <- input$numPlayers
    
    # If the requested number of players is greater than the available players, return all players
    if (num_players > nrow(top_players)) {
      num_players <- nrow(top_players)
    }
    
    # Display the top 'num_players' players
    head(top_players, num_players)
  })
  
  
  # Render the time series plot
  output$timeSeriesPlot <- renderPlotly({
    player_stats <- combined_player_data()
    
    # If there is no data, return NULL
    if (is.null(player_stats)) {
      return(NULL)
    }
    
    # Get the corresponding readable statistic name
    stat_name <- switch(input$statistic,
                        "PTS_per_game" = "Points Per Game",
                        "AST_per_game" = "Assists Per Game",
                        "TRB_per_game" = "Rebounds Per Game",
                        "STL_per_game" = "Steals Per Game",
                        "BLK_per_game" = "Blocks Per Game",
                        "TOV_per_game" = "Turnovers Per Game")
    
    # Create the time series plot
    p <- ggplot(player_stats, aes(x = Season, y = PerGameStat)) +
      geom_line(color = '#87CEEB') +
      geom_point(aes(text = paste("Season:", Season, "<br>", stat_name, ":", round(PerGameStat, 2))), color = 'blue') +  # Add hover text
      labs(title = paste(input$playerName, ":", stat_name, "Over Time"),
           x = "Season",
           y = stat_name) +
      theme_minimal()
    
    # Convert ggplot to plotly object
    ggplotly(p, tooltip = "text") # Specify to use the hover text
  })
}

# Run the application
shinyApp(ui = ui, server = server)  
