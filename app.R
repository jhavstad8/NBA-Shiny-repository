#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Get the list of available seasons (CSV files) in the specified folder
season_files <- list.files("1997-2024 NBA Season Data", pattern = "\\.csv$", full.names = TRUE)
seasons <- gsub("1997-2024 NBA Season Data/(NBA)(\\d{2}).csv", "\\2", season_files)  # Extract season year


# UI layout
ui <- fluidPage(
  titlePanel("NBA Player Statistics"),  # Main title of the app
  
  # Tabbed Panels
  tabsetPanel(
    tabPanel("Top Players",
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
    
    # Rename the chosen_stat column to make it more readable in the table output
    colnames(top_players)[3] <- "Per Game"
    
    # Get the number of players to display from the user input
    num_players <- input$numPlayers
    
    # If the requested number of players is greater than the available players, return all players
    if (num_players > nrow(top_players)) {
      num_players <- nrow(top_players)
    }
    
    # Display the top 'num_players' players
    head(top_players, num_players)
  })
}

# Run the application
shinyApp(ui = ui, server = server)  # This should be at the top level of your script
