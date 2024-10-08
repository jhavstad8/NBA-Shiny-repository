# Necessary Libraries
library(tidyverse)
library(rvest)

# Create Loop to create Datasets for 1997-2023 NBA Seasons

# Define the required column names for each dataset
required_columns <- c("Player", "Rk", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", 
                      "FG%", "3P", "3PA", "3P%", "2P", "2PA", "2P%", "eFG%", "FT", "FTA", 
                      "FT%", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", 
                      "PER", "TS%", "FTr", "ORB%", "DRB%", "TRB%", "AST%", "STL%", "BLK%", 
                      "TOV%", "USG%", "OWS", "DWS", "WS", "WS/48", "OBPM", "DBPM", "BPM", 
                      "VORP", "Dist.", "%2P", "%0-3", "%3-10", "%10-16", "%16-3P", "%3P", 
                      "0-3%", "3-10%", "10-16%", "16-3P%", "2ast", "3ast", "%Dunks", "#Dunks", 
                      "%C3", "C3%")

# Loop through each year from 1997 to 2024
for (year in 1997:2024) {
  
  # Dynamic URLs for each year
  url_totals <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
  url_advanced <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html")
  url_shooting <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_shooting.html")
  
  # Read and parse HTML tables
  wp_totals <- read_html(url_totals)
  Sys.sleep(5)
  wp_advanced <- read_html(url_advanced)
  Sys.sleep(5)
  wp_shooting <- read_html(url_shooting)
  Sys.sleep(5)
  
  # Read tables into dataframes
  totals <- html_table(html_nodes(wp_totals, "#totals_stats"), fill = TRUE)[[1]]
  advanced <- html_table(html_nodes(wp_advanced, "#advanced_stats"), fill = TRUE)[[1]]
  shooting <- html_table(html_nodes(wp_shooting, "#shooting_stats"), fill = TRUE)[[1]]
  
  # Fix column names and clean up shooting data frame
  colnames(shooting)[1:9] <- c("Rk", "Player", "Pos", "Age", "Tm", "G", "MP", "FG%", "Dist.")
  colnames(shooting) <- as.character(unlist(shooting[1,]))
  shooting <- shooting[-1,]
  colnames(shooting)[11:16] <- paste0("%", colnames(shooting)[11:16])
  colnames(shooting)[18:23] <- paste0(colnames(shooting)[18:23], "%")
  colnames(shooting)[25:26] <- c("2ast", "3ast")
  colnames(shooting)[28:29] <- c("%Dunks", "#Dunks")
  colnames(shooting)[31:32] <- c("%C3", "C3%")
  
  del <- c(10, 17, 24, 27, 30, 33, 34, 35)
  shooting <- select(shooting, -all_of(del))
  
  # Handle unnamed columns in totals, advanced, and shooting data
  colnames(totals)[colnames(totals) == ""] <- paste0("unnamed_", seq_along(colnames(totals)[colnames(totals) == ""]))
  colnames(advanced)[colnames(advanced) == ""] <- paste0("unnamed_", seq_along(colnames(advanced)[colnames(advanced) == ""]))
  colnames(shooting)[colnames(shooting) == ""] <- paste0("unnamed_", seq_along(colnames(shooting)[colnames(shooting) == ""]))
  
  
  # Remove * symbols from Player names
  totals$Player <- gsub("\\*", "", totals$Player)
  advanced$Player <- gsub("\\*", "", advanced$Player)
  shooting$Player <- gsub("\\*", "", shooting$Player)
  
  # Remove player duplicates
  totals <- totals[!duplicated(totals$Player), ]
  advanced <- advanced[!duplicated(advanced$Player), ]
  shooting <- shooting[!duplicated(shooting$Player), ]
  
  # Rename overlapping columns in advanced and shooting dataframes
  advanced <- advanced %>% rename_with(~ paste0(., "_adv"), -Player)
  shooting <- shooting %>% rename_with(~ paste0(., "_shoot"), -Player)
  
  # Merge datasets using full join to keep all players
  nba_data <- full_join(totals, advanced, by = 'Player')
  nba_data <- full_join(nba_data, shooting, by = 'Player')
  
  # Remove the suffixes after merging
  colnames(nba_data) <- gsub("_adv|_shoot", "", colnames(nba_data))
  
  # Ensure only required columns are kept
  nba_data_cleaned <- nba_data %>% select(all_of(required_columns))
  
  # Convert columns to numeric where appropriate
  nba_data_cleaned <- mutate_at(nba_data_cleaned, vars(c(4, 6:66)), as.numeric)
  
  # Check for any missing players
  missing_players <- nba_data_cleaned %>% filter(is.na(Player))
  if (nrow(missing_players) > 0) {
    cat("Missing players in year", year, ":", nrow(missing_players), "\n")
  }
  
  # Create the directory if it doesn't exist
  dir.create("1997-2024 NBA Player Data", showWarnings = FALSE)
  
  # Save the final dataset as a CSV file in the specified folder
  write.csv(nba_data_cleaned, paste0("1997-2024 NBA Player Data/NBA", year, ".csv"), row.names = FALSE)
}