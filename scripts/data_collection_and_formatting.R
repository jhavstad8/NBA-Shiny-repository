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

# Loop through each year from 1997 to 2023
for (year in 1997:2024) {
  
  # Dynamic URLs for each year
  url_totals <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_totals.html")
  url_advanced <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_advanced.html")
  url_shooting <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_shooting.html")
  
  # Read and parse HTML tables
  wp_totals <- read_html(url_totals)
  Sys.sleep(5)  # Wait for 5 seconds before making the next request
  wp_advanced <- read_html(url_advanced)
  Sys.sleep(5)  # Wait for 5 seconds before making the next request
  wp_shooting <- read_html(url_shooting)
  Sys.sleep(5)  # Wait for 5 seconds before continuing
  
  # Read tables into dataframes
  totals <- html_table(html_nodes(wp_totals, "#totals_stats"), fill = TRUE)[[1]]
  advanced <- html_table(html_nodes(wp_advanced, "#advanced_stats"), fill = TRUE)[[1]]
  shooting <- html_table(html_nodes(wp_shooting, "#shooting_stats"), fill = TRUE)[[1]]
  
  # Clean up shooting stats
  colnames(shooting) <- as.character(unlist(shooting[1,]))
  shooting <- shooting[-1,]
  colnames(shooting)[11:16] <- paste0("%", colnames(shooting)[11:16])
  colnames(shooting)[18:23] <- paste0(colnames(shooting)[18:23], "%")
  colnames(shooting)[25:26] <- c("2ast", "3ast")
  colnames(shooting)[28:29] <- c("%Dunks", "#Dunks")
  colnames(shooting)[31:32] <- c("%C3", "C3%")
  
  del <- c(10, 17, 24, 27, 30, 33, 34, 35)
  shooting <- select(shooting, -all_of(del))
  
  # Remove player duplicates
  totals <- totals[!duplicated(totals$Player), ]
  advanced <- advanced[!duplicated(advanced$Player), ]
  shooting <- shooting[!duplicated(shooting$Player), ]
  
  # Merge datasets while adding suffixes to avoid duplicate column names
  nba_data <- merge(totals, advanced, by = 'Player', suffixes = c(".totals", ".adv")) # Suffixes added here
  nba_data <- merge(nba_data, shooting, by = 'Player', suffixes = c("", ".shoot")) # Merging shooting stats
  
  # Ensure only required columns are kept
  nba_data_cleaned <- nba_data %>% select(all_of(required_columns))
  
  # Convert columns to numeric where appropriate
  nba_data_cleaned <- mutate_at(nba_data_cleaned, vars(c(4, 6:66)), as.numeric)
  
  # Save the final dataset as a CSV file for the given year
  write.csv(nba_data_cleaned, paste0("NBA", year, ".csv"), row.names = FALSE)
}
