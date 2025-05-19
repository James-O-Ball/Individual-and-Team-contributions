# Load Packages
library(tidyverse)

# Load match data
eng_all_stats <- read.csv("Dataset/prem_example", header = TRUE, stringsAsFactors = FALSE)
ger_all_stats <- read.csv("Dataset/bundesliga_example.csv", header = TRUE, stringsAsFactors = FALSE)
esp_all_stats <- read.csv("Dataset/laliga_example.csv", header = TRUE, stringsAsFactors = FALSE)

# Combine dataframes
all_stats <- bind_rows(eng_all_stats, ger_all_stats, esp_all_stats)

# Create outcome column based on home and away scores
all_stats <- all_stats %>%
  mutate(
    Team = str_to_lower(Team),        # Convert to lowercase
    Home_Team = str_to_lower(Home_Team),
    Away_Team = str_to_lower(Away_Team)
  ) %>%
  mutate(outcome = case_when(
    Team == Home_Team & Home_Score > Away_Score ~ "win",
    Team == Home_Team & Home_Score < Away_Score ~ "loss",
    Team == Home_Team & Home_Score == Away_Score ~ "draw",
    Team == Away_Team & Away_Score > Home_Score ~ "win",
    Team == Away_Team & Away_Score < Home_Score ~ "loss",
    Team == Away_Team & Away_Score == Home_Score ~ "draw",
    TRUE ~ NA_character_ # Handle unexpected cases
  ))

all_stats <- all_stats %>% 
  mutate(Team = ifelse(Team == "brighton  hove albion", "brighton hove albion", Team))

# Separate substitutes and calculate average ratings (direct calculation for both home and away)
substitutes_home <- all_stats %>%
  filter(ws_position == "Sub", Team == Home_Team) %>%
  group_by(MatchURL) %>%
  summarise(avg_ws_rating_sub_home = mean(ws_rating, na.rm = TRUE), .groups = "drop")

substitutes_away <- all_stats %>%
  filter(ws_position == "Sub", Team == Away_Team) %>%
  group_by(MatchURL) %>%
  summarise(avg_ws_rating_sub_away = mean(ws_rating, na.rm = TRUE), .groups = "drop")

# Filter out substitutes and keep only starting players
all_stats_filtered <- all_stats %>%
  filter(ws_position != "Sub")

# Create player rating columns (to later pivot)
df <- all_stats_filtered %>%
  select(MatchURL, League, Match_Date, Home_Team, Away_Team, Home_Score, Away_Score, Home_Away, outcome, 
         Player, ws_rating) 

# Create a new column 'Team' to indicate whether the player is on the home or away team
df <- df %>%
  mutate(Team = case_when(
    Home_Away == "Home" ~ Home_Team,  # If 'Home_Away' is Home, assign to Home_Team
    Home_Away == "Away" ~ Away_Team,  # If 'Home_Away' is Away, assign to Away_Team
    TRUE ~ NA_character_  # Handle any other cases (if any)
  ))

# Filter home players
home_players <- df %>%
  filter(Team == Home_Team) %>%
  group_by(MatchURL) %>%
  mutate(PlayerID = paste0('Player', row_number())) %>%
  select(MatchURL, PlayerID, Player, ws_rating) %>%
  ungroup()

# Filter away players
away_players <- df %>%
  filter(Team == Away_Team) %>%
  group_by(MatchURL) %>%
  mutate(PlayerID = paste0('Opp', row_number())) %>%
  select(MatchURL, PlayerID, Player, ws_rating) %>%
  ungroup()

# Combine home and away players
combined_players <- bind_rows(home_players, away_players)

# Pivot the data to a wide format
wide_data <- combined_players %>%
  pivot_wider(names_from = PlayerID, values_from = c(Player, ws_rating), names_glue = "{PlayerID}_{.value}")

# Merge substitutes' average rating (12th man) and directly create the desired columns
final_data <- df %>%
  filter(Home_Away == "Home") %>%  # Keep only "Home" rows for match outcomes
  select(MatchURL, League, Match_Date, Home_Team, Away_Team, Home_Score, Away_Score, outcome) %>%
  distinct() %>%  # Remove duplicates
  left_join(wide_data, by = "MatchURL") %>%  # Join with the wide data
  left_join(substitutes_home, by = "MatchURL") %>%  # Join home team substitute averages
  left_join(substitutes_away, by = "MatchURL")  # Join away team substitute averages

# Add the specific columns for Player12 and Opp12
final_data$Player12_ws_rating <- final_data$avg_ws_rating_sub_home
final_data$Opp12_ws_rating <- final_data$avg_ws_rating_sub_away

# Remove the unwanted columns
final_data <- final_data[, !(names(final_data) %in% c("avg_ws_rating_sub_home", "avg_ws_rating_sub_away"))]

# View the result
head(final_data)

write.csv(final_data, "Dataset/wide_player_ratings_with_avg_subs.csv")
