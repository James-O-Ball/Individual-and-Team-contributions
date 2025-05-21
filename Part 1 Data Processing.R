# Load Packages & Data ----------------------------------------------------

library(tidyverse)

all_stats <- read.csv("Dataset/example_dataset_1.csv", header = TRUE, stringsAsFactors = FALSE)

# Filter Starters & Create Team Column ------------------------------------

# Keep only starting players (exclude substitutes) and create a 'Team' column to reference who each player plays for
df <- all_stats %>%
  filter(ws_position != "Sub") %>%
  mutate(Team = if_else(Home_Away == "Home", Home_Team,
                        if_else(Home_Away == "Away", Away_Team, NA_character_))) %>%
  select(MatchURL, League, Match_Date, Home_Team, Away_Team,
         Home_Score, Away_Score, Home_Away, outcome, Player, ws_rating, Team)

# Reshape Player Ratings: Home and Away -----------------------------------

# Create player IDs and reshape to wide format for both home and away teams
player_data <- df %>%
  mutate(PlayerID = case_when(
    Team == Home_Team ~ paste0("Player", row_number()),
    Team == Away_Team ~ paste0("Opp", row_number())
  )) %>%
  group_by(MatchURL, Team) %>%
  mutate(PlayerID = if_else(Team == Home_Team,
                            paste0("Player", row_number()),
                            paste0("Opp", row_number()))) %>%
  ungroup() %>%
  select(MatchURL, PlayerID, Player, ws_rating) %>%
  pivot_wider(names_from = PlayerID, values_from = c(Player, ws_rating), names_glue = "{PlayerID}_{.value}")


# Calculate Average Substitute Ratings ------------------------------------

# Calculate average ratings for each team per match
subs_avg <- all_stats %>%
  filter(ws_position == "Sub") %>%
  mutate(TeamType = if_else(Team == Home_Team, "Home", "Away")) %>%
  group_by(MatchURL, TeamType) %>%
  summarise(avg_ws_rating_sub = mean(ws_rating, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = TeamType, values_from = avg_ws_rating_sub,
              names_glue = "avg_ws_rating_sub_{TeamType}")

# Merge All Components ----------------------------------------------------

# Merge everything into one final wide-format dataset
final_data <- df %>%
  filter(Home_Away == "Home") %>% # Use only home rows for unique match-level info
  distinct(MatchURL, League, Match_Date, Home_Team, Away_Team,
           Home_Score, Away_Score, outcome) %>%
  left_join(player_data, by = "MatchURL") %>% # Add reshaped player ratings
  left_join(subs_avg, by = "MatchURL") %>% # Add avg sub ratings
  rename(
    Player12_ws_rating = avg_ws_rating_sub_Home,
    Opp12_ws_rating = avg_ws_rating_sub_Away
  )

# Export Final Data -------------------------------------------------------

# Save the cleaned and reshaped dataset to CSV
write.csv(final_data, "Dataset/wide_player_ratings_with_avg_subs.csv", row.names = FALSE)