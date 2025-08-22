library(tidyverse)

# Remove players >10min and weight ---------------------------------------------------

# Load match data
prem_22_23 <- read.csv("WhoScored/Prem_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
prem_23_24 <- read.csv("WhoScored/Prem_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_22_23 <- read.csv("WhoScored/Laliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_23_24 <- read.csv("WhoScored/LaLiga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
bund_22_23 <- read.csv("WhoScored/Bundesliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
bund_23_24 <- read.csv("WhoScored/Bundesliga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)


# 1. Combine all data and remove players with <10 mins
all_stats <- bind_rows(prem_22_23, prem_23_24,
                       laliga_22_23, laliga_23_24,
                       bund_22_23, bund_23_24) %>%
  #filter(Min >= 10) %>%
  mutate(WeightedRating = Rating * (Min / 90),
         team = if_else(home_away == "home", hometeam,
                        if_else(home_away == "away", awayteam, NA_character_)))

# 2. Create index per team per match
all_stats <- all_stats %>%
  group_by(match_id, team) %>%
  arrange(match_id, team, desc(Position != "Sub"), desc(Min)) %>% # starters first
  mutate(PlayerIndex = row_number()) %>%
  ungroup()

# 3. Separate home and away players
home_players <- all_stats %>%
  filter(home_away == "home") %>%
  mutate(PlayerID = paste0("Home_Player", PlayerIndex)) %>%
  select(match_id, PlayerID, PlayerName, WeightedRating)

away_players <- all_stats %>%
  filter(home_away == "away") %>%
  mutate(PlayerID = paste0("Away_Player", PlayerIndex)) %>%
  select(match_id, PlayerID, PlayerName, WeightedRating)

# 4. Pivot to wide for each team
home_wide <- home_players %>%
  pivot_wider(names_from = PlayerID,
              values_from = c(PlayerName, WeightedRating),
              names_glue = "{PlayerID}_{.value}")

away_wide <- away_players %>%
  pivot_wider(names_from = PlayerID,
              values_from = c(PlayerName, WeightedRating),
              names_glue = "{PlayerID}_{.value}")

# 5. Merge match info with player data
match_info <- all_stats %>%
  filter(home_away == "home") %>% # one row per match
  distinct(match_id, league, Date, hometeam, awayteam, Home_Score, Away_Score, outcome)

final_data <- match_info %>%
  left_join(home_wide, by = "match_id") %>%
  left_join(away_wide, by = "match_id")


# 6. Export
write.csv(final_data, "WhoScored/mins_removed_weighted_wide_all_players.csv", row.names = FALSE)



# Remove players >10min (raw score) ---------------------------------------

library(tidyverse)

# Load match data
prem_22_23 <- read.csv("WhoScored/Prem_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
prem_23_24 <- read.csv("WhoScored/Prem_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_22_23 <- read.csv("WhoScored/Laliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_23_24 <- read.csv("WhoScored/LaLiga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
bund_22_23 <- read.csv("WhoScored/Bundesliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
bund_23_24 <- read.csv("WhoScored/Bundesliga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)



# 1. Combine all data and remove players with <10 mins
all_stats <- bind_rows(prem_22_23, prem_23_24,
                       laliga_22_23, laliga_23_24,
                       bund_22_23, bund_23_24) %>%
  mutate(team = if_else(home_away == "home", hometeam,
                        if_else(home_away == "away", awayteam, NA_character_)))

# 2. Create index per team per match
all_stats <- all_stats %>%
  group_by(match_id, team) %>%
  arrange(match_id, team, desc(Position != "Sub"), desc(Min)) %>%  # starters first
  mutate(PlayerIndex = row_number()) %>%
  ungroup()

# 3. Separate home and away players
home_players <- all_stats %>%
  filter(home_away == "home") %>%
  mutate(PlayerID = paste0("Home_Player", PlayerIndex)) %>%
  select(match_id, PlayerID, PlayerName, Rating)

away_players <- all_stats %>%
  filter(home_away == "away") %>%
  mutate(PlayerID = paste0("Away_Player", PlayerIndex)) %>%
  select(match_id, PlayerID, PlayerName, Rating)

# 4. Pivot to wide for each team
home_wide <- home_players %>%
  pivot_wider(names_from = PlayerID,
              values_from = c(PlayerName, Rating),
              names_glue = "{PlayerID}_{.value}")

away_wide <- away_players %>%
  pivot_wider(names_from = PlayerID,
              values_from = c(PlayerName, Rating),
              names_glue = "{PlayerID}_{.value}")

# 5. Merge match info with player data
match_info <- all_stats %>%
  filter(home_away == "home") %>%
  distinct(match_id, league, Date, hometeam, awayteam,
           Home_Score, Away_Score, outcome)

final_data <- match_info %>%
  left_join(home_wide, by = "match_id") %>%
  left_join(away_wide, by = "match_id")

# 6. Export
write.csv(final_data, "WhoScored/raw_wide_all_players.csv", row.names = FALSE)
