# Load packages
library(tidyverse)

# Load match data
final_data_with_subs <- read.csv("Dataset/example_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Create a duplicate dataset where player names and ratings are swapped
swapped_data <- final_data_with_subs %>%
  mutate(
    # Swap player names
    across(starts_with("Player"), ~ final_data_with_subs[[gsub("Player", "Opp", cur_column())]]),
    across(starts_with("Opp"), ~ final_data_with_subs[[gsub("Opp", "Player", cur_column())]]),
    
    # Swap ratings
    across(starts_with("Player") & ends_with("_ws_rating"), ~ final_data_with_subs[[gsub("Player", "Opp", cur_column())]]),
    across(starts_with("Opp") & ends_with("_ws_rating"), ~ final_data_with_subs[[gsub("Opp", "Player", cur_column())]])
  )

# Combine original and swapped datasets
final_data_expanded <- bind_rows(final_data_with_subs, swapped_data)


# Create a duplicate dataset where player names and ratings are swapped
swapped_data <- final_data_with_subs

# Swap player names
for (i in 1:12) { 
  swapped_data[[paste0("Player", i, "_Player")]] <- final_data_with_subs[[paste0("Opp", i, "_Player")]]
  swapped_data[[paste0("Opp", i, "_Player")]] <- final_data_with_subs[[paste0("Player", i, "_Player")]]
}

# Swap ratings
for (i in 1:12) { 
  swapped_data[[paste0("Player", i, "_ws_rating")]] <- final_data_with_subs[[paste0("Opp", i, "_ws_rating")]]
  swapped_data[[paste0("Opp", i, "_ws_rating")]] <- final_data_with_subs[[paste0("Player", i, "_ws_rating")]]
}


# Swap home and away team information
swapped_data <- swapped_data %>%
  mutate(
    Home_Team = final_data_with_subs$Away_Team,
    Away_Team = final_data_with_subs$Home_Team,
    Home_Score = final_data_with_subs$Away_Score,
    Away_Score = final_data_with_subs$Home_Score
  )

# Swap outcome (win/loss/draw)
swapped_data <- swapped_data %>%
  mutate(outcome = case_when(
    outcome == "win" ~ "loss",
    outcome == "loss" ~ "win",
    TRUE ~ outcome  # Draw remains unchanged
  ))

# Combine original and swapped datasets
final_data_expanded <- bind_rows(final_data_with_subs, swapped_data)


# Transform data to long format and replace 'Star' player ratings
df_long <- final_data_expanded %>% 
  # Select relevant columns, including match details and player ratings
  select(Match_Date, Home_Team, Away_Team, outcome, League, 
         Player1_ws_rating, Player2_ws_rating, Player3_ws_rating, Player4_ws_rating, Player5_ws_rating,
         Player6_ws_rating, Player7_ws_rating, Player8_ws_rating, Player9_ws_rating, Player10_ws_rating,
         Player11_ws_rating, Player12_ws_rating, Opp1_ws_rating, Opp2_ws_rating, Opp3_ws_rating, 
         Opp4_ws_rating, Opp5_ws_rating, Opp6_ws_rating, Opp7_ws_rating, Opp8_ws_rating, Opp9_ws_rating, 
         Opp10_ws_rating, Opp11_ws_rating, Opp12_ws_rating) %>% 
  # Group data by match to calculate statistics per game
  group_by(Match_Date, Home_Team, Away_Team) %>%
  mutate(
    # Calculate the mean rating across home team players only
    mean = mean(c_across(starts_with("Player")), na.rm = TRUE),  
    # Calculate the standard deviation of ratings for home team players only
    sd = sd(c_across(starts_with("Player")), na.rm = TRUE)
  ) %>% 
  # Convert home team player ratings into long format for easier processing
  pivot_longer(
    cols = starts_with("Player"),  # Only modify home team players
    names_to = 'Player', # Column to store player names
    values_to = 'Rating' # Column to store corresponding ratings
  ) %>% 
  # Identify "star" players based on their performance compared to the team average
  mutate(
    Flag = ifelse(Rating > mean + 2*sd, "Star", "Average"),  # Mark players as "Star" if they exceed mean + 2SD
    Rating2 = ifelse(Flag == 'Star', mean, Rating)  # Replace star player rating with the team mean
  ) %>%
  ungroup()

# Descriptive Statistics --------------------------------------------------
# Compute team ratings (game-level)
team_rating_summary <- final_data_expanded %>%
  rowwise() %>%
  mutate(team_avg_rating = mean(c_across(starts_with("Player") & ends_with("_ws_rating")), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(outcome) %>%
  summarise(
    num_games = n(),
    avg_team_rating = mean(team_avg_rating, na.rm = TRUE),
    sd_team_rating = sd(team_avg_rating, na.rm = TRUE)
  )

# Compute individual ratings (player-level)
individual_rating_summary <- final_data_expanded %>%
  pivot_longer(cols = starts_with("Player") & ends_with("_ws_rating"),
               names_to = "player",
               values_to = "rating") %>%
  filter(!is.na(rating)) %>%  # Exclude missing values
  group_by(outcome) %>%
  summarise(
    num_players = n(),  # Number of total player ratings
    avg_individual_rating = mean(rating, na.rm = TRUE),
    sd_individual_rating = sd(rating, na.rm = TRUE)
  )

# Merge both summaries
team_rating_summary <- left_join(team_rating_summary, individual_rating_summary, by = "outcome")

print(team_rating_summary)

# --- League-Level Ratings ---
# Compute team ratings (game-level)
league_team_rating_summary <- final_data_expanded %>%
  rowwise() %>%
  mutate(team_avg_rating = mean(c_across(starts_with("Player") & ends_with("_ws_rating")), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(League, outcome) %>%
  summarise(
    num_games = n(),
    avg_team_rating = mean(team_avg_rating, na.rm = TRUE),
    sd_team_rating = sd(team_avg_rating, na.rm = TRUE)
  )

# Compute individual ratings (player-level)
league_individual_rating_summary <- final_data_expanded %>%
  pivot_longer(cols = starts_with("Player") & ends_with("_ws_rating"),
               names_to = "player",
               values_to = "rating") %>%
  filter(!is.na(rating)) %>%
  group_by(League, outcome) %>%
  summarise(
    num_players = n(),
    avg_individual_rating = mean(rating, na.rm = TRUE),
    sd_individual_rating = sd(rating, na.rm = TRUE)
  )

# Merge both summaries
league_team_rating_summary <- left_join(league_team_rating_summary, league_individual_rating_summary, by = c("League", "outcome"))

print(league_team_rating_summary)

# Number of games with at least one star player
num_games_with_star <- df_long %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  summarise(any_star = any(Flag == "Star", na.rm = TRUE), .groups = "drop") %>%
  summarise(total_games_with_star = sum(any_star, na.rm = TRUE))

print(num_games_with_star)

# Total number of star players flagged
total_star_players <- df_long %>%
  filter(Flag == "Star") %>%
  summarise(total_star_players = n())

print(total_star_players)

# Number of games with multiple star players
num_games_multiple_stars <- df_long %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  summarise(num_stars = sum(Flag == "Star", na.rm = TRUE), .groups = "drop") %>%
  summarise(games_with_multiple_stars = sum(num_stars > 1))

print(num_games_multiple_stars)

# Average rating and SD for star players
star_player_ratings <- df_long %>%
  filter(Flag == "Star") %>%
  summarise(
    avg_star_rating = mean(Rating, na.rm = TRUE),
    sd_star_rating = sd(Rating, na.rm = TRUE)
  )

print(star_player_ratings)

### Splitting by league
# Number of games with at least one star player
num_games_with_star <- df_long %>%
  group_by(League, Match_Date, Home_Team, Away_Team) %>%
  summarise(any_star = any(Flag == "Star", na.rm = TRUE), .groups = "drop") %>%
  group_by(League) %>% 
  summarise(total_games_with_star = sum(any_star, na.rm = TRUE))

print(num_games_with_star)

# Total number of star players flagged
total_star_players_by_league <- df_long %>%
  filter(Flag == "Star") %>%
  group_by(League) %>%
  summarise(total_star_players = n(), .groups = "drop")

print(total_star_players_by_league)

# Number of games with multiple star players
num_games_multiple_stars <- df_long %>%
  group_by(League, Match_Date, Home_Team, Away_Team) %>%
  summarise(num_stars = sum(Flag == "Star", na.rm = TRUE), .groups = "drop") %>%
  group_by(League) %>% 
  summarise(games_with_multiple_stars = sum(num_stars > 1))

print(num_games_multiple_stars)

# Average rating and SD for star players
star_player_ratings_by_league <- df_long %>%
  filter(Flag == "Star") %>%
  group_by(League) %>%
  summarise(
    avg_star_rating = mean(Rating, na.rm = TRUE),
    sd_star_rating = sd(Rating, na.rm = TRUE),
    .groups = "drop"
  )

print(star_player_ratings_by_league)



df_long <- df_long %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  mutate(
    RowID = row_number(),
    Team = if_else(RowID <= 12, Home_Team, Away_Team)
  ) %>%
  ungroup()

teams_with_star <- df_long %>%
  filter(Flag == "Star") %>%
  mutate(Match_Team_ID = paste(Match_Date, Team)) %>%
  distinct(Match_Team_ID)

df_long <- df_long %>%
  mutate(Match_Team_ID = paste(Match_Date, Team))

team_only_df <- df_long %>%
  filter(!Match_Team_ID %in% teams_with_star$Match_Team_ID)

num_games_with_team_only <- team_only_df %>%
  distinct(Match_Date, Home_Team, Away_Team, Team) %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  summarise(any_team_only = n() > 0, .groups = "drop") %>%
  summarise(total_games_with_team_only = sum(any_team_only))

print(num_games_with_team_only)

total_team_players <- team_only_df %>%
  summarise(total_team_players = n())

print(total_team_players)


### win, loss, draw percentages for overall dataset ##
# Step 1: Get list of matches with at least one star player
star_matches <- df_long %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  summarise(any_star = any(Flag == "Star", na.rm = TRUE), .groups = "drop") %>%
  filter(any_star)  # Keep only matches with a star

# Step 2: Join with original data to get outcomes
star_match_outcomes <- star_matches %>%
  left_join(final_data_expanded, by = c("Match_Date", "Home_Team", "Away_Team")) %>%
  select(Match_Date, Home_Team, Away_Team, outcome)

# Step 3: Calculate outcome proportions
star_outcome_summary <- star_match_outcomes %>%
  count(outcome) %>%
  mutate(percentage = round(100 * n / sum(n), 2))

print(star_outcome_summary)


### Get number of team-only games per league
num_team_games_per_league <- team_only_df %>%
  distinct(Match_Date, Home_Team, Away_Team, League, Team) %>%
  group_by(Match_Date, Home_Team, Away_Team, League) %>%
  summarise(any_team_only = n() > 0, .groups = "drop") %>%
  group_by(League) %>%
  summarise(total_team_only_games = sum(any_team_only))

### Win, Loss, Draw percentages for leagues
# Match-level data with outcome and league
match_info <- df_long %>%
  distinct(Match_Date, Home_Team, Away_Team, outcome, League)

# Games with at least one star
star_games <- df_long %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  summarise(has_star = any(Flag == "Star"), .groups = "drop") %>%
  filter(has_star) %>%
  select(Match_Date, Home_Team, Away_Team) %>%
  mutate(group = "Star")

# Games with no star (team games)
team_games <- df_long %>%
  mutate(Match_Team_ID = paste(Match_Date, Team)) %>%
  filter(!Match_Team_ID %in% teams_with_star$Match_Team_ID) %>%
  distinct(Match_Date, Home_Team, Away_Team) %>%
  mutate(group = "Team")

# Combine and merge with outcomes and leagues
combined_games <- bind_rows(star_games, team_games) %>%
  left_join(match_info, by = c("Match_Date", "Home_Team", "Away_Team"))

print(num_team_games_per_league)


# Count and calculate percentages
percentages_by_league_group <- combined_games %>%
  group_by(League, group, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(League, group) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

print(percentages_by_league_group)

