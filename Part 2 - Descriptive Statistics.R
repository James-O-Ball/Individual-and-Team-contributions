
# Load Packages and Data --------------------------------------------------
library(tidyverse)

final_data_with_subs <- read.csv("Dataset/example_dataset_2.csv", header = TRUE, stringsAsFactors = FALSE)


# Data processing ---------------------------------------------------------

# Create mirrored dataset from the opponent perspective (swap home/away info)
swapped_data <- final_data_with_subs

# Swap each team's player names and ratings (Player <-> Opponent)
for (i in 1:12) {
  swapped_data[[paste0("Player", i, "_Player")]] <- final_data_with_subs[[paste0("Opp", i, "_Player")]]
  swapped_data[[paste0("Opp", i, "_Player")]] <- final_data_with_subs[[paste0("Player", i, "_Player")]]
  swapped_data[[paste0("Player", i, "_ws_rating")]] <- final_data_with_subs[[paste0("Opp", i, "_ws_rating")]]
  swapped_data[[paste0("Opp", i, "_ws_rating")]] <- final_data_with_subs[[paste0("Player", i, "_ws_rating")]]
}

# Swap home and away team information (team names, scores, and match outcome)
swapped_data <- swapped_data %>%
  mutate(
    Home_Team = final_data_with_subs$Away_Team,
    Away_Team = final_data_with_subs$Home_Team,
    Home_Score = final_data_with_subs$Away_Score,
    Away_Score = final_data_with_subs$Home_Score,
    # Invert outcome to reflect reversed perspective
    outcome = case_when(
      outcome == "win" ~ "loss",
      outcome == "loss" ~ "win",
      TRUE ~ outcome # draw remians the same 
    )
  )

# Combine original and mirrored datasets to create a full perspective dataset
final_data_expanded <- bind_rows(final_data_with_subs, swapped_data)


# Identify standout player performances and replace ratings with team average in a long format --------

df_long <- final_data_expanded %>% 
  select(
    Match_Date, Home_Team, Away_Team, outcome, League, Team,
    starts_with("Player"), starts_with("Opp")
  ) %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  mutate(
    # Calculate team-average and standard deviation of player ratings
    mean = mean(c_across(starts_with("Player")), na.rm = TRUE),
    sd = sd(c_across(starts_with("Player")), na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("Player"), # Focus on the Reference team side only (not opponents)
    names_to = "Player",
    values_to = "Rating"
  ) %>%
  mutate(
    # Label standout performances as "Standout" if > 2SD above mean
    Flag = ifelse(Rating > mean + 2 * sd, "Standout", "Average"),
    # Replace "standout" player rating with average to assess counterfactual
    Rating2 = ifelse(Flag == "Standout", mean, Rating)
  ) %>%
  ungroup()

# Table 1 Overall player rating summary ----------------------------------------------------

# Overall (all-league) individual player rating summary
individual_rating_summary <- final_data_expanded %>%
  pivot_longer(cols = matches("^Player.*_ws_rating$"), # select all player rating columns
               names_to = "player",
               values_to = "rating") %>%
  filter(!is.na(rating)) %>%
  group_by(outcome) %>% # Group by match outcome
  summarise(
    num_players = n(), # Total number of player ratings per outcome
    avg_individual_rating = mean(rating, na.rm = TRUE), # Mean player rating
    sd_individual_rating = sd(rating, na.rm = TRUE) # Rating variability
  )

print(individual_rating_summary)

# Compute individual ratings (player-level) by League and outcome
league_individual_rating_summary <- final_data_expanded %>%
  pivot_longer(cols = matches("^Player.*_ws_rating$"),
               names_to = "player",
               values_to = "rating") %>%
  filter(!is.na(rating)) %>%
  group_by(League, outcome) %>% # Sratify by both league and match outcome
  summarise(
    num_players = n(),
    avg_individual_rating = mean(rating, na.rm = TRUE),
    sd_individual_rating = sd(rating, na.rm = TRUE)
  )

print(league_individual_rating_summary)

# Table 2 - Outcome percentages by performance type -----------------------

# Identify unique team matches that include at least one standout player
teams_with_star <- df_long %>%
  filter(Flag == "Standout") %>%
  mutate(Match_Team_ID = paste(Match_Date, Team)) %>% # Unique identifier per team per match
  distinct(Match_Team_ID)

# Classify games as "Standout" if either team had at least one standout player
star_games <- df_long %>%
  group_by(Match_Date, Home_Team, Away_Team) %>%
  summarise(has_star = any(Flag == "Standout"), .groups = "drop") %>%
  filter(has_star) %>%
  select(Match_Date, Home_Team, Away_Team) %>%
  mutate(group = "Standout") # Label Group

# Classify remaining games (without any standout players) as "Team" performance
team_games <- df_long %>%
  mutate(Match_Team_ID = paste(Match_Date, Team)) %>%
  filter(!Match_Team_ID %in% teams_with_star$Match_Team_ID) %>%
  distinct(Match_Date, Home_Team, Away_Team) %>%
  mutate(group = "Team")

# Extract distinct match-level info (outcome + League) to join with group labels
match_info <- df_long %>%
  distinct(Match_Date, Home_Team, Away_Team, outcome, League)

# Combine both "Standout" and "Team" groups with match info
combined_games <- bind_rows(star_games, team_games) %>%
  left_join(match_info, by = c("Match_Date", "Home_Team", "Away_Team"))

# Calculate outcome percentages within each group and league
percentages_by_league_group <- combined_games %>%
  group_by(League, group, outcome) %>%
  summarise(n = n(), .groups = "drop") %>% # Count outcomes
  group_by(League, group) %>%
  mutate(percent = round(n / sum(n) * 100, 2)) # Convert to %

print(percentages_by_league_group)
