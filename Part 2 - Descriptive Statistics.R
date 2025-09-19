library(tidyverse)

# ------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------
final_data_all_players <- read.csv("WhoScored/raw_wide_all_players.csv", header = TRUE, stringsAsFactors = FALSE)

# ------------------------------------------------------------------------
# Create mirrored dataset for Away perspective
# ------------------------------------------------------------------------
swapped_data <- final_data_all_players
home_cols <- grep("^Home_Player", names(final_data_all_players), value = TRUE)
away_cols <- grep("^Away_Player", names(final_data_all_players), value = TRUE)

# Swap player columns and match info
swapped_data[home_cols] <- final_data_all_players[away_cols]
swapped_data[away_cols] <- final_data_all_players[home_cols]

swapped_data <- swapped_data %>%
  mutate(
    hometeam = final_data_all_players$awayteam,
    awayteam = final_data_all_players$hometeam,
    Home_Score = final_data_all_players$Away_Score,
    Away_Score = final_data_all_players$Home_Score,
    outcome = case_when(
      outcome == "win" ~ "loss",
      outcome == "loss" ~ "win",
      TRUE ~ outcome
    ),
    team = final_data_all_players$awayteam
  )

final_data_all_players <- final_data_all_players %>% mutate(perspective = "original")
swapped_data <- swapped_data %>% mutate(perspective = "mirrored")

# Combine both perspectives
final_data_expanded <- bind_rows(final_data_all_players, swapped_data)

# ------------------------------------------------------------------------
# Compute team-level stats in wide format
# ------------------------------------------------------------------------
team_stats <- final_data_expanded %>%
  filter(perspective == "original") %>%
  select(match_id, hometeam, awayteam, starts_with("Home_Player"), starts_with("Away_Player")) %>%
  rowwise() %>%
  mutate(
    home_mean = mean(c_across(starts_with("Home_Player") & ends_with("_Rating")), na.rm = TRUE),
    home_sd = sd(c_across(starts_with("Home_Player") & ends_with("_Rating")), na.rm = TRUE),
    away_mean = mean(c_across(starts_with("Away_Player") & ends_with("_Rating")), na.rm = TRUE),
    away_sd = sd(c_across(starts_with("Away_Player") & ends_with("_Rating")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(match_id, hometeam, awayteam, home_mean, home_sd, away_mean, away_sd)

# ------------------------------------------------------------------------
# Build Long Dataset Including Both Teams for Standout Detection
# ------------------------------------------------------------------------
df_long <- final_data_expanded %>%
  select(match_id, Date, hometeam, awayteam, Home_Score, Away_Score, outcome, league, perspective,
         starts_with("Home_Player"), starts_with("Away_Player")) %>%
  pivot_longer(cols = matches("(Home|Away)_Player\\d+_Rating"), names_to = "Player", values_to = "Rating") %>%
  mutate(
    is_home = grepl("^Home", Player),
    team = ifelse(is_home, hometeam, awayteam)
  ) %>%
  left_join(team_stats, by = c("match_id", "hometeam", "awayteam")) %>%
  mutate(
    group_mean = ifelse(is_home, home_mean, away_mean),
    group_sd = ifelse(is_home, home_sd, away_sd),
    Flag = ifelse(!is.na(Rating) & Rating > group_mean + 2 * group_sd, "Standout", "Average"),
    team_outcome = case_when(
      perspective == "original" & team == hometeam & Home_Score > Away_Score ~ "win",
      perspective == "original" & team == hometeam & Home_Score < Away_Score ~ "loss",
      perspective == "original" & team == awayteam & Away_Score > Home_Score ~ "win",
      perspective == "original" & team == awayteam & Away_Score < Home_Score ~ "loss",
      TRUE ~ "draw"
    )
  )

# Average and SD for standout players
standout_summary <- df_long %>%
  filter(Flag == "Standout") %>%
  summarise(
    n = n(),
    mean_rating = mean(Rating, na.rm = TRUE),
    sd_rating = sd(Rating, na.rm = TRUE)
  )

standout_summary

# ------------------------------------------------------------------------
# Table 1: Overall Player Rating Summary (Team Perspective)
# ------------------------------------------------------------------------
individual_rating_summary <- df_long %>%
  filter(perspective == "original") %>%
  group_by(team_outcome) %>%
  summarise(
    num_players = sum(!is.na(Rating)),
    avg_individual_rating = mean(Rating, na.rm = TRUE),
    sd_individual_rating = sd(Rating, na.rm = TRUE),
    .groups = "drop"
  )

print(individual_rating_summary)

# By league and outcome
league_individual_rating_summary <- df_long %>%
  filter(perspective == "original") %>%
  group_by(league, team_outcome) %>%
  summarise(
    num_players = sum(!is.na(Rating)),
    avg_individual_rating = mean(Rating, na.rm = TRUE),
    sd_individual_rating = sd(Rating, na.rm = TRUE),
    .groups = "drop"
  )

print(league_individual_rating_summary)

# ------------------------------------------------------------------------
# Table 2: Outcome Percentages by Performance Type
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Create goal difference bins instead of win/draw/loss
# ------------------------------------------------------------------------

# Step 1: Calculate team-level goal difference
goal_diff_df <- df_long %>%
  filter(perspective == "original") %>%
  distinct(match_id, team, Home_Score, Away_Score, hometeam, awayteam) %>%
  mutate(
    goal_diff = case_when(
      team == hometeam ~ Home_Score - Away_Score,
      team == awayteam ~ Away_Score - Home_Score
    )
  )

# Step 2: Bin goal difference
goal_diff_df <- goal_diff_df %>%
  mutate(
    goal_diff_bin = case_when(
      goal_diff <= -3 ~ "-3+",
      goal_diff == -2 ~ "-2",
      goal_diff == -1 ~ "-1",
      goal_diff == 0  ~ "0",
      goal_diff == 1  ~ "1",
      goal_diff == 2  ~ "2",
      goal_diff >= 3  ~ "+3"
    )
  )

# Step 3: Merge with standout classification
combined_games <- df_long %>%
  filter(perspective == "original") %>%
  distinct(match_id, team, league) %>%
  left_join(
    df_long %>%
      filter(perspective == "original") %>%
      group_by(match_id, team) %>%
      summarise(has_star = any(Flag == "Standout"), .groups = "drop"),
    by = c("match_id", "team")
  ) %>%
  left_join(goal_diff_df, by = c("match_id", "team")) %>%
  mutate(
    has_star = ifelse(is.na(has_star), FALSE, has_star),
    group = ifelse(has_star, "Standout", "Team"),
    goal_diff_bin = factor(goal_diff_bin, 
                           levels = c("-3+", "-2", "-1", "0", "1", "2", "+3"))
  )

# Step 4: Percentages by league and group
percentages_by_league_group <- combined_games %>%
  group_by(league, group, goal_diff_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(league, group) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

print(percentages_by_league_group)

# Step 5: Overall percentages
percentages_overall <- combined_games %>%
  group_by(group, goal_diff_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

print(percentages_overall)

# ------------------------------------------------------------------------
# Additional Summaries and Table 3
# ------------------------------------------------------------------------

# Helper: parse date & label season ONCE
df_base <- df_long %>%
  mutate(
    Date   = as.Date(Date, format = "%d/%m/%Y"),
    season = ifelse(Date >= as.Date("2023-08-01"), "2023-24", "2022-23")
  )

# Keep only original-perspective rows
df_orig <- df_base %>% filter(perspective == "original")

# -----------------------
# 1) Standout count per team per match (teams with >=1 standout)
# -----------------------
standout_count <- df_orig %>%
  group_by(match_id, Date, team, hometeam, awayteam) %>%
  summarise(num_standout = sum(Flag == "Standout", na.rm = TRUE), .groups = "drop") %>%
  filter(num_standout > 0) %>%
  mutate(home_away = if_else(team == hometeam, "Home", "Away"))

nrow(standout_count)  # total team–matches with ≥1 standout

# Home/Away breakdown (counts + % within Home/Away)
dist_home_away <- standout_count %>%
  count(home_away, num_standout, name = "n") %>%
  group_by(home_away) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup()

print(dist_home_away)

# Season breakdown (counts + % within season)
dist_season <- standout_count %>%
  left_join(df_orig %>% distinct(match_id, season), by = "match_id") %>%
  count(season, num_standout, name = "n") %>%
  group_by(season) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  ungroup()

print(dist_season)

# -----------------------
# 2) Standout player rating summaries
# -----------------------
standouts_only <- df_orig %>% filter(Flag == "Standout")

standout_summary <- standouts_only %>%
  summarise(
    total_standout_players = n(),
    avg_standout_rating    = mean(Rating, na.rm = TRUE),
    sd_standout_rating     = sd(Rating,   na.rm = TRUE),
    .groups = "drop"
  )

print(standout_summary)

standout_summary_league <- standouts_only %>%
  group_by(league) %>%
  summarise(
    total_standout_players = n(),
    avg_standout_rating    = mean(Rating, na.rm = TRUE),
    sd_standout_rating     = sd(Rating,   na.rm = TRUE),
    .groups = "drop"
  )

print(standout_summary_league)

# -----------------------
# 3) Detailed summaries by Home/Away & by Season,
#    considering how many standouts the team had in that match
# -----------------------

# Label each standout with home/away and the team’s standout count in that match
standout_detailed <- standouts_only %>%
  mutate(home_away = if_else(team == hometeam, "Home", "Away")) %>%
  add_count(match_id, team, name = "num_standouts")  # fast team-level count

# (a) Home/Away × number of standouts
summary_home_away <- standout_detailed %>%
  group_by(home_away, num_standouts) %>%
  summarise(
    n_teams    = n_distinct(paste(match_id, team)),   # team–matches
    n_players  = n(),                                 # standout players
    avg_rating = round(mean(Rating, na.rm = TRUE), 2),
    sd_rating  = round(sd(Rating,   na.rm = TRUE), 2),
    .groups = "drop"
  )

print(summary_home_away)

# (b) Season × number of standouts
summary_season <- standout_detailed %>%
  group_by(season, num_standouts) %>%
  summarise(
    n_teams    = n_distinct(paste(match_id, team)),
    n_players  = n(),
    avg_rating = round(mean(Rating, na.rm = TRUE), 2),
    sd_rating  = round(sd(Rating,   na.rm = TRUE), 2),
    .groups = "drop"
  )

print(summary_season)


# -----------------------
# Reviewer Comment: Does a standout performance in the home team related to a standout in the away
# -----------------------
# Match-level flags: whether each team had ≥1 standout
standout_flags <- df_orig %>%
  group_by(match_id, hometeam, awayteam) %>%
  summarise(
    home_has_star = any(team == hometeam & Flag == "Standout"),
    away_has_star = any(team == awayteam & Flag == "Standout"),
    .groups = "drop"
  )

# Quick cross-tabulation
table(standout_flags$home_has_star, standout_flags$away_has_star)

# Phi coefficient / correlation
cor(as.numeric(standout_flags$home_has_star),
    as.numeric(standout_flags$away_has_star))


# Goalkeeper analysis -----------------------------------------------------

# 1. Identify which positions had standouts per team per match
standout_by_pos <- df_long %>%
  filter(Flag == "Star") %>%
  group_by(match_id, team, Position) %>%
  summarise(has_standout = 1, .groups = "drop")

# 2. Collapse to team level: did this team have a standout GK / Defender / Mid / Att / Sub?
standout_flags <- standout_by_pos %>%
  mutate(Position_group = case_when(
    Position %in% c("GK", "Goalkeeper") ~ "Goalkeeper",
    Position %in% c("DC", "DL", "DR", "Defender") ~ "Defender",
    Position %in% c("MC", "ML", "MR", "Midfielder") ~ "Midfielder",
    Position %in% c("FW", "FWR", "FWL", "Attacker") ~ "Attacker",
    Position == "Sub" ~ "Substitute",
    TRUE ~ "Other"
  )) %>%
  distinct(match_id, team, Position_group, has_standout) %>%
  tidyr::pivot_wider(
    names_from = Position_group,
    values_from = has_standout,
    values_fill = 0
  )

# 3. Join back to outcomes
standout_outcomes <- df_long %>%
  distinct(match_id, team, outcome) %>%
  left_join(standout_flags, by = c("match_id","team"))

# 4. Summarise outcomes when GK standout vs not
gk_summary <- standout_outcomes %>%
  group_by(Goalkeeper, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Goalkeeper) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

print(gk_summary)
