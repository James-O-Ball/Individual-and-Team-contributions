# Position Descriptives

# Load Packages and Data --------------------------------------------------
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


# --- Step 1: Extract PlayerKey from Player column ---
df_long <- df_long %>%
  mutate(PlayerKey = gsub("_Rating", "", Player))  # e.g., "Home_Player1"

# --- Step 2: Dynamically get the PlayerName using PlayerKey ---
df_long <- df_long %>%
  rowwise() %>%
  mutate(PlayerName = get(paste0(PlayerKey, "_PlayerName"))) %>%
  ungroup()

# --- Step 3: Filter standout players ---
df_standouts <- df_long %>%
  filter(Flag == "Standout", !is.na(Rating), !is.na(PlayerName))

# --- Step 4: Join with original all_stats dataset to get Position ---
df_standouts_pos <- df_standouts %>%
  left_join(all_stats, by = c("match_id", "team", "Date", "PlayerName"))

# --- Step 5: Count standout players by Position ---
standout_position_counts <- df_standouts_pos %>%
  count(Position, sort = TRUE)

print(standout_position_counts)

# --- Step 6: Check for missing positions ---
df_missing_pos <- df_standouts_pos %>%
  filter(is.na(Position)) %>%
  select(PlayerName, match_id, team, Date) %>%
  distinct()

print(df_missing_pos)


# Goalkeeper descriptives -------------------------------------------------

gk_outcomes <- df_standouts_pos %>%
  filter(Position == "GK") %>%             # keep only goalkeepers
  group_by(team_outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = round(100 * n / sum(n), 1))

print(gk_outcomes)
# Overall -----------------------------------------------------------------

# Load match data
prem_22_23 <- read.csv("WhoScored/Prem_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
prem_23_24 <- read.csv("WhoScored/Prem_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_22_23 <- read.csv("WhoScored/Laliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_23_24 <- read.csv("WhoScored/LaLiga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
bund_22_23 <- read.csv("WhoScored/Bundesliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
bund_23_24 <- read.csv("WhoScored/Bundesliga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)


# Combine all data and remove players with <10 mins
all_stats <- bind_rows(prem_22_23, prem_23_24,
                       laliga_22_23, laliga_23_24,
                       bund_22_23, bund_23_24) %>%
  mutate(team = if_else(home_away == "home", hometeam,
                        if_else(home_away == "away", awayteam, NA_character_)))

# Split Sub into those that played vs did not play
all_stats <- all_stats %>%
  mutate(Position2 = case_when(
    Position == "Sub" & Min == 0 ~ "Sub_DNP",
    Position == "Sub" & Min > 0  ~ "Sub_Played",
    TRUE                         ~ Position
  ))

# Create position breakdown
position_breakdown <- all_stats %>%
  group_by(match_id, Position2) %>%   # Count positions within each game
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Position2) %>%             # Sum across all games
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total))

position_breakdown

