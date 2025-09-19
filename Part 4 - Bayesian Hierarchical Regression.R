# Load Packages
library(tidyverse)
library(ggplot2)
library(brms)
library(tidybayes)
library(posterior)

# Load new wide dataset and prepare baseline long format ------------------

# Load new wide dataset
final_data_all_players <- read.csv("WhoScored/mins_removed_raw_wide_all_players.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert match_id to factor
base_data <- final_data_all_players %>%
  mutate(match_id = as.factor(match_id))

# Home perspective
home_data <- base_data %>%
  transmute(
    match_id,
    team = hometeam,
    opponent = awayteam,
    league,
    home_advantage = 1,
    goal_diff = Home_Score - Away_Score,
    Team_Rating = rowMeans(select(., starts_with("Home_Player") & ends_with("_Rating")), na.rm = TRUE),
    Opp_Rating = rowMeans(select(., starts_with("Away_Player") & ends_with("_Rating")), na.rm = TRUE)
  )

# Away perspective
away_data <- base_data %>%
  transmute(
    match_id,
    team = awayteam,
    opponent = hometeam,
    league,
    home_advantage = 0,
    goal_diff = Away_Score - Home_Score,
    Team_Rating = rowMeans(select(., starts_with("Away_Player") & ends_with("_Rating")), na.rm = TRUE),
    Opp_Rating = rowMeans(select(., starts_with("Home_Player") & ends_with("_Rating")), na.rm = TRUE)
  )

# Combine into long baseline dataset
baseline_long <- bind_rows(home_data, away_data) %>%
  mutate(
    Team_Rating_z = scale(Team_Rating)[, 1],
    Opp_Rating_z = scale(Opp_Rating)[, 1]
  )


# Identify standout players and simulate replacements ---------------------

## -------------------------------
## Helper: season label
## -------------------------------
seasonized <- final_data_all_players %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
         season = if_else(Date >= as.Date("2023-08-01"), "2023-24", "2022-23"))


## ------------------------------------------------------
## Compute venue-specific (and season-specific) subs'
##    typical rating for each team
## ------------------------------------------------------
subs_long <- seasonized %>%
  select(match_id, Date, season, hometeam, awayteam, matches("_Rating$")) %>%
  pivot_longer(cols = matches("_Rating$"),
               names_to = "Player", values_to = "Rating") %>%
  mutate(
    is_home = str_starts(Player, "Home_"),
    team    = if_else(is_home, hometeam, awayteam),
    venue   = if_else(is_home, "Home", "Away"),
    # Identify substitutes by column index (Player12+)
    is_sub  = str_detect(Player, "Player1[2-9]|Player[2-9][0-9]")
  ) %>%
  filter(is_sub) %>%
  group_by(team, venue, season) %>%                     # <-- venue-specific (and season)
  summarise(avg_sub_rating = mean(Rating, na.rm = TRUE),
            .groups = "drop")

## ------------------------------------------------------
## Long data for standout detection (original ratings)
## ------------------------------------------------------
df_long <- seasonized %>%
  select(match_id, league, Date, season, hometeam, awayteam,
         Home_Score, Away_Score, matches("_Rating$")) %>%
  pivot_longer(cols = matches("_Rating$"),
               names_to = "Player", values_to = "Rating") %>%
  mutate(
    is_home = str_starts(Player, "Home_"),
    team    = if_else(is_home, hometeam, awayteam),
    venue   = if_else(is_home, "Home", "Away")
  ) %>%
  group_by(match_id) %>%
  mutate(
    home_mask = is_home,
    # group stats computed separately for Home and Away within the match
    group_mean = if_else(is_home,
                         mean(Rating[home_mask],  na.rm = TRUE),
                         mean(Rating[!home_mask], na.rm = TRUE)),
    group_sd   = if_else(is_home,
                         sd(Rating[home_mask],    na.rm = TRUE),
                         sd(Rating[!home_mask],   na.rm = TRUE)),
    Flag = if_else(!is.na(Rating) & Rating > group_mean + 2 * group_sd,
                   "Star", "Average")
  ) %>%
  ungroup()


## ------------------------------------------------------
## Join venue-specific typical sub ratings and replace
##    only standout players with the matching venue avg
## ------------------------------------------------------
df_long_repl <- df_long %>%
  left_join(subs_long, by = c("team","venue","season")) %>%
  # optional safety fallback: if a (team,venue,season) has no sub appearances,
  # fall back to team-overall substitutes across both venues in that season
  {
    # compute team x season overall subs as fallback
    fallback <- seasonized %>%
      select(match_id, Date, season, hometeam, awayteam, matches("_Rating$")) %>%
      pivot_longer(cols = matches("_Rating$"),
                   names_to = "Player", values_to = "Rating") %>%
      mutate(
        is_home = str_starts(Player, "Home_"),
        team    = if_else(is_home, hometeam, awayteam),
        is_sub  = str_detect(Player, "Player1[2-9]|Player[2-9][0-9]")
      ) %>%
      filter(is_sub) %>%
      group_by(team, season) %>%
      summarise(avg_sub_rating_overall = mean(Rating, na.rm = TRUE), .groups = "drop")
    
    x <- left_join(., fallback, by = c("team","season"))
    x$avg_sub_rating <- ifelse(
      is.na(x$avg_sub_rating), x$avg_sub_rating_overall, x$avg_sub_rating
    )
    select(x, -avg_sub_rating_overall)
  } %>%
  mutate(
    Replacement_Rating = if_else(Flag == "Star", avg_sub_rating, Rating)
)

## ------------------------------------------------------
## Wide matrix of replaced ratings, then compute
##    team/opp ratings for home and away perspectives
## ------------------------------------------------------
df_replaced <- df_long_repl %>%
  select(match_id, Player, Replacement_Rating) %>%
  pivot_wider(names_from = "Player", values_from = "Replacement_Rating")

# Home perspective
home_replaced <- seasonized %>%
  select(match_id, league, hometeam, awayteam, Home_Score, Away_Score) %>%
  left_join(df_replaced, by = "match_id") %>%
  mutate(
    Team_Rating = rowMeans(select(., matches("^Home_Player\\d+_Rating$")), na.rm = TRUE),
    Opp_Rating  = rowMeans(select(., matches("^Away_Player\\d+_Rating$")), na.rm = TRUE)
  ) %>%
  transmute(
    match_id,
    team = hometeam,
    opponent = awayteam,
    league,
    home_advantage = 1,
    goal_diff = Home_Score - Away_Score,
    Team_Rating, Opp_Rating
  )

# Away perspective
away_replaced <- seasonized %>%
  select(match_id, league, hometeam, awayteam, Home_Score, Away_Score) %>%
  left_join(df_replaced, by = "match_id") %>%
  mutate(
    Team_Rating = rowMeans(select(., matches("^Away_Player\\d+_Rating$")), na.rm = TRUE),
    Opp_Rating  = rowMeans(select(., matches("^Home_Player\\d+_Rating$")), na.rm = TRUE)
  ) %>%
  transmute(
    match_id,
    team = awayteam,
    opponent = hometeam,
    league,
    home_advantage = 0,
    goal_diff = Away_Score - Home_Score,
    Team_Rating, Opp_Rating
  )

replacement_long <- bind_rows(home_replaced, away_replaced) %>%
  mutate(
    Team_Rating_z = as.numeric(scale(Team_Rating)),
    Opp_Rating_z  = as.numeric(scale(Opp_Rating))
  )


# Set Priors --------------------------------------------------------------

# Priors
prior_linear <- c(
  # Weakly informative prior for fixed effect coefficients 
  # Centres effects at zero and assumes most standardised predictors will have small to moderate effects
  # Regularises estimates while allowing data-driven deviations when evidence is strong
  prior(normal(0, 1), class = "b"),
  
  # Weakly informative prior for the intercept
  # centres the intercept around plausible average gaol difference values for standardised predictors
  # Prevents unrealistic baseline predictions while allowing flexibility
  prior(normal(0, 1), class = "Intercept"),
  
  # Exponential(1) prior for residual standard deviation (sigma)
  # Shrinks towards smaller residual variation unless strongly supported by the data
  # Improves sampling stability and guards against overfitting
  prior(exponential(1), class = "sigma"),
  
  # Exponential(1) prior for team-level random intercept standard deviation
  # Encourages partial pooling across teams, shrinking extreme team effects towards the group mean
  prior(exponential(1), class = "sd", group = "team"),
  
  
  # Exponential(1) prior for match-level random intercept standard deviation
  # Encourages partial pooling across matches, accounting for fixture-specific variability
  prior(exponential(1), class = "sd", group = "match_id")
)


# Baseline model (with standout players) ----------------------------------

model1 <- brm(
  goal_diff ~ Team_Rating_z + Opp_Rating_z + home_advantage + league +
    (1 | nc | team) + (1 | match_id),
  data = baseline_long,
  family = student(), # Student-t provides robustness to outliers in goal difference
  prior = prior_linear,
  # Sampler control parameters:
  # High adapt_delta to reduce divergent transitions
  # Increased max_treedepth to ensure convergence in complex hierarchical structure
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  # MCMC settings:
  # 4 chains, 4000 iterations per chain (2000 warm-up, 2000 post-warmup)
  # Parallelised using 4 cores for efficiency
  chains = 4, cores = 4, iter = 4000
)


# Replacement model (standout replaced) -----------------------------------

model2 <- brm(
  goal_diff ~ Team_Rating_z + Opp_Rating_z + home_advantage + league +
    (1 | nc | team) + (1 | match_id),
  data = replacement_long,
  family = student(),
  prior = prior_linear,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  chains = 4, cores = 4, iter = 4000
)

# Check summary with R-hat and Effective Sample Size
summary(model1)
summary(model2)

# Compare and Summarise Results -------------------------------------------

# Extract draws and compute contrasts
draws1 <- as_draws_df(model1)
draws2 <- as_draws_df(model2)

# Example: Compute difference in coefficients
contrast <- tibble(
  Predictor = c("Team_Rating_z", "Opp_Rating_z"),
  Mean = c(mean(draws2$b_Team_Rating_z - draws1$b_Team_Rating_z),
           mean(draws2$b_Opp_Rating_z - draws1$b_Opp_Rating_z)),
  Lower = c(quantile(draws2$b_Team_Rating_z - draws1$b_Team_Rating_z, 0.025),
            quantile(draws2$b_Opp_Rating_z - draws1$b_Opp_Rating_z, 0.025)),
  Upper = c(quantile(draws2$b_Team_Rating_z - draws1$b_Team_Rating_z, 0.975),
            quantile(draws2$b_Opp_Rating_z - draws1$b_Opp_Rating_z, 0.975))
)

print(contrast)


loo1 <- loo(model1)
loo2 <- loo(model2)
loo_compare(loo1, loo2)


# Translate SD back to the raw 0-10 rating scale
sd_team_raw <- sd(baseline_long$Team_Rating, na.rm = TRUE)
sd_opp_raw  <- sd(baseline_long$Opp_Rating,  na.rm = TRUE)
sd_team_raw; sd_opp_raw

# Figure 1 ----------------------------------------------------------

# ---------------------------------------------------------------------------
# PREP: keep factor levels consistent & compute raw-scale moments
# ---------------------------------------------------------------------------
baseline_long    <- baseline_long    %>% mutate(league = factor(league))
replacement_long <- replacement_long %>% mutate(league = factor(league))
levs_league <- levels(baseline_long$league)

# Raw-scale means/SDs for converting raw (0–10) <-> z-scores
mu_team <- mean(baseline_long$Team_Rating, na.rm = TRUE)
sd_team <- sd(  baseline_long$Team_Rating, na.rm = TRUE)
mu_opp  <- mean(baseline_long$Opp_Rating,  na.rm = TRUE)
sd_opp  <- sd(  baseline_long$Opp_Rating,  na.rm = TRUE)

# Rating ranges to draw smooth curves over (5th–95th percentiles)
rng_team <- quantile(baseline_long$Team_Rating, probs = c(.05, .95), na.rm = TRUE)
rng_opp  <- quantile(baseline_long$Opp_Rating,  probs = c(.05, .95), na.rm = TRUE)
seq_team <- seq(rng_team[1], rng_team[2], length.out = 60)
seq_opp  <- seq(rng_opp[1],  rng_opp[2],  length.out = 60)

# ---------------------------------------------------------------------------
# 1) Build prediction grids on the RAW (0–10) scale
#    - Reference pane: vary the reference team rating; hold opponent typical
#    - Opponent pane:  vary the opponent rating; hold reference typical
#    - Use population-level predictions (re_formula = NA)
# ---------------------------------------------------------------------------
ref_league <- levs_league[1]
home_flag  <- 1  # set to 0 for an "away" version if you prefer

grid_ref <- tibble(
  Scenario        = "Reference",
  Team_Rating_raw = seq_team,
  Opp_Rating_raw  = mu_opp,
  home_advantage  = home_flag,
  league          = factor(ref_league, levels = levs_league)
) %>%
  mutate(
    Team_Rating_z = (Team_Rating_raw - mu_team) / sd_team,
    Opp_Rating_z  = (Opp_Rating_raw  - mu_opp)  / sd_opp,
    Rating_raw    = Team_Rating_raw
  )

grid_opp <- tibble(
  Scenario        = "Opponent",
  Team_Rating_raw = mu_team,
  Opp_Rating_raw  = seq_opp,
  home_advantage  = home_flag,
  league          = factor(ref_league, levels = levs_league)
) %>%
  mutate(
    Team_Rating_z = (Team_Rating_raw - mu_team) / sd_team,
    Opp_Rating_z  = (Opp_Rating_raw  - mu_opp)  / sd_opp,
    Rating_raw    = Opp_Rating_raw
  )

grid_all <- bind_rows(grid_ref, grid_opp)

# ---------------------------------------------------------------------------
# 2) Posterior expected goal difference for both models (population level)
# ---------------------------------------------------------------------------
ep_base <- posterior_epred(model1, newdata = grid_all, re_formula = NA, ndraws = 2000)
ep_repl <- posterior_epred(model2, newdata = grid_all, re_formula = NA, ndraws = 2000)

# Helper to tidy a posterior_epred() matrix into long format joined to grid
tidy_ep <- function(M, grid, model_lab) {
  as_tibble(M) |>
    mutate(.draw = row_number()) |>
    pivot_longer(-.draw, names_to = ".col", values_to = "epred") |>
    mutate(.col = as.integer(sub("^V", "", .col))) |>
    left_join(grid %>% mutate(.col = row_number()), by = ".col") |>
    transmute(Model = model_lab, Scenario, Rating_raw, epred)
}

pred_all <- bind_rows(
  tidy_ep(ep_base, grid_all, "Baseline model (with standout players)"),
  tidy_ep(ep_repl, grid_all, "Replacement model (with standout players replaced)")
)

# Summarise to means and 95% credible intervals for each curve
summ_df <- pred_all %>%
  group_by(Model, Scenario, Rating_raw) %>%
  mean_qi(epred, .width = 0.95) %>%
  ungroup() %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Reference", "Opponent")),
    Model = factor(
      Model,
      levels = c("Baseline model (with standout players)",
                 "Replacement model (with standout players replaced)")
    )
  )

# ---------------------------------------------------------------------------
# 3) Overlay plot — expected goal difference vs rating (two models)
#    Greyscale, high-contrast lines; 95% credible ribbons.
# ---------------------------------------------------------------------------
p_overlay <- ggplot(
  summ_df,
  aes(x = Rating_raw, y = epred, colour = Model, fill = Model, linetype = Model)
) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.6) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ Scenario, nrow = 1) +
  labs(x = "Team average rating (WhoScored 0–10)",
       y = "Expected goal difference (goals)") +
  scale_colour_manual(values = c(
    "Baseline model (with standout players)" = "black",
    "Replacement model (with standout players replaced)" = "grey30"
  )) +
  scale_fill_manual(values = c(
    "Baseline model (with standout players)" = "grey20",
    "Replacement model (with standout players replaced)" = "grey70"
  )) +
  scale_linetype_manual(values = c(
    "Baseline model (with standout players)" = "solid",
    "Replacement model (with standout players replaced)" = "33" # long dash
  )) +
  guides(
    colour = guide_legend(override.aes = list(linewidth = 1.4)),
    fill   = "none"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.9),
    axis.line.y = element_line(colour = "black", linewidth = 0.9),
    axis.ticks   = element_line(colour = "black")
  )

p_overlay

ggsave(
  filename = "figure_overlay_expected_gd.png",
  plot = p_overlay, width = 9, height = 6, dpi = 300, bg = "white"
)


# Figure 2 ----------------------------------------------------------------

# ---------------------------------------------------------------------------
# 4) Difference plot — Baseline minus Replacement
#    Shows the *added value* of standout players in goal-difference units.
# ---------------------------------------------------------------------------
diff_ep <- ep_base - ep_repl

pred_diff <- as_tibble(diff_ep) %>%
  mutate(.draw = row_number()) %>%
  pivot_longer(-.draw, names_to = ".col", values_to = "diff_ep") %>%
  mutate(.col = as.integer(sub("^V", "", .col))) %>%
  left_join(grid_all %>% mutate(.col = row_number()), by = ".col") %>%
  group_by(Scenario, Rating_raw) %>%
  mean_qi(diff_ep, .width = 0.95) %>%
  ungroup() %>%
  mutate(Scenario = factor(Scenario, levels = c("Reference", "Opponent")))

p_change <- ggplot(pred_diff, aes(x = Rating_raw, y = diff_ep)) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dotted", linewidth = 0.7) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = "grey70", alpha = 0.35) +
  geom_line(colour = "black", linewidth = 1.0) +
  facet_wrap(~ Scenario, nrow = 1) +
  scale_x_continuous(limits = c(6, 7.3), breaks = seq(6, 7.3, by = 0.3)) +
  labs(x = "Team average rating (WhoScored 0–10)",
       y = "Change in expected goal difference (goals)\n(Baseline − Replacement)") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.9),
    axis.line.y = element_line(colour = "black", linewidth = 0.9),
    axis.ticks   = element_line(colour = "black")
  )

p_change

ggsave(
  filename = "figure_change_expected_gd.png",
  plot = p_change, width = 9, height = 6, dpi = 300, bg = "white"
)