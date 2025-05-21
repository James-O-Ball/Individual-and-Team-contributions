
# Load packages and dataset -----------------------------------------------

# Load Packages
library(tidyverse)
library(tidymodels)
library(dplyr)
library(rsample)
library(sjPlot)
library(ggplot2)
library(broom)
library(patchwork)
library(brms)

# Load match data
final_data_with_subs <- read.csv("Dataset/example_dataset_2.csv", header = TRUE, stringsAsFactors = FALSE)

###### REFERENCE TEAM ANALYSIS ##################

# Prepare data for analysis -----------------------------------------------

# Transform data to long format and replace 'Star' player ratings
df_long <- final_data_with_subs %>% 
  # Select relevant columns, including match details and player ratings
  select(MatchURL, League, Match_Date, Home_Team, Away_Team, Home_Score, Away_Score, outcome,
         Player1_ws_rating, Player2_ws_rating, Player3_ws_rating, Player4_ws_rating, Player5_ws_rating,
         Player6_ws_rating, Player7_ws_rating, Player8_ws_rating, Player9_ws_rating, Player10_ws_rating,
         Player11_ws_rating, Player12_ws_rating, Opp1_ws_rating, Opp2_ws_rating, Opp3_ws_rating, 
         Opp4_ws_rating, Opp5_ws_rating, Opp6_ws_rating, Opp7_ws_rating, Opp8_ws_rating, Opp9_ws_rating, 
         Opp10_ws_rating, Opp11_ws_rating, Opp12_ws_rating) %>% 
  # Group data by match to calculate statistics per game
  group_by(MatchURL) %>%
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
    Replacement_Rating = ifelse(Flag == 'Star', mean, Rating)  # Replace star player rating with the team mean
  ) %>%
  ungroup()

# Create dataset where 'Star' ratings are replaced with team mean
df_replaced <- df_long %>%
  select(MatchURL, Player, Replacement_Rating) %>%
  pivot_wider(names_from = Player, values_from = Replacement_Rating) %>%
  rename_with(~ str_replace(., "_ws_Replacement_Rating", "_ws_rating"), -MatchURL)

# Ensure only unique columns are kept before joining
dat_replaced <- final_data_with_subs %>%
  select(MatchURL, League, Match_Date, Home_Team, Away_Team, outcome, Opp1_ws_rating, Opp2_ws_rating, Opp3_ws_rating, Opp4_ws_rating, Opp5_ws_rating,
         Opp6_ws_rating, Opp7_ws_rating, Opp8_ws_rating, Opp9_ws_rating, Opp10_ws_rating, Opp11_ws_rating, Opp12_ws_rating) %>%  # Keep only necessary match details
  left_join(df_replaced, by = "MatchURL")


reference_team_baseline <- final_data_with_subs %>%  
  mutate(
    Team_ws_rating = rowMeans(select(., starts_with("Player") & ends_with("ws_rating")), na.rm = TRUE),
    Opp_ws_rating = rowMeans(select(., starts_with("Opp") & ends_with("ws_rating")), na.rm = TRUE)
  )

home_team_replaced <- dat_replaced %>%  
  mutate(
    Team_ws_rating = rowMeans(select(., starts_with("Player") & ends_with("ws_rating")), na.rm = TRUE),
    Opp_ws_rating = rowMeans(select(., starts_with("Opp") & ends_with("ws_rating")), na.rm = TRUE)
  )

reference_team_baseline$outcome <- factor(reference_team_baseline$outcome, levels = c("draw", "loss", "win"))
home_team_replaced$outcome <- factor(home_team_replaced$outcome, levels = c("draw", "loss", "win"))

# Setting Priors ----------------------------------------------------------

# Define weakly informative priors
# Normal (0,2) for coefficients, allowing for moderate effects
# Student-t (3,0,2.5) for intercepts, allowing for more flexibility and robustness to outliers
prior1 <- c(
  # Normal (0,2) prior for all regression coefficients predicting the probability of a win
  # Centred at 0 (no effect), with moderate spread allowing for reasonable variation
  prior(normal(0, 2), class = "b", dpar = "muwin"),
  # Normal (0,2) prior for all regression coefficients predicting the probability of a loss
  prior(normal(0, 2), class = "b", dpar = "muloss"),
  # Student-t (3, 0, 2.5) prior for the intercept of the win category
  # Weakly informative: allows for heavier tails than a normal, useful for intercepts
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muwin"),
  # Student-t (3, 0, 2.5) prior for the intercept of the loss category
  prior(student_t(3, 0, 2.5), class = "Intercept", dpar = "muloss")
)

# Reference Team Model Analysis ----------------------------------------------------------

# Baseline reference team model
model1 <- brm(
  outcome ~ Team_ws_rating + Opp_ws_rating + (1 | League) + (1 | Home_Team),
  data = reference_team_baseline,
  family = categorical(),
  prior = prior1,
  control = list(adapt_delta = 0.999),
  chains = 4,
  cores = 4,
  iter = 2000
)

preds <- posterior_predict(model1)

# Get the most frequent predicted outcome for each observation
predicted_class <- apply(preds, 2, function(x) {
  as.numeric(names(which.max(table(x))))
})

# Check how 'outcome' is encoded
table(reference_team_baseline$outcome)

# If it's a factor, convert it to numeric codes
true_class <- as.numeric(reference_team_baseline$outcome)

# Calculate accuracy
accuracy <- mean(predicted_class == true_class)
print(paste("Predictive accuracy:", round(accuracy * 100, 2), "%"))

# Replaced players reference team model
model2 <- brm(
  outcome ~ Team_ws_rating + Opp_ws_rating + (1 | League) + (1 | Home_Team),
  data = home_team_replaced,
  family = categorical(),
  prior = prior1,
  control = list(adapt_delta = 0.999),
  chains = 4,
  cores = 4,
  iter = 2000
)


# --- 1. Predict class probabilities ---
preds1 <- posterior_epred(model1)  # With standout
preds2 <- posterior_epred(model2)   # Without standout
diff_preds <- preds2 - preds1      # Difference (Without - With)

# --- 2. Define summary function ---
summarise_across_draws <- function(pred_array, outcomes) {
  num_classes <- dim(pred_array)[3]
  num_draws <- dim(pred_array)[1]
  
  # Compute average predicted probabilities across samples
  class_probs <- matrix(nrow = num_draws, ncol = num_classes)
  for (k in 1:num_classes) {
    class_probs[, k] <- rowMeans(pred_array[, , k])
  }
  
  # Set column names to class labels BEFORE conversion
  colnames(class_probs) <- outcomes
  
  # Now convert to tibble safely
  class_probs_df <- as_tibble(class_probs)
  
  # Pivot to long format and summarise
  class_probs_df %>%
    pivot_longer(cols = everything(), names_to = "Outcome", values_to = "Probability") %>%
    group_by(Outcome) %>%
    summarise(
      Mean = mean(Probability),
      Lower = quantile(Probability, 0.025),
      Upper = quantile(Probability, 0.975),
      .groups = "drop"
    )
}

# --- 3. Run summaries ---
summary1 <- summarise_across_draws(preds1, levels(reference_team_baseline$outcome))
summary2 <- summarise_across_draws(preds2, levels(reference_team_baseline$outcome))
diff_summary1 <- summarise_across_draws(diff_preds, levels(reference_team_baseline$outcome)) %>%
  rename(Diff_Mean = Mean, Diff_Lower = Lower, Diff_Upper = Upper)

# --- 4. Combine summaries safely ---
final_summary1 <- summary1 %>%
  rename(Mean_With = Mean, Lower_With = Lower, Upper_With = Upper) %>%
  left_join(
    summary2 %>% rename(Mean_Without = Mean, Lower_Without = Lower, Upper_Without = Upper),
    by = "Outcome"
  ) %>%
  left_join(diff_summary1, by = "Outcome")

# --- 5. View clean final output ---
print(final_summary1)

############ OPPONENT TEAM ANALYSIS #####################


# Data processing for opponent --------------------------------------------

# Create an explicit away outcome
final_data_with_subs <- final_data_with_subs %>%
  mutate(away_outcome = case_when(
    outcome == "win"  ~ "loss",
    outcome == "loss" ~ "win",
    outcome == "draw" ~ "draw"
  ))
# Transform data to long format and replace 'Star' player ratings
df_long_opp <- final_data_with_subs %>% 
  select(MatchURL, League, Match_Date, Home_Team, Away_Team, Home_Score, Away_Score, away_outcome,
         Opp1_ws_rating, Opp2_ws_rating, Opp3_ws_rating, Opp4_ws_rating, Opp5_ws_rating,
         Opp6_ws_rating, Opp7_ws_rating, Opp8_ws_rating, Opp9_ws_rating, Opp10_ws_rating,
         Opp11_ws_rating, Opp12_ws_rating) %>% 
  group_by(MatchURL) %>%
  mutate(
    mean_opp = mean(c_across(starts_with("Opp")), na.rm = TRUE),  
    sd_opp = sd(c_across(starts_with("Opp")), na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = starts_with("Opp"),  
    names_to = 'Player', 
    values_to = 'Rating' 
  ) %>% 
  mutate(
    Flag = ifelse(Rating > mean_opp + 2 * sd_opp, "Star", "Average"),
    Replacement_Rating = ifelse(Flag == 'Star', mean_opp, Rating)  
  ) %>%
  ungroup()

# Create dataset where 'Star' ratings are replaced with team mean for away team
df_replaced_opp <- df_long_opp %>%
  select(MatchURL, Player, Replacement_Rating) %>%
  pivot_wider(names_from = Player, values_from = Replacement_Rating) %>%
  rename_with(~ str_replace(., "_ws_Replacement_Rating", "_ws_rating"), -MatchURL)

# Ensure only unique columns are kept before joining
dat_replaced_opp <- final_data_with_subs %>%
  select(MatchURL, League, Match_Date, Home_Team, Away_Team, away_outcome, 
         Player1_ws_rating, Player2_ws_rating, Player3_ws_rating, Player4_ws_rating, Player5_ws_rating,
         Player6_ws_rating, Player7_ws_rating, Player8_ws_rating, Player9_ws_rating, Player10_ws_rating, 
         Player11_ws_rating, Player12_ws_rating) %>%  
  left_join(df_replaced_opp, by = "MatchURL")


# --- Model Setup for Away Team ---
opponent_team_baseline <- dat_replaced_opp %>%  
  mutate(
    Team_ws_rating = rowMeans(select(., starts_with("Player") & ends_with("ws_rating")), na.rm = TRUE),
    Opp_ws_rating = rowMeans(select(., starts_with("Opp") & ends_with("ws_rating")), na.rm = TRUE)
  )

opponent_team_replaced <- dat_replaced_opp %>%  
  mutate(
    Team_ws_rating = rowMeans(select(., starts_with("Player") & ends_with("ws_rating")), na.rm = TRUE),
    Opp_ws_rating = rowMeans(select(., starts_with("Opp") & ends_with("ws_rating")), na.rm = TRUE)
  )

# Ensure 'away_outcome' is a factor with correct levels
opponent_team_baseline$away_outcome <- factor(opponent_team_baseline$away_outcome, levels = c("draw", "loss", "win"))
opponent_team_replaced$away_outcome <- factor(opponent_team_replaced$away_outcome, levels = c("draw", "loss", "win"))


# Opponent team model analysis --------------------------------------------

# Baseline opponent model
model3 <- brm(
  away_outcome ~ Team_ws_rating + Opp_ws_rating + (1 | League) + (1 | Away_Team),
  data = opponent_team_baseline,
  family = categorical(),
  prior = prior1,
  control = list(adapt_delta = 0.999),
  chains = 4,
  cores = 4,
  iter = 2000
)

# Replaced players opponent model 
model4 <- brm(
  away_outcome ~ Team_ws_rating + Opp_ws_rating + (1 | League) + (1 | Away_Team),
  data = opponent_team_replaced,
  family = categorical(),
  prior = prior1,
  control = list(adapt_delta = 0.999),
  chains = 4,
  cores = 4,
  iter = 2000
)


# --- 4. Make Predictions ---
preds3 <- posterior_epred(model3)  # With standout
preds4 <- posterior_epred(model4)   # Without standout
diff_preds2 <- preds4 - preds3      # Difference (Without - With)

# --- 5. Summarize Predictions ---
summary3 <- summarise_across_draws(preds3, levels(opponent_team_baseline$away_outcome))
summary4 <- summarise_across_draws(preds4, levels(opponent_team_baseline$away_outcome))
diff_summary2 <- summarise_across_draws(diff_preds, levels(opponent_team_baseline$away_outcome)) %>%
  rename(Diff_Mean = Mean, Diff_Lower = Lower, Diff_Upper = Upper)

# --- 6. Combine all summaries ---
final_summary2 <- summary3 %>%
  rename(Mean_With = Mean, Lower_With = Lower, Upper_With = Upper) %>%
  left_join(
    summary4 %>% rename(Mean_Without = Mean, Lower_Without = Lower, Upper_Without = Upper),
    by = "Outcome"
  ) %>%
  left_join(diff_summary2, by = "Outcome")

# --- 7. View Results ---
print(final_summary2)



# Formatting results for plot ---------------------------------------------

# Add location label
final_summary1$Location <- "Team"
final_summary2$Location <- "Opponent"

# Combine both datasets
combined_data <- bind_rows(final_summary1, final_summary2)

# Reshape to long format
plot_data <- combined_data %>%
  select(Outcome, Location,
         Mean_With, Lower_With, Upper_With,
         Mean_Without, Lower_Without, Upper_Without) %>%
  pivot_longer(
    cols = c(Mean_With, Lower_With, Upper_With,
             Mean_Without, Lower_Without, Upper_Without),
    names_to = c("Metric", "Type"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
  mutate(
    Fill = factor(
      if_else(Type == "With", "With standout", "With replacement"),
      levels = c("With standout", "With replacement")  # Ensure "With standout" comes first
    ),
    Shape = case_when(
      Outcome == "win" ~ 21,   # circle
      Outcome == "draw" ~ 22,  # square
      Outcome == "loss" ~ 24   # triangle
    )
  )


# Figure 1 - Match outcome probability difference with standout players and with average player replacements --------


# Apply consistent dodge width
dodge_width <- 0.2

# Ensure factor levels only include "Team" and "Opponent"
plot_data <- plot_data %>%
  mutate(
    Location = factor(Location, levels = c("Team", "Opponent"))
  )

ggplot(plot_data, aes(x = Location, y = Mean, group = interaction(Outcome, Fill))) +
  geom_point(
    aes(shape = Outcome, fill = Fill),
    size = 3,
    color = "black",
    stroke = 0.8,
    position = position_dodge(width = dodge_width)
  ) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    width = 0.1,
    position = position_dodge(width = dodge_width)
  ) +
  scale_shape_manual(
    values = c("win" = 21, "draw" = 22, "loss" = 24),
    name = "Outcome"
  ) +
  scale_fill_manual(
    values = c("With standout" = "white", "With replacement" = "black"),
    name = "Player Included",
    guide = guide_legend(override.aes = list(shape = 21))
  ) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_discrete(labels = c("Team" = "Reference Team", "Opponent" = "Opponent")) +
  labs(
    y = "Predicted Probability",
    x = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.box = "vertical",
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 40, 10, 10)
  )


ggsave(filename = 'Player replacements.png',
       plot = last_plot(),
       dpi = 300,
       bg = "white",
       width = 8,
       height = 6)
