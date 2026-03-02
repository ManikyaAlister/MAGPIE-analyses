############################################################
# How are survey responses affected by platform experiences?
############################################################

library(here)
library(tidyverse)
library(broom)
library(dotwhisker)

source("R/visualise/colour-palettes.R")

# Control and outcome variables
control_variables <- "TrustOverallBefore"
outcome_variables <- "TrustOverallAfter"

############################################################
# Utilities
############################################################

# Construct and fit a linear model
linear_model <- function(outcome, rhs, data) {
  formula <- as.formula(paste0(outcome, " ~ ", rhs))
  lm(formula = formula, data = data)
}

# Run a set of models differing only by predictor specification
runMultipleModels <- function(data, outcome, control, predictors) {
  
  purrr::map_dfr(predictors, function(pred) {
    
    rhs <- paste(c(control, pred), collapse = " ")
    
    m <- linear_model(outcome, rhs, data)
    
    broom::tidy(m) %>%
      mutate(
        model = paste0(outcome, " ~ ", rhs)
      )
  })
}

############################################################
# Plotting
############################################################

plotMultipleModelCoeffs <- function(model_output,
                                    control_var,
                                    title = NULL,
                                    xlab = "Estimate",
                                    ylab = "Model") {
  
  clean <- model_output %>%
    filter(term != "(Intercept)") %>%
    mutate(term = as.character(term)) %>%
    group_by(model) %>%
    mutate(has_interaction = any(str_detect(term, fixed(":")))) %>%
    ungroup() %>%
    # remove control main effect only
    filter(!term %in% c(control_var, paste0("`", control_var, "`"))) %>%
    group_by(model, has_interaction) %>%
    filter(if (has_interaction[1]) str_detect(term, fixed(":")) else TRUE) %>%
    ungroup() %>%
    mutate(condition = case_when(
      grepl("ConditionLeft", term) ~ "Left",
      grepl("ConditionRight", term) ~ "Right",
      TRUE ~ "Not by condition"
    ))
  
  ggplot(clean, aes(x = estimate, y = model, colour = condition)) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbar(
      aes(
        xmin = estimate - 1.96 * std.error,
        xmax = estimate + 1.96 * std.error
      ),
      width = 0.2,
      position = position_dodge(width = 0.6)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "gray50") +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "none"
    )
}

############################################################
# Data loading and preparation
############################################################

load_and_combine_data <- function(troll = "no_troll") {
  
  load(here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))
  
  # Select appropriate posts-seen data
  posts_seen <- if (troll == "no_troll") {
    unique_posts_seen_by_user_no_troll
  } else if (troll == "all_posts") {
    unique_posts_seen_by_user 
  } else if (troll == "only_troll") {
    unique_posts_seen_by_user_troll_only
  }
  
  
  
  # Load posts-seen data
  load(here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))
  
  # Load replies received data
  replies_received <- read_csv(
    here("data/magpie/processed/combined/replies-received.csv"))# %>%
  # keep participants with posts-seen data
   #filter(UserName %in% posts_seen$username)
  
  # add participants who are missing with 

  # Load survey data
  load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))
  # Keep participants with posts-seen data
  d_survey <- d_before_after_change %>%
    filter(UserName %in% posts_seen$username)
  
  # Merge datasets
  combined <- d_survey %>%
    left_join(
      posts_seen,
      by = c("UserName" = "username", "Condition")
    ) %>%
    left_join(
      replies_received,
      by = c("UserName", "Condition")
    )
  
  # Scale numeric variables for coefficient comparison
  # Use as.numeric(scale()) to avoid matrix columns
  combined_scaled <- combined %>%
    mutate(across(where(is.numeric), ~as.numeric(scale(.))))
  
  list(
    scaled = combined_scaled,
    unscaled = combined
  )
}

############################################################
# Model specifications
############################################################

# Predictors of interest
single_param_predictors <- c(
  " + Condition",
  " * Condition",
  " + prop_right",
  " * prop_right",
  " + Condition * prop_right",
  " + prop_left",
  " * prop_left",
  " + PropRightReceived",
  " * PropRightReceived",
  " + TotalFavouritesReceived",
  " * TotalFavouritesReceived",
  " + PropLeftReceived",
  " * PropLeftReceived",
  " + PropFriendlyReceived",
  " * PropFriendlyReceived"
)

predictor_combos <- c(
  "",
  " + BeliefBefore * Condition",
  " + prop_right + Condition",
  " + (prop_right * Condition)",
  " * PropRightReceived + Condition",
  " * PropLeftReceived + Condition",
  " + prop_right + PropRightReceived",
  " + prop_left + PropLeftReceived"
)

all_predictors <- c(single_param_predictors)#, predictor_combos)

#############################################################
# Run general models (full data only)
############################################################

# Load full dataset (including troll-initiated conversations)
combined_survey_scaled <- load_and_combine_data(troll = "no_troll")[["scaled"]]

# Run models across all predictor specifications
model_output <- runMultipleModels(
  data       = combined_survey_scaled,
  outcome    = outcome_variables,
  control    = control_variables,
  predictors = all_predictors
)

############################################################
# Plot results
############################################################

title <- paste0(
  "Predicting ", outcome_variables,
  " controlling for ", control_variables
)

plot <- plotMultipleModelCoeffs(
  model_output,
  control_var = control_variables,
  title = title
)

path <- here(
  paste0(
    "R/visualise/plots/linear-modelling/predicting-",
    outcome_variables,
    "-controlling-",
    control_variables,
    ".png"
  )
)

plot

ggsave(path, plot, width = 16, height = 6)
############################################################
# Robustness check: prop_* predictors
# Troll vs no-troll comparison
############################################################

# Identify predictors affected by troll-initiated conversations
prop_seen_predictors <- all_predictors[grep("prop_", all_predictors)]

# Load datasets
combined_survey_scaled       <- load_and_combine_data(troll = "only_troll")[["scaled"]]
combined_survey_scaled_no_tr <- load_and_combine_data(troll = "no_troll")[["scaled"]]

# Run models (including troll-initiated conversations)
model_output_prop_seen <- runMultipleModels(
  data       = combined_survey_scaled,
  outcome    = outcome_variables,
  control    = control_variables,
  predictors = prop_seen_predictors
) %>%
  mutate(troll = "Including troll")

# Run models (excluding troll-initiated conversations)
model_output_prop_seen_no_tr <- runMultipleModels(
  data       = combined_survey_scaled_no_tr,
  outcome    = outcome_variables,
  control    = control_variables,
  predictors = prop_seen_predictors
) %>%
  mutate(troll = "Excluding troll")

# Combine results for comparison
model_output_prop_seen_combined <- bind_rows(
  model_output_prop_seen,
  model_output_prop_seen_no_tr
)

############################################################
# Plot robustness comparison
############################################################

title <- paste0(
  "Predicting ", outcome_variables,
  " controlling for ", control_variables,
  "\nRobustness to removing troll-initiated conversations"
)

plot <- plotMultipleModelCoeffs(
  model_output_prop_seen_combined,
  control_var = control_variables,
  title = title
) +
  facet_wrap(~ troll)

path <- here(
  paste0(
    "R/visualise/plots/linear-modelling/predicting-",
    outcome_variables,
    "-controlling-",
    control_variables,
    "-prop-predictors-troll-robustness.png"
  )
)

plot

ggsave(path, plot, width = 16, height = 6)
