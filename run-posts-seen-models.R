library(here)
library(tidyverse)
library(brms)
source(here("R/helper-functions/lm-functions.R"))

# ---- Load data ----

load(here("data/magpie/original/combined/before.Rdata"))
load(here("data/magpie/original/combined/after.Rdata"))
load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))
load(here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))

# ---- Variable groups ----

after_questions         <- colnames(d_after)
after_only_questions    <- c("PropLiked", "PropTrolls", "PropAligned", "MagpieSimilarity", "OverallExperience")
change_partisan         <- c(after_questions[grepl("Dem|Rep", after_questions)], "AffPol")
change_beliefs          <- after_questions[grepl("Belief", after_questions) & !grepl("Extreme", after_questions)]
change_beliefs_extreme  <- after_questions[grepl("Extreme", after_questions)]
change_consensus        <- after_questions[grepl("Consensus", after_questions)]
change_relative         <- after_questions[grepl("Relative", after_questions)]
change_trust            <- after_questions[grepl("Trust", after_questions) & after_questions != "WVSTrust"]

# for plot space/interpreatbility, some plots will be based on just one variable
PropLiked  <- "PropLiked"
PropTrolls <- "PropTrolls"
PropAligned<- "PropAligned"
MagpieSimilarity <- "MagpieSimilarity"
OverallExperience <- "OverallExperience"
TrustOverall <- "TrustOverall"
# ---- Scale data ----

d_before_after_change_scale01 <- d_before_after_change %>%
  mutate(across(
    where(is.numeric) & (ends_with("Before") | ends_with("After") | any_of(after_only_questions)),
    scale_to_01
  ))

# ---- Build exposure dataset ----

d_seen_by_troll <- unique_posts_seen_by_user %>%
  left_join(
    unique_posts_seen_by_user_troll %>%
      select(username, Condition, prop_right, prop_left, prop_friendly, meta_seen) %>%
      rename(prop_right_troll = prop_right, prop_left_troll = prop_left,
             prop_friendly_troll = prop_friendly, meta_seen_troll = meta_seen),
    by = c("username", "Condition")
  ) %>%
  left_join(
    unique_posts_seen_by_user_nontroll %>%
      select(username, Condition, prop_right, prop_left, prop_friendly, meta_seen) %>%
      rename(prop_right_nontroll = prop_right, prop_left_nontroll = prop_left,
             prop_friendly_nontroll = prop_friendly, meta_seen_nontroll = meta_seen),
    by = c("username", "Condition")
  ) %>%
  rename(prop_right_all = prop_right, prop_left_all = prop_left,
         prop_friendly_all = prop_friendly, meta_seen_all = meta_seen) %>%
  mutate(across(
    c(prop_right_troll, prop_left_troll, prop_friendly_troll, meta_seen_troll,
      prop_right_nontroll, prop_left_nontroll, prop_friendly_nontroll, meta_seen_nontroll),
    ~ replace_na(., 0)
  ))

d_survey_seen_by_troll <- d_before_after_change_scale01 %>%
  left_join(d_seen_by_troll, by = c(UserName = "username", "Condition")) %>%
  filter(!is.na(prop_left_all) & Condition %in% c("Left", "Right") & total_seen >= 50) # Remove control since people don't see troll posts
# ---- Model setup ----

predictor_names <- c(
  "cond"  = "Condition",
  "pra"   = "prop_right_all",
  "pla"   = "prop_left_all",
  "prnt"  = "prop_right_nontroll",
  "plnt"  = "prop_left_nontroll",
  "pf"    = "prop_friendly_all",
  "pfnt"    = "prop_friendly_nontroll"
  
)

make_label = function(predictor_str) {
  terms <- trimws(strsplit(predictor_str, "\\+")[[1]])
  paste(names(predictor_names)[match(terms, predictor_names)], collapse = "_")
}

# Step 1: does overall content exposure explain condition effects?
comparison_models_all <- c(
  "Condition",
  "prop_right_all",
  "prop_left_all",
  "prop_right_all + prop_left_all",
  "prop_friendly_all"
)

# Step 2: is this driven by norm shift rather than direct troll exposure?
comparison_models_nontroll <- c(
  "Condition",
  "prop_right_nontroll",
  "prop_left_nontroll",
  "prop_right_nontroll + prop_left_nontroll",
  "prop_friendly_nontroll"
)

combined_comparison_models <- unique(c(comparison_models_all, comparison_models_nontroll))

make_legend = function(comparison_models) {
  data.frame(
    label      = sapply(comparison_models, make_label),
    predictors = comparison_models
  )
}

legend_all      <- make_legend(comparison_models_all)
legend_nontroll <- make_legend(comparison_models_nontroll)

# ---- DEFINE OUTCOMES HERE ----
#outcomes   <- "magpie_similarity"
outcomes   <-  c(after_only_questions, change_beliefs,change_consensus, change_trust)


# ---- Run models ----

for (outcome in outcomes) {
  print(paste0("Outcome: ", outcome))
  if (outcome %in% after_only_questions){
    use_change <- F
  } else {
    use_change <- T
  }
  
  runModelComparisons(
    data              = d_survey_seen_by_troll,
    variables         = outcome,
    comparison_models = combined_comparison_models,
    predictor_names   = predictor_names,
    change            = use_change,
    run_models        = TRUE,
    skip_fitted = TRUE
  )
<<<<<<< HEAD

}

# ---- Compute weights separately for each comparison ----

outcome_groups <- c(
  "after_only_questions",
  "change_beliefs",
  "change_consensus",
  "change_trust",
  # also need to do the after only questions and trust seprately for the manuscript plot
  "TrustOverall",
  "PropLiked",
  "PropAligned",
  "PropTrolls",
  "OverallExperience",
  "MagpieSimilarity"
) 

for (outcome_group in outcome_groups){
  results_all      <- computeModelWeights(get(outcome_group), legend_all)
  results_nontroll <- computeModelWeights(get(outcome_group), legend_nontroll)
  
  save(
    results_all, results_nontroll,
    file = here(paste0("R/analyse/lm-output/posts-seen/weights/", outcome_group, ".rdata"))
  )
}

=======
  
  # ---- Compute weights separately for each comparison ----
  
  results_all      <- computeModelWeights(outcome, legend_all)
  results_nontroll <- computeModelWeights(outcome, legend_nontroll)
  
  save(
    results_all, results_nontroll,
    file = here(paste0("R/analyse/lm-output/posts-seen/weights/", outcome, ".rdata"))
  )
}
>>>>>>> 5a32df384e31f2211fc138fadec24084db0a2bd8
