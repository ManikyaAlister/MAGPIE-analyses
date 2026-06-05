# ─────────────────────────────────────────────────────────────────────────────
# Prep for the "posts seen" correlation analysis (Supplementary Materials).
#
# Computes the correlation objects (`cor_all`, `cor_nontroll`) between the
# proportion of each post type a participant saw and the change in each outcome
# measure, both for all content and for non-troll-initiated content only. Also
# defines `tidy_cor()` and the outcome variable groups used downstream.
#
# This is the sourceable compute-only portion, sourced by
# 04_supplementary-materials.qmd to draw the supplementary correlation grids.
#
# Objects created in the calling environment:
#   cor_all, cor_nontroll          correlation objects (package `correlation`)
#   tidy_cor()                     tidier used by plotCorrelationsGrid()
#   change_beliefs / _consensus / _trust, after_only_questions
#   d_survey_seen_by_troll         joined + filtered survey × posts-seen data
#
# Required packages: here, tidyverse, correlation
# ─────────────────────────────────────────────────────────────────────────────

library(here)
library(tidyverse)
library(correlation)
source(here("R/helper-functions/lm-functions.R"))  # for scale_to_01()

# define run label if running a different configuration to normal that is saved
# separately. Must either be "" or match a label with posts_seen data.
if (!exists("run_label")) run_label <- "_visibility50"

load(here("data/magpie/original/combined/before.Rdata"))
load(here("data/magpie/original/combined/after.Rdata"))
load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))
load(here(paste0("data/magpie/processed/combined/posts-seen-by-user", run_label, ".Rdata")))

# create variable groupings
after_questions         <- colnames(d_after)
after_only_questions    <- c("PropLiked", "PropTrolls", "PropAligned", "MagpieSimilarity", "OverallExperience")
change_beliefs          <- paste0(after_questions[grepl("Belief", after_questions) & !grepl("Extreme", after_questions)], "Change")
change_consensus        <- paste0(after_questions[grepl("Consensus", after_questions)], "Change")
change_trust            <- paste0(after_questions[grepl("Trust", after_questions) & after_questions != "WVSTrust"], "Change")

# combine post seen data sets for whether they included troll initiated content or not
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

# standardize scale measures data
d_before_after_change_scale01 <- d_before_after_change %>%
  mutate(across(
    where(is.numeric) & (ends_with("Before") | ends_with("After") | any_of(after_only_questions)),
    scale_to_01
  ))

# combine posts seen with measures
d_survey_seen_by_troll <- d_before_after_change_scale01 %>%
  left_join(d_seen_by_troll, by = c(UserName = "username", "Condition")) %>%
  filter(!is.na(prop_left_all) &              # remove participants without prop_seen data
           Condition %in% c("Left", "Right") & # remove control (no troll posts seen)
           total_seen >= 50)                   # remove participants who saw < 50 posts

# set predictors
predictors <- c("prop_right",
                "prop_left",
                "prop_friendly")

# set filter labels
predictors_all      <- paste0(predictors, "_all")
predictors_nontroll <- paste0(predictors, "_nontroll")

# set outcomes
outcomes <- c(change_beliefs, change_consensus, change_trust, after_only_questions)

cor_all <- correlation(
  data  = d_survey_seen_by_troll %>% select(all_of(predictors_all)),
  data2 = d_survey_seen_by_troll %>% select(all_of(outcomes))
)

cor_nontroll <- correlation(
  data  = d_survey_seen_by_troll %>% select(all_of(predictors_nontroll)),
  data2 = d_survey_seen_by_troll %>% select(all_of(outcomes))
)

# Tidy a correlation object into the long form the grid plotters expect
tidy_cor <- function(cor_obj, label) {
  cor_obj %>%
    as.data.frame() %>%
    select(Parameter1, Parameter2, r, CI_low, CI_high) %>%
    mutate(
      type      = label,
      predictor = factor(
        str_remove(Parameter1, "_all$|_nontroll$"),
        levels = c("prop_right", "prop_left", "prop_friendly"),
        labels = c("Right", "Left", "Friendly")
      )
    )
}
