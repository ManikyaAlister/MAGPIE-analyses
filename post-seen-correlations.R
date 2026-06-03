library(here)
library(tidyverse)
library(correlation)
library(ggpubr)
source(here("R/helper-functions/lm-functions.R"))

# define run label if running a different configuration to normal that is saved separately
run_label <- "_visibility50" # make sure this is either "null" or matches with a label with posts_seen data

# skip already fitted models? 
skip_fitted <- TRUE

load(here("data/magpie/original/combined/before.Rdata"))
load(here("data/magpie/original/combined/after.Rdata"))
load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))
load(here(paste0("data/magpie/processed/combined/posts-seen-by-user",run_label,".Rdata")))

# create variable groupings 
after_questions         <- colnames(d_after)
after_only_questions    <- c("PropLiked", "PropTrolls", "PropAligned", "MagpieSimilarity", "OverallExperience")
change_beliefs          <- paste0(after_questions[grepl("Belief", after_questions) & !grepl("Extreme", after_questions)], "Change")
change_consensus        <- paste0(after_questions[grepl("Consensus", after_questions)], "Change")
change_trust            <- paste0(after_questions[grepl("Trust", after_questions) & after_questions != "WVSTrust"],"Change")

# for plot space/interpreatbility, some plots will be based on just one variable
prop_liked  <- "PropLiked"
prop_trolls <- "PropTrolls"
prop_aligned <- "PropAligned"
magpie_similarity <- "MagpieSimilarity"
overall_experience <- "OverallExperience"
trust_overall <- "TrustOverall"

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
  filter(!is.na(prop_left_all) & # remove participants that don't have prop_seen data
           Condition %in% c("Left", "Right") & # Remove control since people don't see troll posts
           total_seen >= 50) # remove participants who saw less than 50 posts

# set predictors 
predictors <- c("prop_right",
                "prop_left",
                "prop_friendly")

# set filter labels
predictors_all <- paste0(predictors, "_all")
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

#cor_diff <- cor_all - cor_nontroll


# Convert to data frame for plotting
plot_cor_matrix <- function(cor_matrix, title) {
  cor_matrix %>%
    as.data.frame() %>%
    select(Parameter1, Parameter2, r) %>%
    ggplot(aes(x = Parameter1, y = Parameter2, fill = r)) +
    geom_tile() +
    geom_text(aes(label = round(r, 2)), size = 3) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = NULL, y = NULL, fill = "r")
}
plots <- ggarrange(
  plot_cor_matrix(cor_all,      "All posts"),
  plot_cor_matrix(cor_nontroll, "Non-troll posts"),
  common.legend = TRUE
)
plots
ggsave(plot = plots, filename = here("R/visualise/plots/correlations.png"))

# Helper: assign outcome category label
assign_category <- function(var) {
  factor(
    case_when(
      var %in% change_beliefs       ~ "Belief Change",
      var %in% change_consensus     ~ "Consensus Change",
      var %in% change_trust         ~ "Trust Change",
      var %in% after_only_questions ~ "After Only",
      TRUE                          ~ "Other"
    ),
    levels = c("Belief Change", "Consensus Change", "Trust Change", "After Only")
  )
}

# Tidy both correlation objects and bind
tidy_cor <- function(cor_obj, label) {
  cor_obj %>%
    as.data.frame() %>%
    select(Parameter1, Parameter2, r, CI_low, CI_high) %>%
    mutate(
      type      = label,
      predictor = factor(
        str_remove(Parameter1, "_all$|_nontroll$"),
        levels = c("prop_right", "prop_left", "prop_friendly"),
        labels = c("Prop. Right Seen", "Prop. Left Seen", "Prop. Friendly Seen")
      )
    )
}

keywords <- c("AI", "Trans", "Climate", "Israel")
d_cor_plot <- bind_rows(
  tidy_cor(cor_all,      "All posts"),
  tidy_cor(cor_nontroll, "Non-troll initiated posts")
) %>%
  mutate(
    category = assign_category(Parameter2),
    outcome  = str_remove(Parameter2, "Change$"),
    credible = sign(CI_low) == sign(CI_high)
  ) %>%
  group_by(category) %>%
  group_modify(function(df, key) {
    extracted <- str_extract(df$outcome, paste(keywords, collapse = "|"))
    if (key$category != "Trust Change" & any(extracted %in% keywords, na.rm = TRUE)) {
      df %>% mutate(
        outcome = if_else(is.na(extracted), "Overall", extracted),
        outcome = factor(outcome, levels = rev(c(keywords, "Overall")))
      )
    } else if (key$category == "Trust Change") {
      df %>% mutate(
        outcome = str_remove(outcome, "^Trust"),
        outcome = factor(outcome, levels = c("Media", "Unis", "Govt", "Business", "People", "Overall"))
      )
    } else {
      df %>% mutate(
        outcome = factor(outcome, levels = c(
          "MagpieSimilarity", "OverallExperience", "PropAligned",  "PropLiked", "PropTrolls"
        ))
      )
    }
  }) %>%
  ungroup()

p_dotplot <- ggplot(d_cor_plot,
                    aes(x = outcome, y = r,
                        color = type, shape = type,
                        alpha = credible)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = CI_low, ymax = CI_high),
                  position = position_dodge(width = 0.5),
                  size = 0.4) +
  facet_grid(predictor ~ category, scales = "free_x", space = "free_x") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
  scale_color_manual(values = c("All posts"       = "#E15759",
                                "Non-troll initiated posts" = "#4E79A7")) +
  scale_shape_manual(values = c("All posts"       = 16,
                                "Non-troll initiated posts" = 17)) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
    strip.text      = element_text(size = 8, face = "bold"),
    legend.position = "bottom",
    panel.spacing   = unit(0.8, "lines")
  ) +
  labs(x = NULL, y = "r", color = NULL, shape = NULL,
       title = "Correlations coefficients: All posts seen vs Non-troll initiated posts",
       subtitle = "Transparent coefficients overlap with 0")
p_dotplot
ggsave(plot = p_dotplot,
       filename = here(paste0("R/visualise/plots/correlations-dotplot",run_label,".png")),
       width = 12, height = 6)

