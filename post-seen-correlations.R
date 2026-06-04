# ─────────────────────────────────────────────────────────────────────────────
# Exploratory correlation figures for the "posts seen" analysis.
#
# The correlation computation itself (cor_all, cor_nontroll, tidy_cor, outcome
# groups, joined data) now lives in R/analyse/posts-seen-correlations-prep.R so
# it can be shared with 04_supplementary-materials.qmd. This script just sources
# that prep and draws the standalone exploratory heatmap / dot-plot figures.
#
# The manuscript supplementary correlation grids (Figure 5 equivalents) are
# produced in 04_supplementary-materials.qmd, not here.
# ─────────────────────────────────────────────────────────────────────────────

library(here)
library(tidyverse)
library(ggpubr)

# run_label can be set before sourcing the prep to switch configurations
run_label <- "_visibility50"

# Computes cor_all, cor_nontroll, tidy_cor, change_* groups, d_survey_seen_by_troll
source(here("R/analyse/posts-seen-correlations-prep.R"))

# Heatmaps of the raw correlation matrices ────────────────────────────────────
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

# Dot-plot of all correlations, faceted by outcome category ────────────────────
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
       filename = here(paste0("R/visualise/plots/correlations-dotplot", run_label, ".png")),
       width = 12, height = 6)
