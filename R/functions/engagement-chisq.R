# ─────────────────────────────────────────────────────────────────────────────
# Engagement data preparation and the chi-square tests reported in the manuscript.
#
# This script prepares the engagement summary data (`d_engagement_summ`,
# `d_statuses_annotated`, `status_colours`) that 02_engagement.qmd uses for its
# figures, and computes the three chi-square tests of engagement reported in the
# paper, storing each test object (which carries `$statistic`, `$parameter`,
# `$p.value`, and the cellwise standardised residuals `$stdres`):
#
#   chisq_composition  Composition of engagement types (Post / Reply / Reblog /
#                      Favourite) across conditions.            X2(6) = 278.11
#   chisq_polarity     Political polarity (Left / Neutral / Right) of engagement
#                      across conditions, run separately for each engagement
#                      type. Named list with one htest per type.
#                      Post X2(4)=9.17, Reply 13.40, Reblog 43.39, Favourite 118.46
#   chisq_friendly     Friendly vs. non-friendly engagement across conditions.
#                                                               X2(2) = 64.86
#
# It is sourced by both 02_engagement.qmd (which reports the test statistics and
# draws the figures) and 04_supplementary-materials.qmd (which reports the
# standardised residuals), so the data prep and tests are defined only once.
#
# Required packages: here, tidyverse
# ─────────────────────────────────────────────────────────────────────────────

library(here)
library(tidyverse)

# ── Engagement data preparation ───────────────────────────────────────────────
# load activity/conversation data
d_statuses_annotated <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv")) %>%
  mutate(StatusID = as.character(StatusID),
         StatusReplyID = as.character(StatusReplyID),
         StatusReblogID = as.character(StatusReblogID),
         Condition = factor(Condition, levels = c("Left", "Control", "Right"))) %>%
  select(Subject:last_col()) # remove redundant columns before Subject made from csv conversion

# Count how many times each StatusID appears in StatusReplyID
reply_counts <- d_statuses_annotated %>%
  count(StatusReplyID, name = "reply_count") %>%
  rename(StatusID = StatusReplyID)

# Join back to main data
d_statuses_annotated <- d_statuses_annotated %>%
  left_join(reply_counts, by = "StatusID") %>%
  mutate(reply_count = replace_na(reply_count, 0))

# create summary variable of engagement
d_base_summ <- d_statuses_annotated %>%
  filter(UserName != "Admin") %>%
  rename(ResponseType = Type) %>%
  mutate(Type =
    case_when(
      Reply ~ "Reply",
      !is.na(AccountReblog) ~ "Reblog",
      Troll == 1 ~ "Troll Post",
      TRUE ~ "Post"
    )) %>%
  group_by(Condition, category, Topic, Type) %>%
  summarise(
    n = n(),
    n_reblog = sum(reblog_count, na.rm = TRUE),
    n_fav = sum(fav_count, na.rm = TRUE),
    n_reply = sum(reply_count, na.rm = TRUE),
    .groups = "drop"
  )

# Add favourites into "type" column for plotting
fav_summ <- d_base_summ %>%
  group_by(Condition, category, Topic) %>%
  summarise(
    Type = "Favourite",
    n = sum(n_fav, na.rm = TRUE),
    n_reblog = 0,
    n_fav = sum(n_fav, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(n_fav = NA)

# add favourites to base_sum
d_engagement_summ <- bind_rows(d_base_summ, fav_summ) %>%
  mutate(Type = factor(Type,
                       levels = c("Favourite", "Reblog", "Reply",
                                  "Post", "Troll Post")),
         Topic = factor(Topic, levels = c("AI", "Climate", "Israel", "Trans", "Friendly", "Meta", "Offtopic")))

# set colour scale of status plots
status_colours <- c("Post" = "darkgreen", "Reply" = "lightgreen", "Reblog" = "orange4",
                    "Favourite" = "orange2", "Troll Post" = "yellow")

# ── Chi-square tests reported in the manuscript ───────────────────────────────

# 1. Composition of engagement types across conditions  ── X2(6) = 278.11
#    Condition x Type (Post / Reply / Reblog / Favourite), excluding troll posts.
chi_table_composition <- d_engagement_summ %>%
  filter(Type != "Troll Post") %>%
  group_by(Condition, Type) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = Type, values_from = n, values_fill = 0) %>%
  column_to_rownames("Condition") %>%
  as.matrix()

chisq_composition <- chisq.test(chi_table_composition)

# 2. Political polarity of engagement across conditions, per engagement type.
#    Condition x polarity (Left / Neutral / Right), one test per engagement type.
#    polarity as rows, condition as columns, so residuals index [polarity, condition].
engagement_types <- c("Post", "Reply", "Reblog", "Favourite")

chisq_polarity <- setNames(
  lapply(engagement_types, function(t) {
    tab <- d_engagement_summ %>%
      filter(Type == t, category %in% c("Left", "Neutral", "Right")) %>%
      group_by(Condition, category) %>%
      summarise(n = sum(n), .groups = "drop") %>%
      pivot_wider(names_from = Condition, values_from = n, values_fill = 0) %>%
      column_to_rownames("category") %>%
      as.matrix()
    suppressWarnings(chisq.test(tab))
  }),
  engagement_types
)

# 3. Friendly vs. non-friendly engagement across conditions  ── X2(2) = 64.86
#    Condition x {Friendly, non-friendly}, posts and replies only.
friendly_counts <- d_engagement_summ %>%
  filter(Type %in% c("Post", "Reply")) %>%       # keep only Posts and Replies
  group_by(Condition, Topic) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = Topic, values_from = n, values_fill = 0) %>%
  mutate(
    total = rowSums(across(-Condition)),
    non_friendly = total - Friendly
  )

chi_table_friendly <- friendly_counts %>%
  select(Condition, Friendly, non_friendly) %>%
  column_to_rownames("Condition") %>%
  as.matrix()

chisq_friendly <- chisq.test(chi_table_friendly)
