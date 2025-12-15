library(here)
library(tidyverse)
library(ggpubr)
source(here("R/visualise/functions/plot-linear-relationships.R"))

# load activity data
d_activity_annotated <- read.csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv")) 

# get polarity of replies
d_activity_annotated_with_reply_polarity <- d_activity_annotated %>%
  left_join(
    d_activity_annotated %>%
      select(status_id, message_polarity_ai) %>%
      rename(is_reply_to = status_id,
             replying_to_polarity_ai = message_polarity_ai),
    by = "is_reply_to"
  ) 

troll_interactions <- d_activity_annotated_with_reply_polarity %>%
  group_by(mastodon_username, condition) %>%
  summarise(sum(is_confederate, na.rm = TRUE))

posts_by_polarity <- d_activity_annotated %>% 
  filter(is_confederate == 0) %>%
  group_by(mastodon_username) %>%
  summarise(Right = sum(message_polarity_ai == "Right", na.rm = TRUE),
            Left = sum(message_polarity_ai == "Left", na.rm = TRUE))
  
  