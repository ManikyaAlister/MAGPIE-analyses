library(here)
library(tidyverse)

# ---- Load and combine visibility events ----

loadAndCombineVisibility = function(label = NULL, visibility_threshold = 0){
  
  combined_visibility <- bind_rows(
    read_csv(here("data/analytics/processed/control/visibility_events.csv")) %>% mutate(Condition = "Control"),
    read_csv(here("data/analytics/processed/left/visibility_events.csv"))    %>% mutate(Condition = "Left"),
    read_csv(here("data/analytics/processed/right/visibility_events.csv"))   %>% mutate(Condition = "Right")
  ) %>%
    filter(
      (Condition == "Control" & between(event_datetime, ymd_hms("2024-08-06 21:00:00"), ymd_hms("2024-08-09 21:00:00"))) |
        (Condition == "Left"    & between(event_datetime, ymd_hms("2024-09-03 20:30:00"), ymd_hms("2024-09-06 20:30:00"))) |
        (Condition == "Right"   & between(event_datetime, ymd_hms("2024-08-20 21:00:00"), ymd_hms("2024-08-23 21:00:00")))
    ) %>%
    mutate(status_id = as.character(status_id))
  
  # apply visibility filter (default is 0)
    combined_visibility <- combined_visibility %>%
      filter(percentVisible >= visibility_threshold)

  
  # ---- Load and annotate statuses ----
  
  d_statuses_annotated <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv")) %>%
    mutate(
      StatusID       = as.character(StatusID),
      StatusReplyID  = as.character(StatusReplyID),
      StatusReblogID = as.character(StatusReblogID),
      ConvID_unique  = paste(Condition, ConvID, sep = "_")  # ConvID is only unique within condition
    )
  
  # figure out how many participants we have data for
    n_by_cond <- combined_visibility %>%
      distinct(username, Condition) %>%
      count(Condition)
    print("Number of participants with posts seen data in each condition")
    print(n_by_cond)
    
    
  # ---- Join visibility with polarity annotations ----
  
  visibility_polarity <- combined_visibility %>%
    left_join(
      d_statuses_annotated %>% select(StatusID, Condition, Topic, category, Text, Troll, ConvID, ConvID_unique),
      by = c("status_id" = "StatusID", "Condition")
    ) %>%
    filter(Condition != "Control")
  
  # ---- Helper function to summarise posts seen ----
  
  summarise_posts_seen = function(data) {
    
    distinct_posts_seen <- data %>%
      filter(username != "admin") %>%
      group_by(Condition, username, category, Topic, Troll) %>%
      distinct(status_id) 
    
    print(paste0(nrow(distinct_posts_seen), " distinct posts total"))
    
    distinct_posts_seen %>%
      group_by(Condition, username) %>%
      summarise(
        total_seen         = n(),
        left_seen          = sum(category == "Left", na.rm = TRUE),
        right_seen         = sum(category == "Right", na.rm = TRUE),
        friendly_seen      = sum(Topic == "Friendly", na.rm = TRUE),
        meta_seen          = sum(Topic == "Meta", na.rm = TRUE),
        on_topic_seen      = sum(Topic %in% c("Trans", "Israel", "Climate", "AI"), na.rm = TRUE),
        non_political_seen = sum(!category %in% c("Left", "Right"), na.rm = TRUE),
        troll_seen         = sum(Troll, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        prop_left          = left_seen / total_seen,
        prop_right         = right_seen / total_seen,
        prop_friendly      = friendly_seen / total_seen,
        prop_non_political = non_political_seen / total_seen
      )
  }
  
  # ---- Summarise posts seen by troll/non-troll conversations ----
  
  # Troll-initiated content = statuses in conversations started by a troll
  # account (troll posts and the replies to them), plus any troll-authored
  # status elsewhere (scheduled troll replies posted in participant-started
  # threads), plus reblogs of any of these (a reblog displays the original
  # post but gets its own ConvID).
  status_key <- function(d) paste(d$Condition, d$StatusID, sep = "_")

  troll_convs <- d_statuses_annotated %>%
    group_by(ConvID_unique) %>%
    slice_min(Time, n = 1, with_ties = FALSE) %>%
    filter(Troll == 1) %>%
    pull(ConvID_unique)

  troll_content <- d_statuses_annotated %>%
    filter(ConvID_unique %in% troll_convs | Troll == 1)
  troll_keys <- status_key(troll_content)
  troll_reblogs <- d_statuses_annotated %>%
    filter(!is.na(StatusReblogID),
           paste(Condition, StatusReblogID, sep = "_") %in% troll_keys)
  troll_keys <- unique(c(troll_keys, status_key(troll_reblogs)))

  visibility_polarity <- visibility_polarity %>%
    mutate(troll_content = paste(Condition, status_id, sep = "_") %in% troll_keys)

  unique_posts_seen_by_user          <- summarise_posts_seen(visibility_polarity)
  unique_posts_seen_by_user_troll    <- summarise_posts_seen(filter(visibility_polarity,  troll_content))
  unique_posts_seen_by_user_nontroll <- summarise_posts_seen(filter(visibility_polarity, !troll_content))
  
  # ---- Save ----
  
  save(
    unique_posts_seen_by_user,
    unique_posts_seen_by_user_troll,
    unique_posts_seen_by_user_nontroll,
    file = here(paste0("data/magpie/processed/combined/posts-seen-by-user",label,".Rdata"))
  )
}

# a post counts as seen when >= 50% of it was on screen (each logged event is a
# 1000 ms window); this is the configuration used in the manuscript
loadAndCombineVisibility(label = "_visibility50", visibility_threshold = 50)

