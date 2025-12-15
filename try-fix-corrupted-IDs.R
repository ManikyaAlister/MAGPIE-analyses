
# load annotations with corrupted status IDs
d_corrupted_ann <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv")) 

d_corrupted_clean <- d_corrupted_ann %>%
  mutate(Time = force_tz(Time, "UTC"), # ensure time columns match 
         Time = floor_date(Time, "second")) %>%
  # remove corrupted fields
  select(-StatusID, -StatusReplyID, -StatusReblogID) %>%
  # make sure time formats match
  mutate(Time = as.POSIXct(format(Time, "%Y-%m-%d %H:%M:%S"), tz = "UTC")) %>%
  # Filter posts to valid windows per condition
  filter(
    (Condition == "Control" & 
       Time >= ymd_hms("2024-08-06 21:00:00") &
       Time <= ymd_hms("2024-08-09 21:00:00")) |
      
      (Condition == "Left" &
         Time >= ymd_hms("2024-09-03 20:30:00") &
         Time <= ymd_hms("2024-09-06 20:30:00")) |
      
      (Condition == "Right" &
         Time >= ymd_hms("2024-08-20 21:00:00") &
         Time <= ymd_hms("2024-08-23 21:00:00")))


# load correct status IDs
#correct_status_ids <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_participant_ids.csv")) 
correct_status_ids <- read_csv(here("data/magpie/processed/combined/statuses_with_global_ids.csv")) 


corrected_status_ids_for_merge <- correct_status_ids %>%
    select(mastodon_username, text, status_id, is_reply_to, is_reblog_of, replied_to_username, reblogged_username,creation_time, condition) %>%
    # rename columns so they're consistent with the existing data frames
    rename("StatusID" = status_id,
           "UserName" = mastodon_username,
           "StatusReplyID" = is_reply_to,
           "StatusReblogID" = is_reblog_of,
           "Text" = text ,
           "AccountReblog" = reblogged_username,
           "AccountReply" = replied_to_username,
           "Time" = creation_time,
           "Condition" = condition
           #"ConvID" = conversation_id, 
           #"Topic" = message_topic_rater_1)
    ) %>%
  mutate(Time = as.POSIXct(format(Time, "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
         Condition = case_when(Condition == "left" ~ "Left",
                               Condition == "right" ~ "Right",
                               Condition == "control" ~"Control"),
         StatusID = as.character(StatusID),
         StatusReblogID = as.character(StatusReblogID),
         StatusReplyID = as.character(StatusReplyID)) %>%
  filter(
    (Condition == "Control" & 
       Time >= ymd_hms("2024-08-06 21:00:00") &
       Time <= ymd_hms("2024-08-09 21:00:00")) |
      
      (Condition == "Left" &
         Time >= ymd_hms("2024-09-03 20:30:00") &
         Time <= ymd_hms("2024-09-06 20:30:00")) |
      
      (Condition == "Right" &
         Time >= ymd_hms("2024-08-20 21:00:00") &
         Time <= ymd_hms("2024-08-23 21:00:00"))) 



d_joined <- d_corrupted_clean %>%
  # join with correct status IDs
  left_join(
    corrected_status_ids_for_merge ,
    by = c(
      "UserName",
      "Text",
      "AccountReblog",
      "AccountReply",
      "Time",
      "Condition"
    )
  ) 

# identify rows that have missing ideas
missing <- d_joined %>% filter(is.na(StatusID))

# join those missing ids with less strict keys 
missing_filled <- missing %>%
  select(-StatusID, -StatusReplyID, -StatusReblogID) %>%
  left_join(
    corrected_status_ids_for_merge %>% 
      select(UserName, StatusID, StatusReplyID, StatusReblogID, Time),
    by = c("UserName","Time")
  )

d_joined_full_IDs <- bind_rows(d_joined %>% filter(!is.na(StatusID)), missing_filled)


# make sure reblogs are coded with the topic and category of the original post

#build a lookup table of original posts only
original_lookup <- d_joined_full_IDs %>%
  filter(is.na(StatusReblogID)) %>%      # original posts only
  distinct(StatusID, .keep_all = TRUE) %>%  
  select(StatusID, Topic_orig = Topic, category_orig = category, categorised_by_orig = categorised_by)

# join that lookup onto the full dataset
d_joined_clean_reblogs <- d_joined_full_IDs %>%
  left_join(original_lookup, by = c("StatusReblogID" = "StatusID")) %>%
  mutate(
    Topic = if_else(is.na(Topic), Topic_orig, Topic),
    category = if_else(is.na(category), category_orig, category),
    categorised_by =if_else(is.na(categorised_by), categorised_by_orig, categorised_by)
  ) %>%
  select(-Topic_orig, -category_orig, -categorised_by_orig) %>%
  distinct(StatusID, .keep_all = TRUE) # make sure there are no duplicate statuses, keeping the first one if so

write.csv(d_joined_clean_reblogs, file = here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv"))

# test 

t = d_corrupted_clean %>%
  group_by(Text, UserName, AccountReply, AccountReblog, Time, Condition) %>%
  filter(n() > 1)

t2 = corrected_status_ids_for_merge %>%
  group_by(Text, UserName, AccountReply, AccountReblog, Time, Condition) %>%
  filter(n() > 1)

### See whether we have complete ratings 
no_rating <- d_corrupted_ann %>%
  filter(is.na(category))

# is anything that doesn't have a rating a reblog? 
no_rating_reblog <- no_rating %>%
  filter(! is.na(AccountReblog))

# what are the ones that aren;t a reblog? 
no_rating_reblog <- no_rating %>%
  filter(is.na(AccountReblog)) # most of them don't have a topic so probably shouldn't be rated. Pretty much everything is accounted for, though apart from a few. 


