library(here)
library(tidyverse)
source(here("R/visualise/functions/plot-linear-relationships.R"))

# load activity data
#d_activity_annotated <- read.csv(here("data/magpie/processed/combined/combined_statuses_with_participant_ids.csv"))
d_activity_annotated <-  read.csv(
  here(
    "data/mastodon/files/processed/combined/corrupted_all_activity_annotated.csv"
  )
) # use data with corrupted IDs for now because at leat we know ratings are correct


# load change data
load(here(here("data/before_after_change_data.Rdata")))

# trust is in a separate filer for some reason
d_trust <- read_csv(here("data/mastodon/files/processed/combined/trust.csv")) %>%
  select(UserName,
         Condition,
         TimePoint,
         starts_with("Trust"),
         -TrustOverall)

# Read data
d_before <- read_csv(here("data/mastodon/files/processed/combined/before.csv")) %>%
  arrange(UserName) %>%
  left_join(
    d_trust %>% filter(TimePoint == "Before") %>% select(-TimePoint),
    by = c("UserName", "Condition")
  )
d_after  <- read_csv(here("data/mastodon/files/processed/combined/after.csv")) %>%
  arrange(UserName) %>%
  left_join(
    d_trust %>% filter(TimePoint == "After") %>% select(-TimePoint),
    by = c("UserName", "Condition")
  )



# save d_before and d_after with trust 
save(d_before, file = here("data/magpie/original/combined/before.Rdata"))
save(d_after, file = here("data/magpie/original/combined/after.Rdata"))

 # %>% # trust overall is in d_before/after
 #  pivot_wider(
 #    id_cols = c("UserName", "Condition"),
 #    names_from = TimePoint,
 #    values_from = starts_with("Trust"),
 #    names_sep = ""
 #  )

# Join on Subject, automatically adding suffixes for overlapping names
d_before_after <- full_join(d_before,
                            d_after,
                            by = c("UserName", "Condition"),
                            suffix = c("Before", "After")) %>%
  left_join(d_trust, 
            by = c("UserName", "Condition"))

# get the variables that are measured both before and after
change_vars <- colnames(d_before)[colnames(d_before) %in% colnames(d_after)]

if (!all(d_before$UserName == d_after$UserName)) {
  stop("Before and after usernames don't match in order!")
}

# update data frames with only the numeric variables that are measured before and after
d_before_change_vars <- d_before %>%
  select(all_of(change_vars) & where(is.numeric))

d_after_change_vars <- d_after %>%
  select(all_of(change_vars) & where(is.numeric))

# check that columns are the same so that we can easily subtract
if (!all(colnames(d_before_change_vars)  == colnames(d_after_change_vars))) {
  error("Columns of before and after don't match")
}

d_change <-  (d_after_change_vars - d_before_change_vars) %>%
  rename_with( ~ paste0(.x, "Change")) %>% # label variables so we know they are the change variables
  mutate(
    UserName = d_after$UserName,
    # add usernames back in
    Condition = d_after$Condition
  ) %>%
  relocate(UserName)

# get full survey data set with before, after, and change.
d_before_after_change <- d_before_after %>%
  left_join(d_change, by = c("UserName", "Condition")) %>%
  select(-TimePointBefore, TimePointAfter) %>%
  rename(Subject = SubjectBefore)


save(
  d_before_after_change,
  file = here(
    "data/magpie/processed/combined/survey-before-after-change.Rdata"
  ))
  