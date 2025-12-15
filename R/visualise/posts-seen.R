library(here)
library(tidyverse)
library(ggpubr)
source(here("R/visualise/functions/plot-linear-relationships.R"))

unique_seen <- TRUE # if we want to look at unique posts seen 

# load visibility events
visibility_control <- read_csv(here("data/analytics/processed/control/visibility_events.csv")) %>%
  mutate(Condition = "Control")

visibility_left <- read_csv(here("data/analytics/processed/left/visibility_events.csv")) %>%
  mutate(Condition = "Left")

visibility_right <- read_csv(here("data/analytics/processed/right/visibility_events.csv")) %>%
  mutate(Condition = "Right")

# combine visibility events 
combined_visibility <- bind_rows(
  visibility_control, 
  visibility_left, 
  visibility_right
) %>%
  # Filter posts to valid windows per condition (already done for statuses)
  filter(
    (Condition == "Control" & 
       event_datetime >= ymd_hms("2024-08-06 21:00:00") &
       event_datetime <= ymd_hms("2024-08-09 21:00:00")) |
      
      (Condition == "Left" &
         event_datetime >= ymd_hms("2024-09-03 20:30:00") &
         event_datetime <= ymd_hms("2024-09-06 20:30:00")) |
      
      (Condition == "Right" &
         event_datetime >= ymd_hms("2024-08-20 21:00:00") &
         event_datetime <= ymd_hms("2024-08-23 21:00:00"))) %>%
  mutate(status_id = as.character(status_id))


# load polarity ratings 
d_statuses_annotated <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv")) %>%
  mutate(StatusID = as.character(StatusID),
         StatusReplyID = as.character(StatusReplyID),
         StatusReblogID = as.character(StatusReblogID))

# link polarity to posts seen 
visibility_polarity <- combined_visibility %>%
  left_join(
    d_statuses_annotated %>% 
      select(StatusID, Condition, Topic, category, Text, Troll),
    by = c("status_id" = "StatusID", "Condition")  ) %>% 
  mutate(status_id = as.character(status_id))



# get number of posts seen by polarity for each user
posts_seen_by_user <- visibility_polarity %>%
  filter(username != "admin") %>%
  group_by(Condition,username) %>%
  summarise(total_seen = n(),
            left_seen = sum(category == "Left", na.rm = TRUE),
            right_seen = sum(category == "Right", na.rm = TRUE),
            friendly_seen = sum(Topic == "Friendly", na.rm = TRUE),
            meta_seen = sum(Topic == "Meta", na.rm = TRUE),
            on_topic_seen = sum(Topic %in% c("Trans", "Israel", "Climate", "AI"), na.rm = TRUE),
            non_political_seen = sum(! category %in% c("Left", "Right"), na.rm = TRUE),
            troll_seen = sum(Troll, na.rm = TRUE)
            ) %>%
  mutate(
    prop_left = left_seen/total_seen,
    prop_right = right_seen/total_seen,
    prop_friendly = friendly_seen/total_seen,
    prop_non_political = non_political_seen/total_seen
  )


# make the equivalent but for *unique* posts seen 
unique_posts_seen_by_user <- visibility_polarity %>%
  filter(username != "admin") %>%
  group_by(Condition,username, category, Topic, Troll) %>%
  distinct(status_id) %>% # makes it unique posts seen per user
  group_by(Condition,username) %>%
  summarise(total_seen = n(),
            left_seen = sum(category == "Left", na.rm = TRUE),
            right_seen = sum(category == "Right", na.rm = TRUE),
            friendly_seen = sum(Topic == "Friendly", na.rm = TRUE),
            meta_seen = sum(Topic == "Meta", na.rm = TRUE),
            on_topic_seen = sum(Topic %in% c("Trans", "Israel", "Climate", "AI"), na.rm = TRUE),
            non_political_seen = sum(! category %in% c("Left", "Right"), na.rm = TRUE),
            troll_seen = sum(Troll, na.rm = TRUE)
  ) %>%
  mutate(
    prop_left = left_seen/total_seen,
    prop_right = right_seen/total_seen,
    prop_friendly = friendly_seen/total_seen,
    prop_non_political = non_political_seen/total_seen
  )

save(posts_seen_by_user, unique_posts_seen_by_user, file =here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))

# Keep only numeric columns in both data frames for correlations
posts_num <- posts_seen_by_user %>% 
  ungroup() %>% 
  select(where(is.numeric))

unique_num <- unique_posts_seen_by_user %>% 
  ungroup() %>% 
  select(where(is.numeric))

# Compute pairwise correlations for corresponding columns
cor_values <- map_dbl(names(unique_num), ~
  cor(posts_num[[.x]], unique_num[[.x]], use = "pairwise.complete.obs")
)

cor_df <- tibble(
  variable = names(unique_num),
  correlation = cor_values
)

if (unique_seen) {
  # rename unique so we know they;re different 
  #colnames(unique_posts_seen_by_user)[3:ncol(unique_posts_seen_by_user)] <- paste0("unique_", colnames(unique_posts_seen_by_user)[3:ncol(unique_posts_seen_by_user)])
  posts_seen_for_analysis <- unique_posts_seen_by_user
} else {
  posts_seen_for_analysis <- posts_seen_by_user
}


posts_seen_long <- posts_seen_for_analysis %>%
  pivot_longer(
    cols = ends_with("_seen"), 
    names_to = "post_type",
    values_to = "n_seen"
  ) %>%
  mutate(
    post_type_group = case_when(
      post_type %in% c("left_seen", "right_seen", "non_political_seen") ~ "polarity",
        post_type %in% c("friendly_seen", "meta_seen", "on_topic_seen") ~ "topic",
        post_type == "troll_seen" ~ "troll",
        post_type == "total_seen" ~ "total"
      )
    )



# plot some basic condition-level differences
posts_seen_long %>%
  filter(post_type !="total_seen") %>%
  group_by(Condition, post_type, post_type_group) %>%
  summarise(mean = mean(n_seen)) %>%
  ggplot(aes(x = Condition, y = mean, fill = post_type))+ 
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~post_type_group) +
  theme_bw()


# load survey data 
load(here("data/before_after_change_data.Rdata"))
d_before <- read_csv(here("data/magpie/original/combined/before.csv"))
d_after <- read_csv(here("data/magpie/original/combined/after.csv"))

# get variables that were only measured after
d_after_only <- d_after[,c("UserName", colnames(d_after)[!colnames(d_after) %in% colnames(d_before)])]
  
# join after only variables to change data set 
d_change <- d_change %>%
  left_join(d_after_only,
            by = "UserName")


x_variables <- colnames(posts_seen_for_analysis)[!colnames(posts_seen_for_analysis) %in% c("username", "Condition") ]
y_var <- "Belief"
filter_no_replies <- FALSE # filter out participants who do not have any observations of thie x value
abs_outcome <- FALSE # make the outcome (y) variable the absolute value
split_by_prior <- TRUE # split plots by p's initial prior score on the y variable
split_by_cond <- TRUE # split by the condition ps were in 
prior_var <- NULL

if (split_by_prior) {
  
  if (is.null(prior_var)){
    prior_var <- y_var
  } 
  
  # get prior belief of y var
  y_var_before <- d_before[,prior_var]
  
  # split into Low, Medium, High priors
  #splits <- quantile(y_var_before, probs = c(0.33, 0.66))
  splits <- c(33, 66)
  
  
  d_before_splits <- d_before %>%
    mutate(y_var_before_group = case_when(
      !!sym(prior_var) <= splits[1] ~ paste0("Left Initial ", prior_var),
      !!sym(prior_var) > splits[1] & !!sym(prior_var) < splits[2] ~ paste0("Center Initial ", prior_var),
      !!sym(prior_var) >= splits[2] ~ paste0("Right Initial ", prior_var)
    )) %>%
    select(UserName, y_var_before_group)
  
  d_change <- d_change %>%
    left_join(d_before_splits, by = "UserName")
  
  
}


d_change_seen  <- d_change %>%
  full_join(
    posts_seen_for_analysis, 
    by =c("UserName" = "username", "Condition")
  ) %>%
  filter(!is.na(Belief), !is.na(total_seen))


if(abs_outcome){
  
  new_y_var <- paste0("Abs",y_var)
  
  d_change_seen <- d_change_seen %>% 
    mutate(!!new_y_var := abs(.data[[y_var]]))
  
  y_var <- new_y_var
}


plot_list <- NULL


for (i in 1:length(x_variables)){
  x_var <- x_variables[i]
  
  path = paste0("R/visualise/plots/linear-relationships/",y_var)
  
  d_plotting <- d_change_seen 
  
  if(filter_no_replies){
    
    # get rid of participants who did not receive any replies of a certain type 
    d_plotting <- d_change_seen[d_change_seen[,x_var] != 0, ]
    
    path = paste0(path, "-filter-nr")
    
  } 
  
  
  x_lims <- NULL
  x_lims <- NULL
  dir.create(here(path), showWarnings = FALSE)
  
  plot  <- plotLinearRelationship(d_plotting, x_var, y_var, x_lims = x_lims)
  
  if(split_by_prior){
    path <- paste0(path, "-split-by-prior")
    plot <- plot + facet_wrap(~y_var_before_group, ncol = 3, scales = "free")
  }
  
  if (unique_seen){
    x_var <- paste0("unique_", x_var)
  }
  ggsave(filename = paste0(path,"/",x_var,"-",y_var,".png"), plot = plot)
  
  plot_list[[i]] <- plot
}

ggarrange(plotlist = plot_list)
ggsave(filename = here(paste0(path,"/",y_var,"-posts-seen-combined.png")), width = 12, height = 7)
