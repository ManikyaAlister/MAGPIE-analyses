#  note: variable names switch between camel case and snake case because different people created different data sets using different conventions


library(here)
library(tidyverse)
library(ggpubr)
source(here("R/visualise/functions/plot-linear-relationships.R"))

#load(here("data/magpie/processed/combined/statuses-with-aff-cog.Rdata"))
d_activity_annotated <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv"))

# load combined before, after, and change data 
load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))

# get polarity of replies
d_activity_annotated_with_reply_polarity <- d_activity_annotated %>%
  left_join(
    d_activity_annotated %>%
      select(StatusID, category) %>%
      rename(StatusReplyID = StatusID,
             ReplyPolarity = category),
    by = "StatusReplyID"
  )


troll_accounts <- unique(d_activity_annotated[d_activity_annotated$Troll == 1, "UserName"])


polarity_of_replies_received <- d_activity_annotated_with_reply_polarity %>%
  group_by(AccountReply, Condition) %>%
  summarise(TotalRepliesReceived = sum(!is.na(AccountReply), na.rm = TRUE),
            LeftReceived = sum(category == "Left", na.rm = TRUE),
            RightReceived = sum(category == "Right", na.rm = TRUE),
            PropRightReceived = RightReceived - TotalRepliesReceived, 
            PropRightVsLeftReceived = RightReceived - LeftReceived, 
            PropFriendlyReceived = sum(Topic == "Friendly", na.rm = TRUE) - TotalRepliesReceived, 
            PropLeftReceived = LeftReceived - TotalRepliesReceived, 
            NeutralReceived = sum(category == "Neutral" | is.na(category)),
            DissentingLeftReceived = sum(category == "Left" & ReplyPolarity == "Right", na.rm = TRUE), # Left wing replies to right wing posts
            DissentingRightReceived = sum(category == "Right" & ReplyPolarity == "Left", na.rm = TRUE), # Right wing replies to left wing posts
            PropDissentingRightReceived = DissentingRightReceived - DissentingLeftReceived,
            SupportingLeftReceived = sum(category == "Left" & ReplyPolarity == "Left", na.rm = TRUE), # Left wing replies to left wing posts
            SupportingRightReceived = sum(category == "Right" & ReplyPolarity == "Right", na.rm = TRUE), # Left wing replies to right wing posts
            PropSupportingRightReceived = SupportingRightReceived - SupportingLeftReceived,
            TotalFavouritesReceived = sum(fav_count, na.rm = TRUE) ,
  ) %>%
  # remove confederate accounts 
  filter(!AccountReply %in% troll_accounts) %>%
  rename("UserName" = AccountReply)

write_csv(polarity_of_replies_received, file = here("data/magpie/processed/combined/replies-received.csv"))

# let's have a look at the kinds of replies people received and how common they tended to be 
reply_summary <- data.frame(
  reply_types = c(
    "any",
    "politial",
    "neutral" ,
    "dissenting",
    "supporting"
  ),
  
  n_subs_receiving = c(
    sum(polarity_of_replies_received$TotalRepliesReceived > 0), # number of users who received at least 1 reply
    sum(polarity_of_replies_received$LeftReceived + polarity_of_replies_received$RightReceived > 0), # number of users who receivded at least one political reply 
    sum(polarity_of_replies_received$NeutralReceived > 0),
    sum(polarity_of_replies_received$DissentingLeftReceived + polarity_of_replies_received$DissentingRightReceived > 0),
    sum(polarity_of_replies_received$SupportingLeftReceived + polarity_of_replies_received$SupportingRightReceived > 0)
  )
)


reply_summary %>%
  ggplot(aes(x = reply_types, y = n_subs_receiving))+
  geom_col(aes(fill = reply_types)) + 
  theme_bw()+
  labs(title = "How many participants received each type of reply?", x = "Number of participants who received at least 1 of a type", y = "Reply type")+
  scale_fill_viridis_d()+ 
  theme(legend.position = "none")



# load survey variables and make sure they are all ordered the same way
d_before <- read.csv(here("data/magpie/original/combined/before.csv")) %>%
  arrange(by = UserName) 

d_after <- read.csv(here("data/magpie/original/combined/after.csv")) %>%
  arrange(by = UserName)

x_variables <- colnames(polarity_of_replies_received)[colnames(polarity_of_replies_received) != "UserName"]
y_var <- "Belief"
filter_no_replies <- FALSE # filter out participants who do not have any observations of thie x value
abs_outcome <- FALSE # make the outcome (y) variable the absolute value
split_by_prior <- TRUE # split plots by p's initial prior score on the y variable

if (split_by_prior) {
  # get prior belief of y var
  y_var_before <- d_before[,y_var]
  
  # split into low, medium, high priors
  #splits <- quantile(y_var_before, probs = c(0.33, 0.66))
  splits <- c(33, 66)
  
  
  d_before_splits <- d_before %>%
    mutate(y_var_before_group = case_when(
      !!sym(y_var) <= splits[1] ~ paste0("Left Initial ", y_var),
      !!sym(y_var) > splits[1] & !!sym(y_var) < splits[2] ~ paste0("Center Initial ", y_var),
      !!sym(y_var) >= splits[2] ~ paste0("Right Initial ", y_var)
    )) %>%
    select(UserName, y_var_before_group)
  
  d_before_after_change <- d_before_after_change %>%
    left_join(d_before_splits, by = c("UserName"))
  
  
}


d_change_received <- d_before_after_change %>%
  full_join(
    polarity_of_replies_received, 
    by =c("UserName", "Condition")
  ) %>%
  filter(!is.na(BeliefChange), !is.na(TotalRepliesReceived))


if(abs_outcome){
  
  new_y_var <- paste0("Abs",y_var)
  
  d_change_received <- d_change_received %>% 
    mutate(!!new_y_var := abs(.data[[paste0(y_var,"Change")]]))
  
  y_var <- new_y_var
}


plot_list <- NULL


for (i in 1:length(x_variables)){
  x_var <- x_variables[i]
  if(x_var == "NeutralReceived") {
    x_lims <- c(0,50)
  } else if (x_var == "RightReceived" | x_var == "LeftReceived"){
    x_lims <- c(0,25)
  } else{
    x_lims <- FALSE
  }
  
  
  path = paste0("R/visualise/plots/linear-relationships/",y_var)
  
  d_plotting <- d_change_received
  
  if(filter_no_replies){
    
    # get rid of participants who did not receive any replies of a certain type 
    d_plotting <- d_change_received[d_change_received[,x_var] != 0, ]
    
    path = paste0(path, "-filter-nr")
    
  } 
  
  
  
  dir.create(here(path), showWarnings = FALSE)
  
  plot  <- plotLinearRelationship(d_plotting, x_var, y_var, x_lims = x_lims)
  
  if(split_by_prior){
    path <- paste0(path, "-split-by-prior")
    plot <- plot + facet_wrap(~y_var_before_group, ncol = 3, scales = "free")
  }
  
  ggsave(filename = paste0(path,"/",x_var,"-",y_var,".png"), plot = plot)
  
  plot_list[[i]] <- plot
}

ggarrange(plotlist = plot_list)
ggsave(filename = here(paste0(path,"/",y_var,"-combined.png")), width = 12, height = 7)

# polarity_of_replies_received %>%
#   ggplot()+
#   geom_boxplot(aes(x = condition, y = DissentingLeftReceived))

