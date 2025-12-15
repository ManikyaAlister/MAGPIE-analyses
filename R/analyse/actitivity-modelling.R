# how are survey responses affected by their activity/behavioural experiences on the platform. 
library(here)
library(tidyverse)
library(dotwhisker)
library(broom)

source("R/visualise/colour-palettes.R")

generate_formula = function(outcome, control, predictors){
  formula_string <- paste0(outcome, " ~ ", control, predictors)
  as.formula(formula_string)
}

linear_model <- function(outcome, control, predictors, data, type = "glm") {
  formula <- generate_formula(outcome, control, predictors)
  
  if(type == "lm" ){
    data %>% lm(formula = formula)
  } else if (type == "glm") {
    data %>% glm(formula = formula,
                 family = quasibinomial(link = "logit")
                 )
  } else {
    error("type can be lm or glm")
  }
  
}

linear_model <- function(outcome, control, predictors, data) {
  formula <- generate_formula(outcome, control, predictors)
  
  data %>% lm(formula = formula)
}

# scale combined data between 0 and 1 for beta regression

scale_to_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

runMultipleModels <- function(data, outcome, control, predictors, type = "glm") {
  purrr::map_dfr(predictors, function(pred) {
    if (type == "glm"){
      data[,outcome] <- scale_to_01(data[,outcome]) # ensure that outcome variable is scaled between 0 and 1 for beta regression
    }
  
    
    m <- linear_model(outcome, control, pred, data)
    
    broom::tidy(m) %>%
      mutate(model = paste0(outcome, " ~ ", control, pred))
  })
}

plotMultipleModelCoeffs <- function(model_output,
                                            control_var,
                                            title = NULL,
                                            xlab = "Estimate",
                                            ylab = "Model") {
  
  clean <- model_output %>%
    filter(term != "(Intercept)") %>%
    mutate(term = as.character(term)) %>%                 # ensure character
    group_by(model) %>%
    mutate(has_interaction = any(str_detect(term, fixed(":")))) %>%  # detect interactions first
    ungroup() %>%
    # remove the control main effect only (exact match or backticked)
    filter(!term %in% c(control_var, paste0("`", control_var, "`"))) %>%
    group_by(model, has_interaction) %>%
    filter(if (has_interaction[1]) str_detect(term, fixed(":")) else TRUE) %>%
    ungroup() %>%
    mutate(condition = case_when(
      grepl("ConditionLeft", term) ~ "Left",
      grepl("ConditionRight", term) ~ "Right",
      TRUE ~ "Not by condition"
    ))
  
  ggplot(clean, aes(x = estimate, y = model, color = condition)) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    geom_errorbar(
      aes(xmin = estimate - 1.96 * std.error,
          xmax = estimate + 1.96 * std.error),
      width = 0.2,
      position = position_dodge(width = 0.6)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(x = xlab, y = ylab, title = title) +
    guides(color = "none") +          # ← remove legend
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "none"        # ← also ensure no legend
    )
}

# runMultipleLMs <- function(data, formulas, model_names = NULL) {
#   if (is.null(model_names)) {
#     model_names <- formulas
#   }
#   
#   out <- purrr::map2_dfr(
#     formulas,
#     model_names,
#     ~{
#       lm(as.formula(.x), data = data) |>
#         broom::tidy() |>
#         mutate(model = .y)
#     }
#   )
#   
#   out
# }

# load before, after, change combined data
load(here("data/magpie/processed/combined/survey-before-after-change.Rdata"))

# load posts seen data 
load(here("data/magpie/processed/combined/posts-seen-by-user.Rdata"))

# load received data 
replies_received <- read_csv(here("data/magpie/processed/combined/replies-received.csv"))

# get participants whose data was available for the "seen" metrics
d_survey<- d_before_after_change %>%
  filter(UserName %in% unique_posts_seen_by_user$username)

# combine posts seen data and survey data
combined_seen_survey <- d_survey %>%
  left_join(
    unique_posts_seen_by_user,
    by = c("UserName" = "username", "Condition")
  )
  
combined_seen_reply_survey <- combined_seen_survey %>%
  left_join(
    replies_received, 
    by = c("UserName", "Condition")
  )


# scale variables for compariosn of coefficients
combined_survey_scaled <- combined_seen_reply_survey %>%
  mutate(across(where(is.numeric), scale))


# remove participants with NAs in key variables so that we can appropriately compare using BIC
combined_survey_clean <- combined_seen_reply_survey
  

# set up linear models 

# set up general predictors of interest
  
# single parameter predictors
  
single_param_predictors <- c( # i know interactions aren't technically 1 param but the idea is that there are no other unique variables
  " + Condition",
  " * Condition",
  " + prop_right",
  " * prop_right",
  "+ Condition * prop_right",
  " + prop_left",
  " * prop_left",
  " + PropRightReceived",
  " * PropRightReceived",
  " + TotalFavouritesReceived ",
  " * TotalFavouritesReceived ",
  " + PropRightReceived",
  " * PropRightReceived",
  " + PropLeftReceived",
  " * PropLeftReceived",
  " + PropFriendlyReceived",
  " * PropFriendlyReceived"
)  

predictor_combos <- c(
  "",
  "BelifAfter ~ BeliefBefore * Condition",
  " + prop_right + Condition",
  " + (prop_right * Condition)",
  " * PropRightReceived + condition",
  " * PropLeftReceived + condition",
  " + prop_right + PropRightReceived",
  " + prop_left + PropLeftReceived"
)

control_variables <- c("BeliefIsraelBefore")#, "ConsensusPoliticsBefore", "RelativePoliticsBefore")

outcome_variables <- c("BeliefIsraelAfter")#, "ConsensusPoliticsAfter", "PropLiked", "PropAligned", "PropTrolls")




model_output <- runMultipleModels(
  data = combined_survey_scaled,
  outcome = outcome_variables,
  control = control_variables,
  predictors = single_param_predictors
)


plotMultipleModelCoeffs(model_output,
                                control_var = control_variables,
                                title = paste0("Coefficient Plot Predicting ",outcome_variables, " Controlling for ", control_variables ))


ggsave(filename = here(paste0("R/visualise/plots/linear-modelling/predicting-",outcome_variables,"-controlling-",control_variables,".png")), width = 16, height = 5)

t = linear_model("BeliefAfter", "BeliefBefore", "+ Condition", data = combined_survey_scaled)
summary(t)

# follow up interactions
## consensusPolitics
### consensusPoliticsBefore * Condition
### condition * prop_right

# Bin consensus politics before for potting 

combined_survey_clean <- combined_survey_clean %>%
  mutate(ConsensusPoliticsBeforeBinned = case_when(
    ConsensusPoliticsBefore < 50 ~ "Low Perceivceived Consensus",
    ConsensusPoliticsBefore > 50 ~ "High Perceivceived Consensus"
  ))


combined_survey_clean %>%
  ggplot(aes(x = prop_right, y = ConsensusPoliticsAfter, colour = Condition, fill = Condition)) + 
  geom_point()+
  geom_smooth(method = "lm") +
  scale_fill_manual(values = conditionColours)+
  scale_colour_manual(values = conditionColours)+
  theme_bw()

combined_survey_clean %>%
  ggplot(aes(x = ConsensusPoliticsBefore, y = ConsensusPoliticsAfter, colour = Condition, fill = Condition)) + 
  geom_point()+
  geom_smooth(method = "lm") +
  scale_fill_manual(values = conditionColours)+
  scale_colour_manual(values = conditionColours)+
  theme_bw()



