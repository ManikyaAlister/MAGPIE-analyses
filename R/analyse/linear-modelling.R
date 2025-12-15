library(here)
library(tidyverse)
library(dotwhisker)
library(broom)
library(ggpubr)


source("R/visualise/colour-palettes.R")


# load before and after data separately
d_before <- read_csv(here("data/mastodon/files/processed/combined/before.csv"))
d_after  <- read_csv(here("data/mastodon/files/processed/combined/after.csv"))

# load before, after, change combined data
load(here(
  "data/magpie/processed/combined/survey-before-after-change.Rdata"
))


# scale combined data between 0 and 1 for beta regression

scale_to_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# apply to data
d_before_after_change_scale01 <- d_before_after_change %>%
  mutate(across(
    where(is.numeric) &
      !ends_with("Before") &
      !ends_with("Change"),
    scale_to_01
  ))# scale the outcome variables between 0 & 1
#across(where(is.numeric) & ends_with("Before"), scale)) # regular scaling for control variables


runMultipleLMs = function(data, variables, change = FALSE) {
  model_output <-  NULL
  for (i in 1:length(variables)) {
    variable <- variables[i]
    
    if (length(change) > 1) {
      change_i <- change[i]
    } else {
      change_i <- change
    }
    
    # Dynamically build formula as string
    
    if (change_i) {
      formula_str <- paste0(variable, "After ~ ", variable, "Before + Condition")
    } else {
      formula_str <- paste0(variable, " ~ Condition")
    }
    
    
    m = glm(
      data = data,
      formula = as.formula(formula_str),
      family = quasibinomial(link = "logit"),
    ) %>%
      tidy() %>%
      mutate(model = variable)
    
    model_output <- bind_rows(model_output, m) %>%
      mutate(model = factor(model, levels = variables))
  }
  model_output
}

plotMultipleModelCoeffs = function(model_output,
                                   title = NULL ,
                                   xlab = NULL,
                                   ylab = NULL,
                                   subtitle = NULL,
                                   legend.position = "none",
                                   ytext = TRUE,
                                   xlim = c(-1.2,0.8)) {
  keywords <- c("AI", "Trans", "Climate", "Israel")
  
  # Extract condition label (Left/Right)
  clean_m <- model_output %>%
    filter(term == "ConditionLeft" | term == "ConditionRight") %>%
    mutate(Condition = ifelse(grepl("Left", term), "Left", "Right"),
           model_clean = str_extract(model, paste(keywords, collapse = "|")),
           model_clean = if_else(is.na(model_clean), "Overall", model_clean),
           model_clean = factor(model_clean, levels = c(keywords, "Overall")))
  
  
  # Plot
  p <- ggplot(clean_m, aes(x = estimate, y = model_clean, color = Condition)) +
    geom_point(position = position_dodge(width = 0.6), size = 3) +
    geom_errorbar(
      aes(
        xmin = estimate - 1.96 * std.error,
        xmax = estimate + 1.96 * std.error
      ),
      width = 0.2,
      position = position_dodge(width = 0.6)
    ) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               color = conditionColours["Control"]) +
    scale_color_manual(values = conditionColours) +
    #lims(x = c(-0.45,0.45))+
    labs(
      x = xlab,
      y = ylab,
      color = "Condition",
      title = title,
      subtitle = subtitle
    ) +
    lims(x = xlim)+
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = legend.position,
          plot.title = element_text(hjust = 0)
         # plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 80, unit = "pt")  # Standardise margins
    )
  
  if (!ytext){
    p <- p + 
      theme(axis.text.y = element_blank())
  }
  p
}

plotMultipleBarPlots = function(data,
                                variables,
                                y_title,
                                title = NULL,
                                subtitle = NULL,
                                legend.position = "none",
                                change = TRUE,
                                ylim = c(-10,10),
                                scales = "fixed") {
  
  if(change){
    change_cols <- paste0(variables, "Change")
  } else {
    change_cols <- variables
  }
  print(change_cols)
  
  d_bar_plots <- data[, c("UserName", "Condition", change_cols)] %>%
    mutate(Condition = factor(Condition, levels = condition_order))
  
  d_bar_plots_long <- d_bar_plots %>%
    pivot_longer(cols = change_cols, names_to = "Variable") %>%
    mutate(Variable = factor(Variable, levels = rev(change_cols)))  
  
  
  d_long_summ <- d_bar_plots_long %>%
    group_by(Condition, Variable) %>%
    summarise(mean = mean(value),
              sd = sd(value),
              n = n()) %>%
    mutate(
      se = sd / sqrt(n))
  
  
  p <- 
    d_long_summ %>%
    ggplot(aes(x = Condition, y = mean, fill = Condition)) +
    geom_col(alpha = .5, colour = "black") +
    geom_jitter(data = d_bar_plots_long,
                aes(y = value, colour = Condition),
                alpha = .3) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2) +
    facet_wrap( ~ Variable, ncol = 1, scales = scales) +
    scale_fill_manual(values = conditionColours) +
    scale_colour_manual(values = conditionColours) +
    theme_minimal(base_size = 12) +
    coord_flip() +
    labs(
      y = y_title ,
      x = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme(
      strip.background = element_blank(),
      # Removes the background box
      strip.text = element_blank(),
      # Removes the text labels
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = legend.position
    )
  
  if (is.numeric(ylim)){
    p <- p + lims(y = ylim) 
  }
  
  p
}

plot_lm_and_bar = function(variable_groups,
                           xlabs_lm,
                           xlabs_bar,
                           titles,
                           d_lm,
                           d_bar,
                           change = TRUE,
                           ylim = NULL,
                           ytext = FALSE,
                           xlim = c(-1.2, 0.8)) {
  plot_list <- list()
  
  for (i in 1:length(variable_groups)) {
    variable_group <- variable_groups[[i]]
    models <- runMultipleLMs(d_lm, variable_group, change = change)
    
    xlab_lm <- xlabs_lm[i]
    xlab_bar <- xlabs_bar[i]
    title <- titles[i]
    
    if(i == 1){
      legend.position = "top"
    } else{
      legend.position = "none"
    }
    
    p_models <- plotMultipleModelCoeffs(models,NULL, xlab_lm, ytext = ytext,  legend.position = legend.position, xlim = xlim)
    
    p_bar <- plotMultipleBarPlots(d_bar, variable_group, xlab_bar, ylim = ylim, scales = "free", change = change, legend.position = legend.position)
    
    p_combined <- ggarrange(p_models, p_bar)
    
    p_combined <- annotate_figure(p_combined, top = title)
    
    plot_list[[i]] <- p_combined
  }
  
  plot_list
}



# get variable names
before_questions <- colnames(d_before)
after_questions <- colnames(d_after)

# questions asked at the end related to their overall experience
after_only_questions <- c("PropLiked",
                          "PropTrolls",
                          "PropAligned",
                          "MagpieSimilarity",
                          "OverallExperience")

# questions relating to their feelings towards republicans/democrats
change_partisan <- c(after_questions[grepl("Dem", after_questions) |
                                       grepl("Rep", after_questions)], "AffPol")


# questions pertaining to political beliefs
change_beliefs <- after_questions[grepl("Belief", after_questions) != grepl("Extreme", after_questions)]

# belief questions transformed to mark non partisan extremity
change_beliefs_extreme <- after_questions[grepl("Extreme", after_questions)]

# questions asking how politically divided they think the us is
change_consensus <- after_questions[grepl("Consensus", after_questions)]

# questions asking how extreme their own beliefs are relative to the population
change_relative <- after_questions[grepl("Relative", after_questions)]

# questions asking how extreme their own beliefs are relative to the population
change_trust <- after_questions[grepl("TrustOverall", after_questions)]


## After only questions

variable_groups <- after_only_questions
titles <- c(
  "What proportion of other participants did they like?",
  "What proportion of accounts did they think were trolls?",
  "How aligned did they think they were compared to the rest of the sample politically?",
  "How similar was the experiment to normal social media?",
  "How much did they enjoy the experience overall?"
)
x_labs_lm <- c(
  "<  0 = liked fewer other participants compared to control",
  "< 0 = fewer trolls compared to control",
  "< 0 = less politicaly aligned compared to control",
  "< 0 = experience less similar compared to control",
  "< 0 = enjoyed less compared to control"
)

x_labs_bar <- c(
  "Lower = liked fewer participants",
  "Lower = fewer trolls",
  "Lower = less aligned politically",
  "Lower = less similar to social media",
  "Lower = less enjoyment"
)

d_lm <- d_before_after_change_scale01
d_bar <- d_before_after_change

p_list_survey_qs <- plot_lm_and_bar(variable_groups, x_labs_lm,x_labs_bar, titles, d_lm, d_bar, change = FALSE)
p_survey_qs <- ggarrange(plotlist = p_list_survey_qs,
                         ncol = 1,
                         common.legend = TRUE,
                         heights = c(2,1.5,1.5,1.5,1.5)
)

ggsave(filename = "R/visualise/plots/combined-bar-lm-post-survey.png", width = 23, height = 30, units = "cm", plot = p_survey_qs) # roughly a4 size

## Belief, consensus, affpol, relative beliefs, trust 


variable_groups <- list(change_beliefs,
                        change_consensus,
                        change_relative,
                        change_trust,
                        "AffPol"
                        )
titles <- c(
  "Political Beliefs", 
  "Perceptions of Conesnsus",
  "Perceived own beliefs relative to population",
  "Overall trust in government and other institutions",
  "Affective polarisation"
)

x_labs_lm <- c(
  "< 0 = more left wing than control",
  "< 0 = less belief there is a consensus compared to control",
  "< 0 = beliefs more left wing relative to population compared to control",
  "< 0 = less trusting than control",
  "< 0 = less polarised than control"
)

x_labs_bar <- c(
  "< 0 = left wing change",
  "< 0 = reduced belief that there is a consensus",
  "< 0 = beliefs became more left wing relative to population",
  "< 0 = became less trusting",
  "< 0 = became less polarised"
)


p_list_change_vars <- plot_lm_and_bar(
  variable_groups,
  x_labs_lm,
  x_labs_bar,
  titles,
  d_lm,
  d_bar,
  change = TRUE,
  ytext = TRUE,
  ylim = c(-10, 10),
  xlim = c(-0.5,0.5)
)
p_change_vars <- ggarrange(
  plotlist = p_list_change_vars,
  ncol = 1,
  heights = c(6, 5, 5, 1.5, 1.5, 5)
)
p_change_vars
ggsave(filename = "R/visualise/plots/combined-bar-lm-change-vars.png", width = 30, height = 37, units = "cm", plot = p_change_vars) # roughly a4 size


