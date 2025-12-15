library(here)
library(tidyverse)
library(dotwhisker)
library(broom)

# now for the engagement data 

m = lm( data = d_before_after_change, formula = AffPolAfter ~ AffPolBefore + Condition)
#m =brms::brm( data = d_before_after_change, formula = BeliefAfter ~ BeliefBefore + Condition)

plotLinearRelationship(data = d_before_after_change, x = "PropLiked", y = "PropTrolls")+
  facet_wrap(~Condition)+
  ylab("PropTrolls")



plotMeanCondDifferences = function(data, y_var) {
  data %>%
    mutate(Condition = factor(Condition, levels = c("Left", "Control", "Right"))) %>%
    group_by(Condition) %>%
    summarise(mean_var = mean(!!sym(y_var)),
              n = n(),
              sd = sd(!!sym(y_var)),
              se = sd/sqrt(n)) %>%
    ggplot(aes(x = Condition)) +
    geom_col(aes(y = mean_var, fill = Condition)) +
    geom_errorbar(aes(ymin = mean_var - se, ymax = mean_var + se), width = 0.2) +
    labs(y = y_var) +
    theme_bw()
}

plotMeanCondDifferences(d_before_after_change, "OverallExperience")

plotMeanCondDifferences(d_before_after_change, "RepFearChange")

m = lm( data = d_before_after_change, formula = RepFearAfter ~ RepFearBefore+Condition)
summary(m)

m = lm( data = d_before_after_change, formula = DemFearAfter ~ DemFearBefore+Condition)
summary(m)

m = lm( data = d_before_after_change, formula = RepAdmireAfter ~ RepAdmireBefore+Condition)
summary(m)

m = lm( data = d_before_after_change, formula = DemAdmireAfter ~ DemAdmireBefore+Condition)
summary(m)
