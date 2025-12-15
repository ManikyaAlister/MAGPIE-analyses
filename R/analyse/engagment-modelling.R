library(here)
library(tidyverse)
 
d_annotated <- read_csv(here("data/magpie/processed/combined/combined_statuses_with_original_annotations.csv"))

t <- d_annotated %>% lm(formula = fav_count ~ Topic + Polarity)

summary(t)
