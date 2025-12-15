library(here)
library(tidyverse)
library(ggcorrplot)

# load before and after data & rename so that base consensus is actually the base
d_before <- read_csv(here("data/magpie/original/combined/before.csv")) %>%
  rename(Consensus = ConsensusPolitics)
d_after <- read_csv(here("data/magpie/original/combined/after.csv"))  %>%
  rename(Consensus = ConsensusPolitics)

# load change data and same renamins 
load(here("data/before_after_change_data.Rdata"))
d_change <- d_change  %>%
  rename(Consensus = ConsensusPolitics)
# check correlation between  belief changes for different topics

checkMultiCorr = function(data, variable_base) {
  d_change <- data %>%
    select(starts_with(variable_base), -variable_base)
  cor(d_change)
}

multiCorrPlot = function(data, title){
  ggcorrplot(data, method = "circle", lab = TRUE, type = "lower") + 
    labs(title = title)
}


belief_corr_change <- checkMultiCorr(d_change, "Belief")
multiCorrPlot(belief_corr_change,"Correlation in belief *change* for each belief dimension" )

# what about before/after, not change? 

belief_corr_before <- checkMultiCorr(d_before, "Belief")
multiCorrPlot(belief_corr_before,"Correlation in initial for each belief dimension" )

belief_corr_after <- checkMultiCorr(d_after, "Belief")
multiCorrPlot(belief_corr_after,"Correlation in post-experiment beliefs for each belief dimension" )

# consensus
consensus_corr_change <- checkMultiCorr(d_change, "Consensus")
multiCorrPlot(consensus_corr_change, "Correlation of change in perceived consensus for each consensus dimension")

consensus_corr_before <- checkMultiCorr(d_before, "Consensus")
multiCorrPlot(consensus_corr_before, "Correlation of initial perceived consensus for each consensus dimension")

consensus_corr_after <- checkMultiCorr(d_after, "Consensus")
multiCorrPlot(consensus_corr_after, "Correlation of post-experiment perceived consensus for each consensus dimension")


# How much change was there in different variables generally? 
hist(d_change$Belief, breaks = 100)
hist(d_change$BeliefAI, breaks = 100)
hist(d_change$BeliefTrans, breaks = 100)
hist(d_change$BeliefIsrael, breaks = 100)
hist(d_change$BeliefClimate, breaks = 100)
# observation: much less variance in changes in the aggregate belief variable. 

# affective polarisation
hist(d_change$AffPol, breaks = 100)




