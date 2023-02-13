library(readxl)
library(ggplot2)
library(tidyverse)

datascan <- read_excel("//NAS/home/Antoine/Projects/Social closure/Scan observations/Behaviors_scans.xlsx")
datascan$treatment <- factor (datascan$treatment, levels = c("Control", "Intrapopulation", "Interpopulation", "Interspecies"))
datascan$time_obs <- factor (datascan$time_obs, levels = c("before", "after"))
datascan$observation <- factor (datascan$observation, levels = c("bo1", "bo2", "bo3", "H+2", "H+6", "H+24"))
datascan$females_total <- datascan$females_rest+datascan$females_walk+datascan$females_dig+datascan$females_feed+datascan$females_clean+datascan$females_shuffle+datascan$females_groom+datascan$females_copulate+datascan$females_cannibalise
datascan$larvae_total <- datascan$larvae_rest+datascan$larvae_walk+datascan$larvae_dig+datascan$larvae_feed+datascan$larvae_clean+datascan$larvae_shuffle+datascan$larvae_groom+datascan$larvae_cannibalise

### Make females behavioural data binomial ###
females_restbin <- cbind (datascan$females_rest, datascan$females_total-datascan$females_rest)
females_walkbin <- cbind (datascan$females_walk, datascan$females_total-datascan$females_walk)
females_digbin <- cbind (datascan$females_dig, datascan$females_total-datascan$females_dig)
females_feedbin <- cbind (datascan$females_feed, datascan$females_total-datascan$females_feed)
females_cleanbin <- cbind (datascan$females_clean, datascan$females_total-datascan$females_clean)
females_shufflebin <- cbind (datascan$females_shuffle, datascan$females_total-datascan$females_shuffle)
females_groombin <- cbind (datascan$females_groom, datascan$females_total-datascan$females_groom)
females_copulatebin <- cbind (datascan$females_copulate, datascan$females_total-datascan$females_copulate)
females_cannibalisebin <- cbind (datascan$females_cannibalise, datascan$females_total-datascan$females_cannibalise)


### Make larvae behavioural data binomial ###
larvae_restbin <- cbind (datascan$larvae_rest, datascan$larvae_total-datascan$larvae_rest)
larvae_walkbin <- cbind (datascan$larvae_walk, datascan$larvae_total-datascan$larvae_walk)
larvae_digbin <- cbind (datascan$larvae_dig, datascan$larvae_total-datascan$larvae_dig)
larvae_feedbin <- cbind (datascan$larvae_feed, datascan$larvae_total-datascan$larvae_feed)
larvae_cleanbin <- cbind (datascan$larvae_clean, datascan$larvae_total-datascan$larvae_clean)
larvae_shufflebin <- cbind (datascan$larvae_shuffle, datascan$larvae_total-datascan$larvae_shuffle)
larvae_groombin <- cbind (datascan$larvae_groom, datascan$larvae_total-datascan$larvae_groom)
larvae_cannibalisebin <- cbind (datascan$larvae_cannibalise, datascan$larvae_total-datascan$larvae_cannibalise)

### GLMs for all  females behaviours ###
summary(glm (females_restbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_walkbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_digbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_feedbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_cleanbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_shufflebin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_groombin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_copulatebin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (females_cannibalisebin~datascan$treatment*datascan$time_obs, binomial))

### GLMs for all  larval behaviours ###
summary(glm (larvae_restbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_walkbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_digbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_feedbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_cleanbin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_shufflebin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_groombin~datascan$treatment*datascan$time_obs, binomial))

summary(glm (larvae_cannibalisebin~datascan$treatment*datascan$time_obs, binomial))