# GBC recogniser analysis
# Table x (Time)

library(dplyr)

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)

# Set working directory
setwd("C:/Users/danie/Google Drive/PhD/Data and analysis/Recogniser nestling and adults/KI GBC/Surveys")

gbc <- read.csv("./Data_combined_GBC_UPDATED.csv")

gbc_sum <- gbc %>% group_by(Outcome, Time, GBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
gbc_sum
write.csv(gbc_sum, "Time_analysis_GBC.csv")
