# RTBC recogniser analysis
# Table x (Time)
# Excluding nestling this time (c.f. what was in my thesis)

library(dplyr)

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)

# Set working directory
setwd("C:/Users/danie/Google Drive/PhD/Data and analysis/Recogniser nestling and adults/SE RTBC/Surveys")

rtbc <- read.csv("./Data_combined_RTBC.csv")

rtbc_sum <- rtbc %>% group_by(Outcome, Time, RTBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
rtbc_sum
write.csv(rtbc_sum, "Time_analysis_RTBC.csv")
