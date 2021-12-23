
# Day-level recogniser analysis

# Install packages
library(dplyr)
library(ggplot2)

# Garbage collection
rm(list=ls())
gc(reset=T)

# Data import and filter
data <- read.csv("./Day_level_UPDATED.csv")
gbc <- data %>% filter(Species == "KI GBC") %>% droplevels()
rtbc <- data %>% filter(Species == "SE RTBC") %>% droplevels()

# Summarise GBC
gbc_sum <- gbc %>% group_by(Verified) %>%
                   summarise(n=n()) %>%
                   mutate(freq = n / sum(n))
gbc_sum
write.csv(gbc_sum, "./gbc_sum_daylevel_UPDATED.csv")

# Summarise RTBC
rtbc_sum <- rtbc %>% group_by(Verified) %>%
                    summarise(n=n()) %>%
                    mutate(freq = n / sum(n))
rtbc_sum
write.csv(rtbc_sum, "./rtbc_sum_daylevel_UPDATED.csv")
