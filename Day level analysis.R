
# Day-level recogniser analysis

library(dplyr)
library(ggplot2)

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)

# Set working directory
setwd("C:/Users/danie/Google Drive/PhD/Data and analysis/Recogniser nestling and adults")

# Data
data <- read.csv("./Day_level_UPDATED.csv")
data$Verified <- as.factor(data$Verified)
levels(data$Verified)
levels(data$Verified)[levels(data$Verified)=="Y - TP"] <- "y - TP"
levels(data$Verified)[levels(data$Verified)=="Y - FP"] <- "y - FP"
levels(data$Verified)[levels(data$Verified)=="Y - FP and FN"] <- "y - FP and FN"

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
