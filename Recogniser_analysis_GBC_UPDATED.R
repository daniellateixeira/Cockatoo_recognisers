
# GBC recogniser analysis

library(dplyr)
library(ggplot2)

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)

# Set working directory
setwd("C:/Users/danie/Google Drive/PhD/Data and analysis/Recogniser nestling and adults/KI GBC/Surveys")

# Data ####

gbc <- read.csv("./Data_combined_GBC_Updated.csv")

# FLEDGED NESTS ####

fledged <- gbc %>% filter(Outcome == "Fledged") %>% droplevels()
str(fledged)

# No. nests and detections
unique(fledged$Nest) #11
nrow(fledged) # 990

# To calcuate no detections and mean/nest
sum <- fledged %>% group_by(Time) %>%
  summarise(nobs = n(), 
            nnest = n_distinct(Nest), 
            mean = nobs/nnest)
sum
write.csv(sum, "./GBC_fledged_sum.csv")

# To calculate percentage of true positives for nestling
sum1 <- fledged %>% group_by(Time, Verify) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum1
write.csv(sum1, "./GBC_fledged_sum1.csv")

# To caluclate percentage of true positics for nestling & adults
sum2 <- fledged %>% group_by(Time, GBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum2
write.csv(sum2, "./GBC_fledged_sum2.csv")

# FAILED NESTS ####

failed <- gbc %>% filter(Outcome == "Failed") %>% droplevels()
str(failed)

# No. nests and detections
unique(failed$Nest) #9
nrow(failed) # 115

# To calcuate no detections and mean/nest
sum3 <- failed %>% group_by(Time) %>%
  summarise(nobs = n(), 
            nnest = n_distinct(Nest), 
            mean = nobs/nnest)
sum3
write.csv(sum3, "./GBC_failed_sum3.csv")

# To calculate percentage of true positives for nestling
sum4 <- failed %>% group_by(Time, Verify) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum4
write.csv(sum4, "./GBC_failed_sum4.csv")

# To caluclate percentage of true positics for nestling & adults
sum5 <- failed %>% group_by(Time, GBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum5
write.csv(sum5, "./GBC_failed_sum5.csv")

# UNSURE NESTS ####

unsure <- gbc %>% filter(Outcome == "Unsure") %>% droplevels()
str(unsure)

# No. nests and detections
unique(unsure$Nest) #3
nrow(unsure) # 283

# To calcuate no detections and mean/nest
sum6 <- unsure %>% group_by(Time) %>%
  summarise(nobs = n(), 
            nnest = n_distinct(Nest), 
            mean = nobs/nnest)
sum6
write.csv(sum6, "./GBC_unsure_sum6.csv")

# To calculate percentage of true positives for nestling
sum7 <-unsure %>% group_by(Time, Verify) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum7
write.csv(sum7, "./GBC_unsure_sum7.csv")

# To caluclate percentage of true positics for nestling & adults
sum8 <- unsure %>% group_by(Time, GBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum8
write.csv(sum8, "./GBC_unsure_sum8.csv")

# PLOTS ####
# Plot frequency distribution (count of Verfiy by score) NESTLING ####

# All data
fig1 <- ggplot(gbc, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,27.5), breaks = seq(18,27.5, by = 1)) +
  scale_y_continuous(breaks = seq(0, 550, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig1
ggsave(filename="GBC_NestlingXScore_All.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 1
time1 <- gbc %>% filter(Time == "Time 1") %>% droplevels()
fig2 <- ggplot(time1, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,27), breaks = seq(18,27, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig2
ggsave(filename="GBC_NestlingXScore_Time1.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 2
time2 <- gbc %>% filter(Time == "Time 2") %>% droplevels()
fig3 <- ggplot(time2, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,27.5), breaks = seq(18,27.5, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig3
ggsave(filename="GBC_NestlingXScore_Time2.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 3
time3 <- gbc %>% filter(Time == "Time 3") %>% droplevels()
fig4 <- ggplot(time3, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,24.5), breaks = seq(18,24.5, by = 1)) +
  #scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig4
ggsave(filename="GBC_NestlingXScore_Time3.png", width = 30, height =20, units = "cm", dpi = 300)


# Plot frequency distribution (count of GBC by score) NESTLING + ADULT ####

# All data
fig5 <- ggplot(gbc, aes(x=score, fill = GBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,27.5), breaks = seq(18,27.5, by = 1)) +
  #scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig5
ggsave(filename="GBC_NestlingAdsXScore_All.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 1
fig6 <- ggplot(time1, aes(x=score, fill = GBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,27), breaks = seq(18,27, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig6
ggsave(filename="GBC_NestlingAdsXScore_Time1.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 2
fig7 <- ggplot(time2, aes(x=score, fill = GBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,27.5), breaks = seq(18,27.5, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig7
ggsave(filename="GBC_NestlingAdsXScore_Time2.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 3
fig8 <- ggplot(time3, aes(x=score, fill = GBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge", color="black") +
  scale_x_continuous(limits = c(17.5,24.5), breaks = seq(18,24.5, by = 1)) +
  scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig8
ggsave(filename="GBC_NestlingAdsXScore_Time3.png", width = 30, height =20, units = "cm", dpi = 300)


# Plot overall performance ####

overall_nestling <- gbc %>% group_by(Verify) %>%
                            summarise(n = n()) %>%
                            mutate(freq = n / sum(n))
overall_nestling
write.csv(overall_nestling, "./overall_nestling.csv")

overall_nestling_adults <- gbc %>% group_by(GBC) %>%
                                    summarise(n = n()) %>%
                                    mutate(freq = n / sum(n))
overall_nestling_adults
write.csv(overall_nestling_adults, "./overall_nestling_adults.csv")


fig9 <- ggplot(gbc, aes(x=Verify, fill = Verify)) +
  theme_classic()+
  geom_bar(aes(y = (..count..)/sum(..count..)), colour="black") + # plot as proportion
  guides(fill=FALSE)+
  scale_x_discrete(breaks=c("N","Y"),labels=c("FP","TP"))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2))+
  xlab("")+
  ylab("Proportion of detections\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig9
ggsave(filename="GBC_Nestling_overall.png", width = 20, height =30, units = "cm", dpi = 300)

fig10 <- ggplot(gbc, aes(x=GBC, fill = GBC)) +
  theme_classic()+
  geom_bar(aes(y = (..count..)/sum(..count..)), colour="black") + # plot as proportion
  guides(fill=FALSE)+
  scale_x_discrete(breaks=c("N","Y"),labels=c("FP","TP"))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2))+
  xlab("")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig10
ggsave(filename="GBC_NestlingAds_overall.png", width = 20, height =30, units = "cm", dpi = 300)

# Plot by site ####

# NB: cant see any pattern here. Not using it.
gbc$Site <- ifelse(gbc$Nest == "Ballast 2017", "Amercian River",
                   ifelse(gbc$Nest == "Ballast 2018", "Amercian River",
                   ifelse(gbc$Nest == "Billboard", "Woodlot Gully",
                   ifelse(gbc$Nest == "Cluster", "Lathami Cons Park",
                   ifelse(gbc$Nest == "Curved", "North East River",
                   ifelse(gbc$Nest == "Flynn 2018 First", "Cygnet Park",
                   ifelse(gbc$Nest == "Flynn 2018 Second", "Cygnet Park",
                   ifelse(gbc$Nest == "Hilary", "Cygnet Park",
                   ifelse(gbc$Nest == "NE1", "NE River",
                   ifelse(gbc$Nest == "Motel", "Amercian River",
                   ifelse(gbc$Nest == "Pit 2017", "Cygnet Park",
                   ifelse(gbc$Nest == "Pit 2018", "Cygnet Park",
                   ifelse(gbc$Nest == "Reluctant 2017", "Woodlot Gully",
                   ifelse(gbc$Nest == "Reluctant 2018", "Woodlot Gully",
                   ifelse(gbc$Nest == "Ritz", "Woodlot Gully",
                   ifelse(gbc$Nest == "Ros", "Dudley Peninsula",
                   ifelse(gbc$Nest == "Skyscraper", "Woodlot Gully",
                   ifelse(gbc$Nest == "Spike", "Woodlot Gully",
                   ifelse(gbc$Nest == "Starfish", "Amercian River",
                   ifelse(gbc$Nest == "Sunglasses", "Cygnet Park",
                   ifelse(gbc$Nest == "Tallest", "Woodlot Gully",
                   ifelse(gbc$Nest == "Tyto", "Parndarna Cons Park",
                   "Other"))))))))))))))))))))))
gbc$Site <- as.factor(gbc$Site)

fig11 <- ggplot(gbc, aes(x=Site, fill = Verify)) +
  theme_classic()+
  geom_bar(aes(y = (..count..)/sum(..count..)), colour="black") + # plot as proportion
  scale_y_continuous(limits = c(0,0.6), breaks = seq(0,1,by = 0.2))+
  ylab("Proportion of detections\n") +
  xlab("")+
  theme(axis.text.x = element_text(size=16, angle = 45))+
  theme(axis.title = element_text(size=18))
fig11
#ggsave(filename="GBC_NestlingXSite.png", width = 20, height =30, units = "cm", dpi = 300)

# Plot by Time ####

# Count
fig12 <- ggplot(gbc, aes(x=Time, fill = Verify)) +
  theme_classic()+
  geom_bar(stat = "count", width=.5, position = "dodge",colour="black") + # plot as proportion
  xlab("")+
  ylab("Count\n") +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig12
ggsave(filename="GBC_NestlingXTime.png", width = 20, height =30, units = "cm", dpi = 300)

fig13 <- ggplot(gbc, aes(x=Time, fill = GBC)) +
  theme_classic()+
  geom_bar(stat = "count", width=.5, position = "dodge",colour="black") + # plot as proportion
  xlab("")+
  ylab("Count\n") +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig13
ggsave(filename="GBC_NestlingAdsXTime.png", width = 20, height =30, units = "cm", dpi = 300)

# Mean - Nestling only
# First, count verifications by nest and time
gbcsum <- gbc %>% group_by(Nest, Time, Verify) %>%
            summarise(n_verify = length(Verify))
                      
gbcsum
#write.csv(gbcsum, "./gbcsum.csv")

# then summarise
gbcsum2 <- gbcsum %>% group_by(Time, Verify) %>%
            summarise(n_verify2 = sum(n_verify),
                      nnest = n_distinct(Nest),
                      mean_verify = n_verify2/nnest,
                      sd = sd(n_verify),
                      se = sd/sqrt(nnest))
gbcsum2

fig14 <- ggplot(gbcsum2, aes(x=Time, y = mean_verify, fill = Verify)) +
  theme_classic()+
  geom_bar(stat = "identity", width=.5, position = "dodge",colour="black") + # plot as proportion
  xlab("")+
  ylab("Detections (mean ± SE)\n") +
  scale_y_continuous(limits = c(0,125), breaks = seq(0,125, by = 25)) +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  geom_errorbar(aes(ymax = gbcsum2$mean_verify + gbcsum2$se, ymin = gbcsum2$mean_verify - gbcsum2$se), position = position_dodge(0.5), width = 0.25) + # Plots the error bars. Remove - data$SE if you only want top bar.
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig14
ggsave(filename="GBC_NestlingXTime_mean.png", width = 20, height =30, units = "cm", dpi = 300)

# Mean - Nestling and Adults

# First, count verifications by nest and time
gbcsum3 <- gbc %>% group_by(Nest, Time, GBC) %>%
  summarise(n_GBC = length(GBC))

gbcsum3
#write.csv(gbcsum, "./gbcsum.csv")

# then summarise
gbcsum4 <- gbcsum3 %>% group_by(Time, GBC) %>%
  summarise(n_GBC2 = sum(n_GBC),
            nnest = n_distinct(Nest),
            mean_GBC = n_GBC2/nnest,
            sd = sd(n_GBC),
            se = sd/sqrt(nnest))
gbcsum4

fig15 <- ggplot(gbcsum4, aes(x=Time, y = mean_GBC, fill = GBC)) +
  theme_classic()+
  geom_bar(stat = "identity", width=.5, position = "dodge",colour="black") + # plot as proportion
  xlab("")+
  ylab("Detections (mean ± SE)\n") +
  scale_y_continuous(limits = c(0,125), breaks = seq(0,125, by = 25)) +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  geom_errorbar(aes(ymax = gbcsum4$mean_GBC + gbcsum4$se, ymin = gbcsum4$mean_GBC - gbcsum4$se), position = position_dodge(0.5), width = 0.25) + # Plots the error bars. Remove - data$SE if you only want top bar.
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig15
ggsave(filename="GBC_NestlingAdsXTime_mean.png", width = 20, height =30, units = "cm", dpi = 300)
