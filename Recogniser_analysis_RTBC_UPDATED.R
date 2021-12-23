
# RTBC recogniser analysis

library(dplyr)
library(ggplot2)

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)


# Data ####
rtbc <- read.csv("./Data_combined_RTBC.csv")

# FLEDGED NESTS ####

fledged <- rtbc %>% filter(Outcome == "Fledged") %>% droplevels()
str(fledged)

# No. nests and detections
unique(fledged$Nest) #4
nrow(fledged) # 420

# To calcuate no detections and mean/nest
sum <- fledged %>% group_by(Time) %>%
           summarise(nobs = n(), 
                     nnest = n_distinct(Nest), 
                     mean = nobs/nnest)
sum
write.csv(sum, "./RTBC_fledged_sum.csv")

# To calculate percentage of true positives for nestling
sum1 <- fledged %>% group_by(Time, Verify) %>%
            summarise(n=n()) %>%
            mutate(freq = n / sum(n))
sum1
write.csv(sum1, "./RTBC_fledged_sum1.csv")

# To calculate percentage of true positives for nestling & adults
sum2 <- fledged %>% group_by(Time, RTBC) %>%
          summarise(n=n()) %>%
          mutate(freq = n / sum(n))
sum2
write.csv(sum2, "./RTBC_fledged_sum2.csv")

# FAILED NESTS ####

failed <- rtbc %>% filter(Outcome == "Failed") %>% droplevels()
str(failed)

# No. nests and detections
unique(failed$Nest) #8
nrow(failed) # 285

# To calcuate no detections and mean/nest
sum3 <- failed %>% group_by(Time) %>%
         summarise(nobs = n(), 
                   nnest = n_distinct(Nest), 
                   mean = nobs/nnest)
sum3
write.csv(sum3, "./RTBC_failed_sum3.csv")

# To calculate percentage of true positives for nestling
sum4 <- failed %>% group_by(Time, Verify) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum4
write.csv(sum4, "./RTBC_failed_sum4.csv")

# To caluclate percentage of true positives for nestling & adults
sum5 <- failed %>% group_by(Time, RTBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum5
write.csv(sum5, "./RTBC_failed_sum5.csv")

# UNSURE NESTS ####

unsure <- rtbc %>% filter(Outcome == "Unsure") %>% droplevels()
str(unsure)

# No. nests and detections
unique(unsure$Nest) #7
nrow(unsure) # 431

# To calcuate no detections and mean/nest
sum6 <- unsure %>% group_by(Time) %>%
  summarise(nobs = n(), 
            nnest = n_distinct(Nest), 
            mean = nobs/nnest)
sum6
write.csv(sum6, "./RTBC_unsure_sum6.csv")

# To calculate percentage of true positives for nestling
sum7 <-unsure %>% group_by(Time, Verify) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum7
write.csv(sum7, "./RTBC_unsure_sum7.csv")

# To caluclate percentage of true positives for nestling & adults
sum8 <- unsure %>% group_by(Time, RTBC) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n))
sum8
write.csv(sum8, "./RTBC_unsure_sum8.csv")

# PLOTS ####
# Plot frequency distribution (count of Verfiy by score) NESTLING ####

# All data
fig1 <- ggplot(rtbc, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,34), breaks = seq(18,34, by = 1)) +
  scale_y_continuous(breaks = seq(0, 750, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig1
ggsave(filename="RTBC_NestlingXScore_All.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 1
time1 <- rtbc %>% filter(Time == "Time 1") %>% droplevels()
fig2 <- ggplot(time1, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,29), breaks = seq(18,29, by = 1)) +
  scale_y_continuous(limits = c(0,350),breaks = seq(0, 350, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig2
ggsave(filename="RTBC_NestlingXScore_Time1.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 2
time2 <- rtbc %>% filter(Time == "Time 2") %>% droplevels()
fig3 <- ggplot(time2, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,33.5), breaks = seq(18,33.5, by = 1)) +
  scale_y_continuous(breaks = seq(0, 350, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig3
ggsave(filename="RTBC_NestlingXScore_Time2.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 3
time3 <- rtbc %>% filter(Time == "Time 3") %>% droplevels()
fig4 <- ggplot(time3, aes(x=score, fill = Verify)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,28.5), breaks = seq(18,28.5, by = 1)) +
  scale_y_continuous(breaks = seq(0, 350, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig4
ggsave(filename="RTBC_NestlingXScore_Time3.png", width = 30, height =20, units = "cm", dpi = 300)


# Plot frequency distribution (count of RTBC by score) NESTLING + ADULT ####

# All data
fig5 <- ggplot(rtbc, aes(x=score, fill = RTBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,34), breaks = seq(18,34, by = 1)) +
  #scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig5
ggsave(filename="RTBC_NestlingAdsXScore_All.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 1
fig6 <- ggplot(time1, aes(x=score, fill = RTBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,29), breaks = seq(18,29, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig6
ggsave(filename="RTBC_NestlingAdsXScore_Time1.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 2
fig7 <- ggplot(time2, aes(x=score, fill = RTBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,33.5), breaks = seq(18,33.5, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig7
ggsave(filename="RTBC_NestlingAdsXScore_Time2.png", width = 30, height =20, units = "cm", dpi = 300)

# Time 3
fig8 <- ggplot(time3, aes(x=score, fill = RTBC)) +
  theme_classic()+
  geom_histogram(binwidth=.5, position = "dodge",color="black") +
  scale_x_continuous(limits = c(17.5,28.5), breaks = seq(18,28.5, by = 1)) +
  scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, by = 50))+
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  xlab("\nScore")+
  ylab("Count\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))+
  theme(legend.text = element_text(size=18))
fig8
ggsave(filename="RTBC_NestlingAdsXScore_Time3.png", width = 30, height =20, units = "cm", dpi = 300)


# Plot overall performance ####

overall_nestling <- rtbc %>% group_by(Verify) %>%
                             summarise(n = n()) %>%
                             mutate(freq = n / sum(n))
overall_nestling
write.csv(overall_nestling, "./overall_nestling.csv")

overall_nestling_adults <- rtbc %>% group_by(RTBC) %>%
                              summarise(n = n()) %>%
                              mutate(freq = n / sum(n))
overall_nestling_adults
write.csv(overall_nestling_adults, "./overall_nestling_adults.csv")


fig9 <- ggplot(rtbc, aes(x=Verify, fill = Verify)) +
  theme_classic()+
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black") + # plot as proportion
  guides(fill=FALSE)+
  scale_x_discrete(breaks=c("N","Y"),labels=c("FP","TP"))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2))+
  xlab("")+
  ylab("Proportion of detections\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig9
ggsave(filename="RTBC_Nestling_overall.png", width = 20, height =30, units = "cm", dpi = 300)

fig10 <- ggplot(rtbc, aes(x=RTBC, fill = RTBC)) +
  theme_classic()+
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black") + # plot as proportion
  guides(fill=FALSE)+
  scale_x_discrete(breaks=c("N","Y"),labels=c("FP","TP"))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by = 0.2))+
  xlab("")+
  ylab("Proportion of detections\n") +
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig10
ggsave(filename="RTBC_NestlingAds_overall.png", width = 20, height =30, units = "cm", dpi = 300)

# Plot by Time ####

fig12 <- ggplot(rtbc, aes(x=Time, fill = Verify)) +
  theme_classic()+
  geom_bar(stat = "count", width=.5, position = position_dodge(preserve = "single"),colour="black") + # plot as count
  xlab("")+
  ylab("Count\n") +
  scale_y_continuous(limits = c(17.5,34), breaks = seq(18,34, by = 1)) +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig12
ggsave(filename="RTBC_NestlingXTime.png", width = 20, height =30, units = "cm", dpi = 300)

position = position_dodge(preserve = "single", padding = 0)

fig13 <- ggplot(rtbc, aes(x=Time, fill = RTBC)) +
  theme_classic()+
  geom_bar(stat = "count", width=.5, position = "dodge",colour="black") + # plot as proportion
  xlab("")+
  ylab("Count\n") +
  scale_y_continuous(limits = c(0,1500), breaks = seq(0,1500, by = 500)) +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig13
ggsave(filename="RTBC_NestlingAdsXTime.png", width = 20, height =30, units = "cm", dpi = 300)

# Mean - Nestling only
# First, count verifications by nest and time
rtbcsum <- rtbc %>% group_by(Nest, Time, Verify) %>%
  summarise(n_verify = length(Verify))
rtbcsum
#write.csv(rtbcsum, "./rtbcsum.csv")

# then summarise
rtbcsum2 <- rtbcsum %>% group_by(Time, Verify) %>%
  summarise(n_verify2 = sum(n_verify),
            nnest = n_distinct(Nest),
            mean_verify = n_verify2/nnest,
            sd = sd(n_verify),
            se = sd/sqrt(nnest))
rtbcsum2

fig14 <- ggplot(rtbcsum2, aes(x=Time, y = mean_verify, fill = Verify)) +
  theme_classic()+
  geom_bar(stat = "identity", width=.5, position = position_dodge(preserve = "single"),colour="black") + # plot as proportion
  xlab("")+
  ylab("Detections (mean ± SE)\n") +
  scale_y_continuous(limits = c(0,125), breaks = seq(0,125, by = 25)) +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  geom_errorbar(aes(ymax = rtbcsum2$mean_verify + rtbcsum2$se, ymin = rtbcsum2$mean_verify - rtbcsum2$se), position = position_dodge(0.5), width = 0.25) + # Plots the error bars. Remove - data$SE if you only want top bar.
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig14
ggsave(filename="RTBC_NestlingXTime_mean.png", width = 20, height =30, units = "cm", dpi = 300)

# Mean - Nestling and Adults

# First, count verifications by nest and time
rtbcsum3 <- rtbc %>% group_by(Nest, Time, RTBC) %>%
  summarise(n_RTBC = length(RTBC))

rtbcsum3
#write.csv(rtbcsum, "./rtbcsum.csv")

# then summarise
rtbcsum4 <- rtbcsum3 %>% group_by(Time, RTBC) %>%
  summarise(n_RTBC2 = sum(n_RTBC),
            nnest = n_distinct(Nest),
            mean_RTBC = n_RTBC2/nnest,
            sd = sd(n_RTBC),
            se = sd/sqrt(nnest))
rtbcsum4

fig15 <- ggplot(rtbcsum4, aes(x=Time, y = mean_RTBC, fill = RTBC)) +
  theme_classic()+
  geom_bar(stat = "identity", width=.5, position = "dodge",colour="black") + # plot as proportion
  xlab("")+
  ylab("Detections (mean ± SE)\n") +
  scale_y_continuous(limits = c(0,125), breaks = seq(0,125, by = 25)) +
  scale_fill_discrete(name = "", labels = c("FP","TP"))+ # Remove legend title and change names
  geom_errorbar(aes(ymax = rtbcsum4$mean_RTBC + rtbcsum4$se, ymin = rtbcsum4$mean_RTBC - rtbcsum4$se), position = position_dodge(0.5), width = 0.25) + # Plots the error bars. Remove - data$SE if you only want top bar.
  guides(fill = guide_legend(reverse=TRUE))+ # Reverse legend order
  theme(axis.text = element_text(size=16))+
  theme(axis.title = element_text(size=18))
fig15
ggsave(filename="RTBC_NestlingAdsXTime_mean.png", width = 20, height =30, units = "cm", dpi = 300)
