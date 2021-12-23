# -----------------------------------------------------------------#
# CALL RECOGNISER: KANGAROO ISLAND GLOSSY BLACK-COCKATOO
# This script uses Binary Point Matching from the package monitoR
# Updated from D. Teixeira PhD to include adult calls
# Script written by D. Teixeira & S. Linke
# UPDATED Scripts are the improved scripts for template building and 
# template evaluation
# -----------------------------------------------------------------#

# Set working directory
setwd("C:\\Users\\danie\\Google Drive\\PhD\\Data and analysis\\Recogniser nestling and adults\\KI GBC\\Pilot")

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)

# -----------------------------------------------------------------#
# RECOGNISER
# -----------------------------------------------------------------#

# Load packages
library(monitoR)
library(tuneR)
library(RODBC)
library(seewave)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggplot2)

# --------------------------- UPDATED: TEMPLATE CREATOR --------------------------- ####

# This script makes templates from the sound clips

templatestack <- "GBC_templates_pilot"
ref_filelist <- as.data.frame(list.files(pattern=".WAV"))
ref_filelist <- list.files(pattern=".wav")
NoOfRefcalls <- length(ref_filelist)
templatename <- list()
bpmlist <- list()
bpmlist2 <- list()
#callno<-4
#ref_filelist[callno]

cutoff_increments<-c(-2,0,2) #amplification cutoff modifier
#cutoff_increments<-c(0) #amplification cutoff modifier

refcolnames <- c("amp", "template","nestname") # Filename is as such: "31_Perch2_Alan.wav"
newCols <- colsplit(ref_filelist, "_", refcolnames)
templatenum <- 0
#pdf(paste((stackname),".pdf", sep=""), width=4, height=4)

# Loop to convert all templates to mono and 44.1kHz sample rate and create binTemplates
for(callno in 1:NoOfRefcalls){
  detectsamplerate <- readWave(ref_filelist[callno], header=TRUE)
  convflag <- 1
  refcall_in <- readWave(ref_filelist[callno])  # read template
  if(detectsamplerate$channels==2){
    refcall_in<-mono(refcall_in)} #converts it to mono
  if(detectsamplerate$sample.rate!=44100){
    refcall_in<-resamp(refcall_in, f=detectsamplerate$sample.rate, g=44100, output="Wave")
  }
  unlink("refcall_in.wav")  
  
  # This reads the amp level from the filename
  # Filename is as such: "31_Perch2_Alan.wav"
  amplist <- do.call(rbind, strsplit(ref_filelist[callno], '_'))
  
  for(modifier in cutoff_increments){
    templatenum <- templatenum+1
    amplification <- as.numeric(amplist[1,1])*-1+modifier
    templatename <- paste(amplification,newCols[callno,2],newCols[callno,3],sep="_")
    cat(templatename)
    
    # Make BMP template
    bpmtemplate <- makeBinTemplate(refcall_in, wl=512, amp.cutoff = amplification, name=templatename, write.wav=TRUE)       
    title(paste(templatename))
    cat(callno)
    
    # Rename and save template files
    file.rename("refcall_in.wav", paste(templatename, sep=""))
    assign(templatename,bpmtemplate)
    bpmlist[templatenum] <- paste(templatename)
    
    
  }
}      

unlink("refcall_in.wav")  
dev.off()

# --------------------------- UPDATED: TEMPLATE STACK BUILDER --------------------------- ####

# This script combines templates into a stack that can be read in and used in the recogniser

# makeBinTemplate converts time-domain data into binary i.e. cells are "on" or "off"
# Depending on whether they are greater than or less than the user-set amp.cutoff
# Set buffer by specifying buffer = x, cells in buffer are excluded from template

# Combine templates into Bin List
btemps <- NULL
btemps <- combineBinTemplates(get(bpmlist[[1]])) # initiates the list at 1

temp_filelist <- as.data.frame(list.files(pattern=".wav")) # Move any original templates (not starting with -) to a different folder
temp_filelist <- list.files(pattern=".wav")
NoOfTemps <- length(temp_filelist)

for(callno in 2:NoOfTemps){ #loop to template number, starting from 2
  bpmnew <- get(temp_filelist[[callno]])
  btemps <- combineBinTemplates(btemps,bpmnew)
}

# Change the threshold score cut-off
templateCutoff(btemps) <- c(default = 20)
#templateCutoff(btemps) <- c(9.4,9.8,13.4,8.4,7.4) # Specify unique cutoff for each template
btemps

save(btemps,file=paste(templatestack,".Rdata", sep=""))


# --------------------------- UPDATED: BPM DETECTION BATCH FILES ----------- ####

# Load the template stack
load(".\\GBC_templates_pilot.Rdata")

# Change the threshold score cut-off
templateCutoff(btemps) <- c(default = 5)
btemps

directory <- "C:\\Users\\danie\\Google Drive\\PhD\\Data and analysis\\Recogniser nestling and adults\\KI GBC\\Pilot\\Pilot surveys"
surveys <- list.files(directory, pattern=".wav", full.names = TRUE) # Need recursive = TRUE to read all wav files in folders
surveys

for(i in 1:length(surveys)){    
  
  offset = 0
  filename = surveys[i] # Filename or for a batch filename = surveys[i]
  detectsamplerate <- readWave(filename, header=TRUE)
  #calculates duration and sampling freq
  dursec <- detectsamplerate[[4]] /detectsamplerate[[1]]
  durmin <- as.integer(dursec/60)
  
  interval = 60 # snippet length in seconds
  
  summary <- data.frame() # Create a blank dataframe - Must do this every run otherwise it won't be blank
  
  if (dursec/60==as.integer(dursec/60)){
    durmin<-durmin-1}

  for(offset in 0:durmin) {
    unlink("snippet_in.wav")
    snippet_in <- readWave(filename, from = offset*interval, to = offset*interval + interval, units = "seconds")
    #writeWave(snippet_in, filename = "snippet.wav" )
    
    # Perform the survey matching
    scores <- binMatch(
      #survey="snippet.wav",
      survey = snippet_in,
      templates=btemps,
      time.source="fileinfo",
      write.wav = TRUE,
      show.prog = TRUE)
    
    # Convert scores to peaks
    pks <- findPeaks(score.obj=scores)
    
    # Extract the detections
    detects <- getDetections(pks)
    
    
    # Best detections only
    rowcount <- nrow(detects)
    if (rowcount !=0){
      detects$fulltime <- detects$time+offset*interval # Will add time for the full survey (not just snippet)
      detects$file <- filename
    }
    
    summary <- rbind(summary,detects)
    
    # If there are detections, write a PNG of the detections
    #if(rowcount !=0){
    # png_filename <- paste(filename,offset,".png", sep="")
    #  png(png_filename)
    # plot(pks, ask=FALSE,t.each=60)
    #dev.off()
    #}
    
  }
  
  outputname <- gsub('\\.wav', '', i, '.csv')
  write.csv(summary, paste0(filename, '.csv'), row.names=FALSE)
  
} 

# --------------------------- UPDATED: EVALUATOR AND SCORE CUTOFF DETERMINATION ---- ####

# Create list of all templates and all survey files

# Load the template stack - unless this is already loaded
stackname <- "GBC_templates_pilot_full_" # Name for full list of all templates tested
load(".\\GBC_templates_pilot.Rdata")

templatelist <- names(btemps@templates) # List of templates
templatelist

surveylist <- list.files("./Pilot surveys", pattern = ".wav") # List of surveys
surveylist

# Create dataframe of all templates and surveys
tempsurveylist <- NULL
tempsurveylist <-as.data.frame(tempsurveylist)

rownum <- 1

for(i in 1:length(templatelist)){
      for (j in 1:length(surveylist)){
        tempsurveylist[rownum,1] <- templatelist[i] # add template name to column 1
        tempsurveylist[rownum,2] <- surveylist[j] # add survey name to column 2
        rownum=rownum+1
      }
}

colnames(tempsurveylist) <- c("template","Filename") # Rename columns
tempsurveylist$Presence <- as.numeric(gsub("_.*", "", tempsurveylist$Filename)) # Get presence-absence from file name and add to dataframe
 
# Create full detectlist (all detections from all files)
outputlist <- list.files("./Pilot surveys/CSV Outputs", pattern = ".csv") # manually moved the csv files of detections into this folder

detectlist <- NULL

for (l in 1:length(outputlist)){
     newfile <- read.csv(paste("./Pilot surveys/CSV Outputs/", outputlist[l], sep = ""))
     detectlist <- rbind(detectlist, newfile)
     }

unique(detectlist$file) # extract unique files
detectlist$Filename <- gsub(".*/", "", detectlist$file) # Get filename without path
detectlist$file <- NULL # Drop file path
detectlist$Presence <- as.numeric(gsub("_.*", "", detectlist$Filename)) # Get presence-absence from file name and add to dataframe


# Summarise detectlist for number of detections by template and file
detectlist_sum <- detectlist %>% 
                  group_by(template, Filename) %>%
                  summarise(Num_Detects = n())
detectlist_sum

# Merge detectlist with tempsurveylist
pre_summary <- merge(detectlist_sum, tempsurveylist, by = c("template","Filename"), all = TRUE)
summary_to_exp <- pre_summary

# -------------  ROC ANALYSIS ------------

# ROC analysis is used to determine a good score cut off for each template

roctable <- NULL
summarised <- NULL
toexp <- NULL

# Number of survey files
nosites <- length(unique(summary_to_exp$Filename))
nosites

# Filter out zero scores, if there are any
detectlist_no0 <- filter(detectlist, score != 0) 

# Find true negatives
summary_to_exp_nulls <- summary_to_exp %>% 
                        filter(is.na(Num_Detects)) %>% 
                        filter(Presence==0)
summary_to_exp_nulls

summary_to_exp_nullstats <- summary_to_exp_nulls %>% 
                            group_by(template) %>%
                            summarize(addedcorr=n())
summary_to_exp_nullstats

# List templates
list_of_templates <- as.data.frame(unique(detectlist$template))
list_of_templates

for(thresh in seq(5,25,0.2)){ # Score min, max and increment for evaluation
  
   detectlist_no0$predict=0
   detectlist_no0$predict[detectlist_no0$score>thresh] <- 1 # Find detections where score is greater than evaluation threshold
   detectlist_no0$TP<-0
   detectlist_no0$TP[detectlist_no0$Presence+detectlist_no0$predict==2] <- 1 # if presence = 1 and detections = 1, make TP equal 1
   detectlist_no0$TN<-0
   detectlist_no0$TN[detectlist_no0$Presence+detectlist_no0$predict==0] <- 1 # if presence = 0 and detections = 0, make TN equal 1
 
  
   summarised<-detectlist_no0 %>%
     group_by(template,Filename) %>%
     summarize(TP = max(TP, na.rm = TRUE),
               TN = min(TN, na.rm = TRUE))
   
   
   toexp_tomerge<-summarised %>% 
     group_by(template) %>%
     summarize(TP = sum(TP, na.rm = TRUE),
               TN = sum(TN, na.rm = TRUE),
               n=n())
  
  toexp <- merge(toexp_tomerge, summary_to_exp_nullstats, by="template", all=TRUE )
  toexp$addedcorr[is.na(toexp$addedcorr)] <- 0
  toexp$TNcorr <- toexp$TN+toexp$addedcorr
  toexp$FP <- nosites/2-toexp$TNcorr 
  
  toexp$roc <- (toexp$TP+toexp$TNcorr)/nosites
  toexp$cutoff <- thresh
  roctable <- (rbind(roctable,toexp))
  
}

# Run ROC table
write.csv(roctable, "roctable.csv")
roctable <- read.csv("roctable.csv")

# ROC plots
plot(roctable$roc, roctable$cutoff)
ggplot(roctable, aes(cutoff, roc)) + geom_point() + facet_wrap(~ template)


# Give best score cut-off per template (highest ROC value)
aggregator <- roctable %>% group_by(template) %>% top_n(1, roc)
aggregator <- roctable %>% group_by(template) %>% slice(which.max(roc))
write.csv(aggregator,"Aggregated_BestScoreCutOffs.csv")


# Update template stack based on best score cut-offs and save the updated stack
templateCutoff(btemps) <- aggregator$cutoff
btemps
save(btemps,file=paste(stackname,"_Best_Cutoffs.Rdata", sep=""))


# If relevant, select the subset of templates to be used in the full run
temp1 <- btemps["-20_Nestling1_Huntsman.wav"] # cutoff 15.8
temp2 <- btemps["-23_Nestling2_Amy.wav"] # cutoff 19.2

runnertemps <- combineBinTemplates(temp1, temp2) # In brackets, the template number in the btemps stack
runnertemps
templateCutoff(runnertemps) <- c(15.8, 19.2) # Update their score cut off if necessary (check against aggregator)
runnertemps

# Save the final recogniser
stackname <- "GBC"
save(runnertemps, file=paste(stackname,"_Final_Stack.Rdata", sep=""))


# -----------------------------------------------------------------#
# END
# -----------------------------------------------------------------#

