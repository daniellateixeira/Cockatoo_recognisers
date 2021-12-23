# ----------------------------------------------------------------------#
# CALL RECOGNISER: SE RED-TAILED BLACK-COCKATOO
# This script uses Binary Point Matching from the package monitoR
# Updated from D. Teixeira PhD to improve template selection and testing
# Script written by D. Teixeira & S. Linke
# ----------------------------------------------------------------------#

# Set working directory
setwd("C:\\Users\\danie\\Google Drive\\PhD\\Data and analysis\\Recogniser nestling and adults\\SE RTBC")

# Garbage collection
rm(list=ls()) #removes everything done to date
gc(reset=T)

# -----------------------------------------------------------------#
# RECOGNISER
# -----------------------------------------------------------------#

# Load packages
library(monitoR)
library(tuneR)
#library(RODBC)
library(seewave)
# library(lubridate)
# library(reshape2)
# library(dplyr)
# library(ggplot2)

# Load the template stack
load(".\\RTBC_Final_Stack.Rdata")
btemps <- runnertemps
btemps

# Set the filepath to the surveys you want to run the recogniser on
directory <- "E:\\Surveys\\Loner"
surveys <- list.files(directory, pattern=".wav", full.names = TRUE, recursive = TRUE) # Need recursive = TRUE to read all wav files in folders
surveys

# Run the recogniser
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
