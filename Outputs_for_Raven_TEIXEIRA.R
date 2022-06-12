
# --------------------------------------------------------- #
#---------------- CLEAN OUTPUT FILES ---------------------- #
# This script takes the output csv files from monitoR
# Duplicate detections are removed
# Four columns are added to allow for import into Raven Pro
# Low Freq and High Freq are set tp 0kHz and 20,000kHz
# Box width is +- 0.25 seconds from the detections
# Script written by D. Teixeira & S. Linke
# --------------------------------------------------------- #

# Garbage collection
rm(list=ls()) 
gc(reset=T)

# Packages
library(dplyr)

# Set working directory
setwd("G:\\My Drive\\Presentations\\monitoR_demo")

# List the monitoR output files
outputfiles <- list.files(pattern = "\\.csv", recursive = TRUE, full.names = TRUE)
outputfiles

# Run the loop to clean the files
for(i in outputfiles) {
     
      # Read the csv files 
      data <- read.csv(i, header = TRUE)
      
      rowcount <- nrow(data)
      
      if (rowcount !=0){ # If rowcount is not zero (i.e. if there were detections), perform output cleaning on files
        
            
              # Sort by fulltime
              data <- data[order(-data$fulltime),]
              
              # Create two new columns
              data$difference<-NULL 
              data$Duplicate<-NULL 
              
        
              #Identify which rows are duplicates (within a buffer of 1 second)
              data$Duplicate[1] <-"No" # Fill the first row's Duplicate with No
              buffer=1
              for(r in 1:nrow(data)){
                    data$difference[r] <- data$fulltime[r] - data$fulltime[r+1] # Calculate the difference between the two rows in seconds
                        ifelse((data$difference[r] <= buffer), # If the difference is smaller than the 1 second buffer
                               data$Duplicate[r] <- "Yes", # Label as "yes" to duplicate
                               data$Duplicate[r] <- "No") # Otherwise "no"
                }
        
              
              # Drop duplicates
              data <- subset(data, data$Duplicate != "Yes")
             
              # Add new columns of detection frequencies and duration
              data$Low <- 0 
              data$High <- 16000
              data$Begin <- data$fulltime-0.5
              data$End <- data$fulltime+0.5
              
             
              # Rename columns per Raven requirements
              colnames(data)[colnames(data)=="Low"] <- "Low Freq (Hz)"
              colnames(data)[colnames(data)=="High"] <- "High Freq (Hz)"
              colnames(data)[colnames(data)=="Begin"] <- "Begin Time (s)"
              colnames(data)[colnames(data)=="End"] <- "End Time (s)"
              
          
              # Output Name
              outputname <- gsub('\\.csv', '', i)
              
              # Export
              write.table(data, paste0(file = outputname, '_Raven', '.txt'), sep='\t', col.names = TRUE, row.names=FALSE, quote = FALSE)
          
      }
  }



