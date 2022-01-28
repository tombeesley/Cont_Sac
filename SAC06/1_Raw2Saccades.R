## @knitr Process_Raw_to_Saccades

rm(list=ls())
library(tidyverse)
library(saccades)

patternTypes <- c("fix","inst","stim") #variable that differentiates between the 3 different csv files for the different experimental periods

fnams <- list.files("CSV Raw Data", pattern = "fix", full.names = TRUE) # needed for reading data

allData <- NULL # empty array to fill with processed data from all participants' raw data

for (subj in 1:length(fnams)) { # telling it to run through all the "fix" period files in the raw data file
  
  # creates variable to extract file name (counting backwards from file name characters to get participant nr)
  subNum <- substr(fnams[subj], start = nchar(fnams[subj])-6, stop = nchar(fnams[subj])-4)
  print(subNum)
  subFiles <- list.files("CSV Raw Data", pattern = subNum, full.names = TRUE) # needed for reading data
  
  subData <- NULL 
  print(subNum)
  
  for (period in 1:3) { #for loop that goes through all files pertaining to periods "fix","inst",&"stim"
    
    print(period)
    
    curFile <- subFiles[str_detect(subFiles,pattern=patternTypes[period])] #variable that detects files of the relevant period (fix,instr,stim)
    
    periodRaw <- read_csv(curFile, col_types = cols(), col_names = FALSE) # read the data from csv
    
    # run saccades program
    
    colnames(periodRaw) <- c("time","x","y","trial")
    periodFix <- subset(detect.fixations(periodRaw), event=="fixation")
    
    periodCol <- rep(period,nrow = periodFix) # generates an array that matches same nr of rows as 'periodFix' and adds an extra 'period' column
    subCol <- rep(subNum,nrow = periodFix) # generates an array that matches same nr of rows as 'periodFix' and adds 'subject nr' column
    
    periodFix <- cbind(subCol,periodCol,periodFix) # overwrites 'saccades' periodFix by combining existing periodFix with subject nr col and period col
    
    subData <- rbind(subData,periodFix) 
  }
  
  allData[[subj]] <- subData
  
}
