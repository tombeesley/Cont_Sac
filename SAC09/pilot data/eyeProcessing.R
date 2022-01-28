# the following packages require install via "BiocManager"
#
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(version = "3.14")
BiocManager::install("rhdf5")
BiocManager::install("IRanges")

library(tidyverse)
library(IRanges)
library(rhdf5) # for reading hdf5 files
library(saccades) # for processing fixations
library(patchwork)
library(data.table)
library(fuzzyjoin)
library(janitor)

# # psychoPy timestamps
# ppTS <- read.csv("psychoPyTS.csv")
# 
# # this gives you the fields of the hdf5 file
contents <- h5ls("eye_events.EDF")
# 
# # get the data from the hdf5 file
msg <- h5read(file = "Data/events.hdf5", name = "data_collection/events/experiment/MessageEvent")
eyeData <- h5read(file= "Data/events.hdf5", name = "/data_collection/events/eyetracker/MonocularEyeSampleEvent")
save(msg,eyeData, file = "rawDataUnProc.RData")

load("rawDataUnProc.RData")

allRaw <- NULL
allFixations <- NULL

for (pNum in 1:2) {#distinct(msg, session_id)) {

  # from the msg data, obtain timestamps for each trial, clean and tidy the dataframe
  pTS <- msg %>%
    filter(session_id == pNum) %>% 
    select(device_time, text) %>% # timestamp and message text
    separate(text, into = c('event', 'blockTrial'), sep = "_") %>%
    filter(!event == "Suj") %>% 
    mutate(trial = rep(1:288, each = 6)) %>% # create a trial number for each row (i.e., 1 1 2 2 3 3 etc)
    mutate(event = str_remove(event, pattern = "Dev")) %>% # fix devaluation events having different names
    mutate(event = recode(event, "Cookie" = "cookie", "feedbTxt" = "feedbackTxt")) %>% # fix inconsistent naming
    pivot_wider(names_from = event, values_from = device_time) %>% # spread the timestamps to new columns (for trial, and value)
    clean_names() # clean up the variable names
  
  # gets the timestamps that we want and calls them start and end
  pTS <- pTS %>% 
    select(trial, start = cookie, end = feedback_txt) 
  
  # these columns are redundant (contain 0s), so I've deleted them before the summary
  pEye <- eyeData %>%
    filter(session_id == pNum) %>% 
    select(-(gaze_z:raw_y), -(pupil_measure2:pupil_measure2_type), -(velocity_x:velocity_xy))
  
  # "time" column seems to have regular steps, reflecting millisecond intervals
  pEye <- pEye %>% 
    select(time, gaze_x, gaze_y) %>% 
    filter(between(gaze_x, -1000, 1000), between(gaze_y, -500, 500))
  
  # stitches together the eye data with the trialTS
  combinedData <- pTS %>% 
    interval_right_join(y = pEye, by = c(start = "time", end = "time")) %>% 
    select(time, x = gaze_x, y = gaze_y, trial) %>% 
    group_by(trial) %>% 
    mutate(time = round(time*1000 - time[1]*1000)) %>% # converts time to ms since trial started
    ungroup() %>% 
    drop_na()
  
  # doesn't recognise samples in chronological order
  # try looping through trials

  combinedData <- combinedData %>% 
    group_by(trial, time) %>% 
    slice_head(n = 1)
    
    d <- combinedData[1:1000,]
    d$time = 1:1000
    d$time[50] <- d$time[49]
    detect.fixations(d)
    
  }
  
  data(samples)
  detect.fixations(samples)
  
  
  fixations <- subset(detect.fixations(combinedData2), event=="fixation") # pass raw data through fixation detection function
  
  # add a fixation order variable, by trial
  fixations <- fixations %>% 
    group_by(trial) %>%
    mutate(fixNum = 1:n()) %>% 
    ungroup()

  allFixations <- bind_rows(allFixations,cbind(pNum,fixations))
  
  #arrange combined data and append
  combinedData <- combinedData %>% 
    mutate(pNum = pNum) %>% 
    select(pNum, trial, time, x, y)
  
  allRaw <- bind_rows(allRaw, combinedData)

}

write_csv(allFixations, "allFixations.csv")
write_csv(allRaw, "allRawProc.csv")
