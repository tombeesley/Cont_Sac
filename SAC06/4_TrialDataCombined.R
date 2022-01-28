
alt_switch <- CombinedData %>% 
  mutate(
  epoch = ceiling(Block/5),
  Stage = if_else(
    epoch <= 8, "1",                    # "1" = Stage 1 (all normal training trials) 
        if_else(epoch > 8, "2", "NA")),     # "2" = Stage 2 (experimental trials, normal and reversal trials)
  Trial.Type = if_else(                  
    epoch <= 8, "1",                    # "1" = training trials in stage 1 (all normal)
    if_else(
      epoch > 8 &
        Norm_Rev == 0, "2",             # "2" = normal trials in stage 2 
      if_else(
        epoch > 8 &
          Norm_Rev == 1, "3", "NA"))),
  F1Predictive = if_else(                  # first fixation in period 3 was on predictive cue if:
    centreLarge == T &                     # last fixation in period 2 was on centreLarge
      firstP3_LEFT == T &                  # first fixation in P3 went left AND
        Cue_L <= 2 &                       # the cue shown in left AOI was either cue 1 or cue 2 AND
          Trial.Type <= 2, T,              # it was a normal trial (i.e. trial type 1 and 2 = normal trials, trial type 3 = reversal trials)
    if_else(                               # OR 
      centreLarge == T &
        firstP3_LEFT == F &                # first fixation in P3 was right (firstP3_LEFT == F translates to RIGHT here) AND
          Cue_R <= 2 &                     # the cue shown in right AOI was either cue 1 or 2 AND
            Trial.Type <= 2, T,            # it was a normal trial (not reversal trial)
      if_else(                             # OR
        centreLarge == T &
          firstP3_LEFT == T &              # first fixation was left AND
            Cue_L >= 3 &                   # cue in left AOI was 3 or 4 AND
              Trial.Type >= 3, T,          # it was a reversal trial
        if_else(                           # OR
          centreLarge == T &
            firstP3_LEFT == F &            # first fixation was right AND
            Cue_R >= 3 &                   # cue in right AOI was 3 or 4 AND
            Trial.Type >= 3, T, F)))),     # it was a reversal trial, then TRUE, otherwise FALSE
  LastFixPredictive = if_else(             # Last fixation in period 3 was on predictive cue if:
    centreLarge == T &
      lastP3_LEFT == T &                   # last fixation was left &
        Cue_L <= 2 &                       # cue in left AOI was 1 or 2
          Trial.Type <= 2, T,              # it was normal trial type
    if_else(                               # OR
      centreLarge == T &
        lastP3_LEFT == F &                 # last fixation was right &
          Cue_R <= 2 &                     # cue in right AOI was 1 or 2 &
            Trial.Type <= 2, T,            # it was a normal trial type
      if_else(                             # OR
        centreLarge == T &
          lastP3_LEFT == T &               # last fixation was left &
            Cue_L >= 3 &                   # cue in left AOI was 3 or 4
              Trial.Type >= 3, T,          # it was a reversal trial
        if_else(                           # OR
          centreLarge == T &
            lastP3_LEFT == F &             # last fixation was left & 
              Cue_R >= 3 &                 # cue in right AOI was 3 or 4 &   
                Trial.Type >= 3, T, F))))) # it was a reversal trial


data <- select(alt_switch,-subCol)

data <- data[,c(6:8,19:21,9,1,10:18,2,3,22,23)]      # rearrange the columns


rm(list=setdiff(ls(), "data"))                    # remove all variables/objects except the one named. Very useful!

save.image("FinalDF.RData")


