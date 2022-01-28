
### ONLY INCLUDES P3 FIXATIONS WHERE LAST P2 FELL IN CENTRE LARGE ###

trial = data.frame(trial=c(1:480))          # create dataframe with 480 rows to merge with new cols below:

trialLevelFixations <- function(inArray) {  # function that creates new dfs that only contain the relevant data for each condition
  
  fixOnCentreP2 <- inArray %>%                                                             # df that only includes data for last fixations of period two
    filter(periodCol==2,fixcountP==max(fixcountP), centreLarge == TRUE) %>%                # only include last fixation in Period 2 and only if it was on centreLarge
      ungroup %>%                                                                          # otherwise cannot only select individual columns
        select(trial,subCol,centre,centreLarge) %>% merge(trial,by = "trial", all = TRUE)  # joins the frames by specified variable names (by = "trial"), to match only on those fields. (*Use by.x and by.y if the matching variables have different names in the different dfs).
  
  firstLRP3 <- inArray %>%                                          # df that only includes data for first fixations of period three
    filter(periodCol==3,(left==TRUE|right==TRUE)) %>%               # only include fixations in period 3 that either went left or right
      filter(fixcountP==min(fixcountP)) %>%                         # only include first fixations
        mutate(firstP3_LEFT=if_else(left==TRUE,TRUE,FALSE)) %>%     # mutate col indicating if fixation was left (if left "true", otherwise "false")
          ungroup() %>%                                             # otherwise cannot single out variables for selection
            select(trial,subCol,firstP3_LEFT) %>%                   # select only those variables relevant for this df (first fixation of P3 that went left/right)
              merge(trial,by = "trial", all = TRUE)                 # joins the frames by specified variable names (by = "trial"), to match only on those fields. (*Use by.x and by.y if the matching variables have different names in the different dfs).
  
  lastLRP3 <- inArray %>%                                           # df that only includes data for last fixations of period three
    filter(periodCol==3,(left==TRUE|right==TRUE)) %>%
      filter(fixcountP==max(fixcountP)) %>%                         # only include last fixations
        mutate(lastP3_LEFT=if_else(left==TRUE,TRUE,FALSE)) %>%      
          ungroup() %>%
            select(trial,subCol,lastP3_LEFT) %>% merge(trial,by = "trial", all = TRUE) 
  
  return(cbind(fixOnCentreP2,firstLRP3,lastLRP3) %>% subset(., select = which(!duplicated(names(.)))))  #binds the dfs created above and out of these only selects each variable once (i.e. doesn't select duplicates such as "trial" or "subCol")
  
}

newDF <- map(EditedAOI,trialLevelFixations)                               # creates new df out of the dfs that were bound together with function above


EyeData <- do.call("rbind", newDF)                                        # changes large list into a dataframe


TrialData <- read_csv(file = 'allData_td.csv')                            # import trial data


TrialData <- TrialData %>% filter(Subj != 108, Subj != 120, Subj != 144)  # Remove participants 108,120,& 144 to match allData file


CombinedData <- bind_cols(EyeData, TrialData)                             # combine columns from EyeData and TrialData

