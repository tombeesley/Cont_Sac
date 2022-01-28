library(tidyverse)

# Define coordinates of AOIs (Left/Top/Right/Bottom pixel values)

fixAOI = c(910,490,1010,590)
fixLargeAOI = c(910,0,1010,1080)
leftAOI = c(50,415,300,665)
leftLargeAOI = c(0,0, 910, 1080)
rightAOI = c(1620,415,1870,665)
rightLargeAOI = c(1010,0,1920,1080)

checkXYinAOI <- function(x,y,AOI) {                     # passes the function 3 variables: x,y,and AOI
  return(x>=AOI[1] & y>=AOI[2] & x<=AOI[3] & y<=AOI[4]) # checks if given coordinates match the respective AOIs (specified above)
}


# createAOIcol is name of function // 
# inArray is the passed dataframe
# inAOI is the variable with which the function will be referred to the above defined AOIs (e.g. inAOI = fixAOI for centre)//
# newColName is the variable needed for the mutate function to create new col(name)
# checkXYinAOI is the function (see above) to check AOIs (not sure why x,y,inAOI is in brackets...)

createAOIcol <- function(inArray, inAOI, newColName) {        # passes the function 3 variables with which create columns to check if fixations were within the areas of interest (AOI)
  inArray %>% mutate(!!newColName := checkXYinAOI(x,y,inAOI)) # dfs that function is passed through will mutate new column which contains another function that checks fixations for AOIs
}



AOIdf <- allData %>%                                                          # maps above functions to allData  
  map(~createAOIcol(., inAOI = fixAOI, newColName = 'centre')) %>%            # maps above 'createAOIcol' function to create new 'centre' col if pixel values match fixAOI
  map(~createAOIcol(., inAOI = fixLargeAOI, newColName = 'centreLarge')) %>% 
  map(~createAOIcol(., inAOI = leftAOI, newColName = 'left')) %>% 
  map(~createAOIcol(., inAOI = leftLargeAOI, newColName = 'leftLarge')) %>% 
  map(~createAOIcol(., inAOI = rightAOI, newColName = 'right')) %>% 
  map(~createAOIcol(., inAOI = rightLargeAOI, newColName = 'rightLarge'))


EditCols <- function(editor) {                                                                  # function to edit all arrays in AOIdf to only include columns necessary for identifying first/last fixations
  editor %>% 
    select(trial,x,y,subCol,periodCol,centre,centreLarge,left,leftLarge,right,rightLarge) %>%   # name of cols in AOIdf that should be selected
    group_by(trial) %>% 
    mutate(fixcount = 1:n()) %>%                                                                # fixcount column counting nr of fixations in a trial
    arrange(trial) %>%                                                                          # reorders rows by 'trial' variable
    group_by(trial,periodCol) %>% 
    mutate(fixcountP = 1:n())                                                                   # fixcount column counting nr of fixations in a period
  
}

EditedAOI <- map(AOIdf, EditCols)                                                               # NameOfNewDF <- map(NameOfOldDF, NameOfFunction)


