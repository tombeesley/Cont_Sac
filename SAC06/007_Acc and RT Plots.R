library(tidyverse)
library(gridExtra)
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')
library('ggsignif')


## Plotting Accuracy ##



overallAcc <- mean(NoOutliers$Acc)       # calculate overall mean for accuracy ratings

TT1Acc <- NoOutliers %>%                                   # calculate mean and SD for trial type 1 (normal/training)
  filter(Trial.Type == 1) %>% 
  summarise(MeanTT1 = mean(Acc), SDTT1 = sd(Acc))

TT2Acc<- NoOutliers %>% 
  filter(Trial.Type == 2) %>%                            # calculate mean and SD for trial type 2 (normal)
  summarise(MeanTT2 = mean(Acc), SDTT2 = sd(Acc))

TT3Acc<- NoOutliers %>% 
  filter(Trial.Type == 3) %>%                            # calculate mean and SD for trial type 3 (reversal)
  summarise(MeanTT3 = mean(Acc), SDTT3 = sd(Acc))

AccSummaryStats <- cbind(TT1Acc, TT2Acc, TT3Acc)         # combine all summary stats into one df

AccPlotData <- NoOutliers %>%                              # create df for plotting accuracy data
  group_by(epoch, Trial.Type) %>%                        # group data by epoch (for x-axis) and trial type (for plot legend)
  summarise(meanAcc = mean(Acc), SDAcc = sd(Acc))        # redefine variables as factors for plotting
  

tiff("AccPlot.NO.tiff", units="in", width=5, height=5, res=300)                                                  # generate high resolution plot (tiff)        
ggplot(AccPlotData, aes(epoch, meanAcc, shape = Trial.Type, group = Trial.Type)) +                            # specify epoch as x-axis, accuracy as y-axis, and trial type levels as shapes connected by lines
  labs(x = "Epoch of 40 trials" ,y = "Mean response accuracy") +                                              # specify x and y axis labels
  geom_line() +                                                                                               # specify type of plot
  geom_point() +                                                                                              # combine line with points (can only do this if you use 'group =' when specifying x and y axes)
  ylim(0.5,1) +                                                                                               # specify y axis ticks (and intervals between them with 'by =')
  scale_shape_manual(values = c(17, 16, 5),                                                                   # specify shapes to identify levels of the variable (trial type)
                     name = "Trial Type",                                                                     # specify legend title
                     labels = c("Training (normal)", "Normal", "Reversed")) +                                 # specify labels for legend
  theme_classic() +                                                                                           # specify background theme of plot
  theme(legend.position = c(0.5, 0.2), legend.title.align=0.5)                                                # move legend into bottom centre of plot and centre legend title
dev.off()

##################################################################################

## Plotting RT ##

overallRT <- mean(NoOutliers$RT)       # calculate overall mean for accuracy ratings


TT1RT <- NoOutliers %>%                                   # calculate mean and SD for trial type 1 (normal/training)
  filter(Acc == 1, Trial.Type == 1) %>% 
  summarise(MeanTT1 = mean(RT), SDTT1 = sd(RT))

TT2RT <- NoOutliers %>% 
  filter(Acc == 1, Trial.Type == 2) %>%                            # calculate mean and SD for trial type 2 (normal)
  summarise(MeanTT2 = mean(RT), SDTT2 = sd(RT))

TT3RT <- NoOutliers %>% 
  filter(Acc == 1, Trial.Type == 3) %>%                            # calculate mean and SD for trial type 3 (reversal)
  summarise(MeanTT3 = mean(RT), SDTT3 = sd(RT))

RTSummaryStats <- cbind(TT1RT, TT2RT, TT3RT)         # combine all summary stats into one df


RTPlotData <- NoOutliers %>%                           # create df for plotting accuracy data
  group_by(epoch, Trial.Type) %>%                    # group data by epoch (for x-axis) and trial type (for plot legend)
  summarise(meanRT = mean(RT), SDRT = sd(RT))        # generate means and SDs for each trial type by epoch


tiff("RTPlot.NO.tiff", units="in", width=5, height=5, res=300)                             # generate high resolution plot (tiff)        
ggplot(RTPlotData, aes(epoch, meanRT, shape = Trial.Type, group = Trial.Type)) +        # specify epoch as x-axis, accuracy as y-axis, and trial type levels as shapes connected by lines
  labs(x = "Epoch of 40 trials" ,y = "Mean reaction time") +                            # specify x and y axis labels
  geom_line() +                                                                         # specify type of plot
  geom_point() +                                                                        # combine line with points (can only do this if you use 'group =' when specifying x and y axes)
  ylim(800,1400) +                                                                      # specify y axis ticks (and intervals between them with 'by =')
  scale_shape_manual(values = c(17, 16, 5),                                             # specify shapes to identify levels of the variable (trial type)
                     name = "Trial Type",                                               # specify legend title
                     labels = c("Training (normal)", "Normal", "Reversed")) +           # specify labels for legend
  theme_classic() +                                                                     # specify background theme of plot
  theme(legend.position = c(0.5, 0.8), legend.title.align=0.5)                          # move legend into bottom centre of plot and centre legend title
dev.off()

#######################################################

## Plotting PROPORTION OF RESPONSES (Accurate, Inaccurate, and unexplained) ##

AccFactor  <- PlotPrep %>%                        # redefine variables as factors for plotting
  mutate(Acc = as.factor(Acc)) %>%                # numeric values to factor
  group_by(Subj, Trial.Type) %>%
  mutate(zRT = scale(RT)) %>%                     # remove outliers (RT)
  filter(zRT < 2.5)

is.factor(AccFactor$Acc)     # ensure Acc successfully converted into a factor (for plotting)

#### DON'T TOUCH THIS WORKS !!! ###

AllRespProp.test <- AccFactor %>%        # create df to calculate proportions of the specified variables
  group_by(Stage, Trial.Type) %>%       # specify grouping variables
  count(Stage,Trial.Type,Acc) %>%       # specify which proportions need calculating
  mutate(prop = prop.table(n))          # create proportion column in new df

tiff("AllRespProp.test.tiff", units="in", width=5, height=5, res=300)                               # to save/export the bar chart as a .tiff file
ggplot(AllRespProp.test, aes(Trial.Type, prop, fill = Acc,label = scales::percent(prop))) +         # define x and y axes, and which proportions should be plotted (fill)
  geom_bar(stat = 'identity', position = 'dodge',colour="black") +                                                 # plot bar chart with different response categories (inaccurate, accurate, and unexplained plotted next to each other 'dodge')
  ylab("Proportion of Responses") +                                                                 # label y-axis
  scale_x_discrete(
    factor(1:3),                                                                                    # specify number of positions on x-axis
    labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal")) +      # Rename tick mark labels on x-axis (use \n between words to add line break)
  scale_fill_manual(
    values = c("white", "#999999", "gray83"),                                                        # specify fill colours for bars (no longer need third because removed unexplained - i.e. Acc == 9999)
    name = "Responses",                                                                             # specify legend title
    labels = c("Inaccurate", "Accurate", "Unexplained")) +                                          # specify labels for legend
  theme_classic() +                                                                                 # specify aesthetic theme (no background lines)
  geom_text(position = position_dodge(width = 1.0),                                                 # move text to center of bars
            vjust = -0.5,                                                                           # nudge above top of bar
            size = 2.5) +                                                                           # font size
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +                                                    # show proportions as percentages
  theme(
    axis.title.x = element_blank(),                                                                 # remove x-axis title
    axis.title.y = element_text(size=14, face="bold"),                                              # modify y-axis title text
    axis.text.x = element_text(colour = "black", face = "bold"))                                    # modify x-axis tick text
dev.off()                                                                                           # complete tiff conversion


