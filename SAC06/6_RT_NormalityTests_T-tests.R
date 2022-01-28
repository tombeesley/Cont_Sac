library(ggpubr)
library(tidyverse)
library(gridExtra)
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')
library('ggsignif')   # for adding significance level stars to plots



### Clean data for plotting and analysis ### 


PlotPrep <- drop_na(data,centre:LastFixPredictive)        # remove all na data from columns ranging between centre and LastFixPredictive

PlotPrep  <- PlotPrep %>%                             # redefine variables as factors for plotting
  mutate(epoch = as.ordered(epoch)                    # numeric values to ordered factor
         , Stage = as.ordered(Stage)                  # numeric to ordered factor
         , Trial.Type = as.ordered(Trial.Type)) %>%   # numeric to ordered factor
  filter(!Acc == 9999, !RT == 9999)                   # filter out eye tracking errors (9999)

# REMOVE OUTLIERS

# filter out all datapoints with RT greater than 2.5 SDs
NoOutliers <- PlotPrep %>% 
  group_by(Subj, Trial.Type) %>%
  mutate(zRT = scale(RT)) %>%           # calculate z-score for RT
  filter(zRT < 2 & zRT > -2 )           # remove all trials with RT z-scores greater than 2 standard deviations from 0 (supported by exploration using QQ plot, after removal of only those RTs that were greater than 2.5 SD still showed a skewed distribution)


#filter out participants for whom less than 50% of data was retained (e.g. due to eye tracker malfunctions or inattention)
propEG <- NoOutliers %>% 
  group_by(Subj) %>% 
  summarise(propED = n()/480)       # create column that calculates each participants proportion of retained trials (out of the full 480 trials)

ggplot(propEG, aes(y = propED)) + geom_boxplot() # plot proportions of retained eye gaze data to visualise outliers

arrange(propEG, propED) # arrange participants by proportion of retained eye gaze data to identify number of participants who retained less than 50%

# filter out participants that retained less than 50% eye gaze data:
PlotPrep <- PlotPrep %>% 
  filter(!Subj == 106, 
         !Subj == 112,
         !Subj == 157,
         !Subj == 151,
         !Subj == 140,
         !Subj == 159,
         !Subj == 103,
         !LastFixPredictive == FALSE) # filter first fixation errors to only include those that are followed by correct last fixation


NoOutliers <- NoOutliers %>% 
  filter(!Subj == 106, 
         !Subj == 112,
         !Subj == 157,
         !Subj == 151,
         !Subj == 140,
         !Subj == 159,
         !Subj == 103,
         !LastFixPredictive == FALSE) # filter first fixation errors to only include those that are followed by correct last fixation


##################################################################################

### Explore/visualise RT data incl all 3 trial types ### 

# incl. OUTLIERS
 
# MEANS incl outliers
RTmeans_O <- PlotPrep %>%               # df with RT means in all TTs
  filter(!Acc == 0) %>%                 # remove inaccurate data
  group_by(Subj, Trial.Type) %>%        # group data by subjects and trial type 
  summarise(meanRT = mean(RT),          # calculate each participant's mean RT for each trial type
            SDRT = sd(RT))              # calculate SD for RT

# boxplot looking at RTs in the 3 different TTs (including outliers)
TTboxplot.RT_O <- ggplot(RTmeans_O) + 
  geom_boxplot(mapping = aes(Trial.Type, meanRT)) + 
  labs(x = "Trial Type", y = "Mean response time") +                                                 # change names of x and y labels
  theme_classic()                                                                                    # need to create this separately so that it can be added to multiplot with patchwork
ggsave("TTboxplot.RT_O.tiff", units="in", width=5, height=5, dpi=300, compression = 'lzw')             #to save/export the boxplot as a compressed .tiff file

# Z-SCORES
zRT_O <- PlotPrep %>%              # df with z-scores of RT 
  filter(!Acc == 0) %>%            # remove inaccurate data
  group_by(Subj, Trial.Type) %>%   # group data by subjects and trial type 
  mutate(zRT = scale(RT))          # calculate z-scores for RT data

#boxplot looking at zRTs in the 3 different TTs (including outliers)
TTboxplot.zRT_O <- ggplot(zRT_O) + 
  geom_boxplot(mapping = aes(Trial.Type, zRT)) + 
  labs(x = "Trial Type", y = "Standardized response time\n (z-scores)") +     
  theme_classic()
ggsave("TTboxplot.zRT_O.tiff", units="in", width=5, height=5, dpi=300, compression = 'lzw')             #to save/export the boxplot as a compressed .tiff file



# NO OUTLIERS

# MEANS excl. outliers
RTmeans <- NoOutliers %>%               # df with RT means in all TTs
  filter(!Acc == 0) %>%                 # remove inaccurate data
  group_by(Subj, Trial.Type) %>%        # group data by subjects and trial type 
  summarise(meanRT = mean(RT),          # calculate each participant's mean RT for each trial type
            SDRT = sd(RT))              # calculate SD for RT

# boxplot looking at RTs in the 3 different TTs (NO outliers)
TTboxplot.RT <- ggplot(RTmeans) + 
  geom_boxplot(mapping = aes(Trial.Type, meanRT)) + 
  labs(x = "Trial Type", y = "Mean response time") +                                                 # change names of x and y labels
  theme_classic()                                                                                    # need to create this separately so that it can be added to multiplot with patchwork
ggsave("TTboxplot.RT.tiff", units="in", width=5, height=5, dpi=300, compression = 'lzw')             #to save/export the boxplot as a compressed .tiff file

# Z-SCORES
zRT <- NoOutliers %>%              # df with z-scores of RT 
  filter(!Acc == 0) %>%            # remove inaccurate data
  group_by(Subj, Trial.Type) %>%   # group data by subjects and trial type 
  mutate(zRT = scale(RT))          # calculate z-scores for RT data

#boxplot looking at zRTs in the 3 different TTs (including outliers)
TTboxplot.zRT <- ggplot(zRT) + 
  geom_boxplot(mapping = aes(Trial.Type, zRT)) + 
  labs(x = "Trial Type", y = "Standardized response time\n (z-scores)") +     
  theme_classic()
ggsave("TTboxplot.zRT.tiff", units="in", width=5, height=5, dpi=300, compression = 'lzw')             #to save/export the boxplot as a compressed .tiff file







# density plot of RT means 
DensityRT <- zRT %>% 
  ggplot(aes(zRT, colour=factor(Trial.Type))) + 
  geom_density(aes(y=..scaled..)) + 
  scale_color_discrete(name='Trial Type',labels=c("Training (Normal)", "Normal", "Reversed")) +
  labs(x = "Standardized response time\n (z-scores)") +                                               # change names of x and y labels
  theme_classic()
#to save/export the boxplot as a .tiff file
tiff("DensityRT.tiff", units="in", width=5, height=5, res=300)
DensityRT
dev.off()

# QQplot of RT means 
QQplot.RT <- ggqqplot(zRT$zRT) +
  labs(x ="Q-Q Plot of Response Time", y = "")
tiff("QQplot.RT.tiff", units="in", width=5, height=5, res=300)
QQplot.RT
dev.off()

##################################################################################
##################################################################################
# ----------- Trial Types 2 & 3 ONLY ---------------

# COMPARING MEANS

# RT

ComparingMeansRT <- NoOutliers %>%           # prepare data for t-test/wilcoxon paired samples test
  filter(!Trial.Type == 1, !Acc == 0) %>%    # remove training trials (TT1) and inaccurate trials (important for RT data)
  group_by(Subj, Trial.Type) %>%             # group data by subjects and trial type
  summarise(meanRT = mean(RT),               # calculate each participant's mean RT for each trial type
            SDRT = sd(RT))                   # calculate SD for RT


# assessing RT for normality after removing outliers (only TT2 and TT3)

# density plot of RT means 
ComparingMeansDensityRT <-  ComparingMeansRT %>% 
  ggplot(aes(meanRT, colour=factor(Trial.Type))) + 
  geom_density(aes(y=..scaled..)) + 
  scale_color_discrete(name='Trial Type',labels=c("normal", "reversed")) +
  labs(x = "Mean response time", y = "") +
  theme_classic()
#to save/export the boxplot as a .tiff file
tiff("ComparingMeansDensityRT.tiff", units="in", width=5, height=5, res=300)
ComparingMeansDensityRT
dev.off()


QQ.ComparingMeans.RT <- ggqqplot( ComparingMeansRT$meanRT) +
  labs(x ="Q-Q Plot of Response time", y = "")
# create QQplot of RT including only TT2 and TT3 to assess normality (NO OUTLIERS)
tiff("QQ.ComparingMeans.RT.tiff", units="in", width=5, height=5, res=300)
QQ.ComparingMeans.RT
dev.off()

# conduct shapiro-wilk test of normality on Acc data
RT.shapiro.test <- shapiro.test(ComparingMeansRT$meanRT) # test if data are normally distributed and suitable for parametric testing

# ---------- WILCOXON ---------- #

# means of Accuracy scores are compared using non-parametric test (paired samples Wilcoxon test) because data is not normally distributed 

Wilcox.RT <- wilcox.test(meanRT ~ Trial.Type, data = ComparingMeansRT, paired=TRUE)

# Calculate the standardised z statistic 
RT.Zstat <- qnorm(Wilcox.RT$p.value/2)


RTMedian <- PlotPrep.2 %>%              # calculate median 
  filter(Trial.Type != 1) %>%                            
  group_by(Trial.Type) %>% 
  summarise(TTmedian = median(RT)) 


# non significant difference between response time (RT) scores (Z = -0.24, p > .05) in normal (Median = 1059.373) and reversal (Median = 1078.341) trials.


##################################################################################

