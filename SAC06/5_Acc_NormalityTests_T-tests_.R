library(ggpubr)
library(tidyverse)
library(gridExtra)
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')
library('ggsignif')

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
  mutate(zRT = scale(RT)) %>%
  filter(zRT < 2)


#filter out participants for whom less than 50% of data was retained (e.g. due to eye tracker malfunctions or inattention)
propEG <- NoOutliers %>% 
  group_by(Subj) %>% 
  summarise(propED = n()/480)

ggplot(propEG, aes(y = propED)) + geom_boxplot() # plot proportions of retained eye gaze data to visualise outliers

arrange(propEG, propED) # arrange participants by proportion of retained eye gaze data to identify number of participants who retained less than 50%

# filter out participants that retained less than 50% eye gaze data:
PlotPrep.2 <- PlotPrep %>% 
  filter(!Subj == 106, 
         !Subj == 112,
         !Subj == 157,
         !Subj == 151,
         !Subj == 140,
         !Subj == 159,
         !Subj == 103,
         !LastFixPredictive == FALSE) # filter first fixation errors to only include those that are followed by correct last fixation


NoOutliers.2 <- NoOutliers %>% 
  filter(!Subj == 106, 
         !Subj == 112,
         !Subj == 157,
         !Subj == 151,
         !Subj == 140,
         !Subj == 159,
         !Subj == 103,
         !LastFixPredictive == FALSE) # filter first fixation errors to only include those that are followed by correct last fixation



##################################################################################
#   RT     --- ALL 3 TRIAL TYPES ---

 ### Explore RT data (for outliers) ### 
 

 # MEANS 
 RTmeans <- PlotPrep.2 %>%                 # df with RT means in all TTs
   filter(!Acc == 0) %>%                 # remove inaccurate data
   group_by(Subj, Trial.Type) %>%        # group data by subjects and trial type 
   summarise(meanRT = mean(RT),          # calculate each participant's mean RT for each trial type
             SDRT = sd(RT))              # calculate SD for RT

# boxplot looking at RTs in the 3 different TTs (including outliers)
TTboxplot.RT <- ggplot(RTmeans) + 
  geom_boxplot(mapping = aes(Trial.Type, meanRT)) + 
  labs(x = "Trial Type", y = "Mean response time") +                            # change names of x and y labels
  theme_classic()  # need to create this separately so that it can be added to multiplot with patchwork
#to save/export the boxplot as a .tiff file
tiff("TTboxplot.RT.tiff", units="in", width=5, height=5, res=300)
TTboxplot.RT
dev.off()


 # Z-SCORES
 zRT <- PlotPrep.2 %>%                # df with z-scores of RT ##
   filter(!Acc == 0) %>%            # remove inaccurate data
   group_by(Subj, Trial.Type) %>%   # group data by subjects and trial type 
   mutate(zRT = scale(RT))          # calculate z-scores for RT data

#boxplot looking at zRTs in the 3 different TTs (including outliers)
TTboxplot.zRT <- ggplot(zRT) + 
  geom_boxplot(mapping = aes(Trial.Type, zRT)) + 
  labs(x = "Trial Type", y = "Standardized response time\n (z-scores)") +     
  theme_classic()
#to save/export the boxplot as a .tiff file
tiff("TTboxplot.zRT.tiff", units="in", width=5, height=5, res=300)
TTboxplot.zRT
dev.off()

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


#   ACC     --- ALL 3 TRIAL TYPES ---

### Explore Accuracy data (for outliers) ### 

# MEANS 
AccMeans <- PlotPrep.2 %>%                # prepare data for t-test 
  group_by(Subj, Trial.Type) %>%        # group data by subjects and trial type 
  summarise(meanAcc = mean(Acc),        # calculate each participant's mean RT for each trial type
            SDAcc = sd(Acc))            # calculate SD for RT


#boxplot looking at Acc in the 3 different TTs 
TTboxplot.Acc <- ggplot(AccMeans) +                                   # need to create this separately so that it can be added to multiplot with patchwork
  geom_boxplot(mapping = aes(Trial.Type, meanAcc)) +                  # specify axes
  labs(x = "Trial Type", y = "Accuracy") +                            # change names of x and y labels
  theme_classic()                                                     # specify theme of coordinate grid
#to save/export the boxplot as a .tiff file
tiff("TTboxplot.Acc.tiff", units="in", width=5, height=5, res=300)
TTboxplot.Acc
dev.off()


# assessing Accuracy for normality

# density plot of Accuracy means 
DensityAcc <- AccMeans %>% 
  ggplot(aes(meanAcc, colour=factor(Trial.Type))) + 
  geom_density(aes(y=..scaled..)) + 
  scale_color_discrete(name='Trial Type',labels=c("Training (Normal)", "Normal", "Reversed")) +    # change name of legend labels
  labs(x = "Accuracy") +                                                                           # change names of x and y labels
  theme_classic()
#to save/export the boxplot as a .tiff file
tiff("DensityAcc.tiff", units="in", width=5, height=5, res=300)
AccMeans %>% ggplot(aes(meanAcc, colour=factor(Trial.Type))) + 
  geom_density(aes(y=..scaled..)) + 
  scale_color_discrete(name='Trial Type',labels=c("Training (Normal)", "Normal", "Reversed")) +
  labs(x = "Accuracy") +      
  theme_classic()
dev.off()

# QQplot of Accuracy means 
QQplot.Acc <- ggqqplot(AccMeans$meanAcc) +
  labs(x ="Q-Q Plot of Accuracy", y = "")
tiff("QQplot.Acc.tiff", units="in", width=5, height=5, res=300)
ggqqplot(AccMeans$meanAcc) +
  labs(x ="Q-Q Plot of Accuracy", y = "")
dev.off()


# Can see that these data are very skewed - not suitable for parametric testing

##################################################################################
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

# Acc

ComparingMeansAcc <- NoOutliers %>%     # prepare data for t-test/wilcoxon paired samples test
  filter(!Trial.Type == 1) %>%          # remove training trials (TT1)
  group_by(Subj, Trial.Type) %>%        # group data by subjects and trial type
  summarise(meanRT = mean(RT),          # calculate each participant's mean RT for each trial type
            SDRT = sd(RT),              # calculate SD for RT
            meanAcc = mean(Acc),        # calculate each participant's mean Acc for each trial type
            SDacc = sd(Acc))            # calculate SD for Acc
 

# assessing Accuracy for normality after removing outliers (only TT2 and TT3)

# density plot of Accuracy means including only TT2 and TT3 
ComparingMeansDensityAcc <- ComparingMeansAcc %>% 
  ggplot(aes(meanAcc, colour=factor(Trial.Type))) + 
  geom_density(aes(y=..scaled..)) + 
  scale_color_discrete(name='Trial Type',labels=c("normal", "reversed")) +
  labs(x = "Mean accuracy", y = "") +
  theme_classic()
#to save/export the boxplot as a .tiff file
tiff("ComparingMeansDensityAcc.tiff", units="in", width=5, height=5, res=300)
ComparingMeansDensityAcc
dev.off()

# create QQplot of Acc including only TT2 and TT3 to assess normality 
QQ.ComparingMeans.Acc <- ggqqplot(ComparingMeansAcc$meanAcc) +
  labs(x = "Q-Q plot of mean accuracy", y = "")
tiff("QQ.ComparingMeans.Acc.tiff", units="in", width=5, height=5, res=300)
QQ.ComparingMeans.Acc
dev.off()


# conduct shapiro-wilk test of normality on Acc data
Acc.shapiro.test <- shapiro.test(ComparingMeansAcc$meanAcc) # test if data are normally distributed and suitable for parametric testing

# ---------- WILCOXON TEST ---------- #

# means of Accuracy scores are compared using non-parametric test (paired samples Wilcoxon test) because data is not normally distributed 

Wilcox.Acc <- wilcox.test(meanAcc ~ Trial.Type, data = ComparingMeansAcc, paired=TRUE)

# Calculate the standardised z statistic 
Acc.Zstat<-qnorm(Wilcox.Acc$p.value/2)


AccMedian <- PlotPrep.2 %>%              # calculate median 
  filter(Trial.Type != 1) %>%                            
  group_by(Trial.Type) %>% 
  summarise(TTmedian = median(Acc)) 

AccMeans <- PlotPrep.2 %>%              # calculate median 
  filter(Trial.Type != 1) %>%                            
  group_by(Trial.Type) %>% 
  summarise(TTmean = mean(Acc)) 

# marginal significance in difference between accuracy scores (Z = -2.12, p = 0.03) in normal and reversal trials, whereby Accuracy scores were lower in normal trials (M = .93) compared to reveral trials (M = .95).

