library(tidyverse)
library(gridExtra)
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')
library('ztable')     # format tables for reporting

## Plotting PROPORTION OF RESPONSES (Accurate, Inaccurate, and unexplained) ##
    
  data_prop <- data %>%                     # create new df with data from "data" that includes columns for experimental stages and trial types
      mutate(Stage = if_else(
        epoch <= 8, "1",                    # "1" = Stage 1 (all normal training trials) 
        if_else(epoch > 8, "2", "NA")),     # "2" = Stage 2 (experimental trials, normal and reversal trials)
      Trial.Type = if_else(                  
          epoch <= 8, "1",                  # "1" = training trials in stage 1 (all normal)
        if_else(
          epoch > 8 &
            Norm_Rev == 0, "2",             # "2" = normal trials in stage 2 
        if_else(
          epoch > 8 &
            Norm_Rev == 1, "3", "NA"))))    # "3" = reversal trials in stage 2 
      
  data_prop  <- data_prop %>%
      mutate(epoch = as.ordered(epoch)               # numeric values to ordered factor
             , Acc = as.factor(Acc)                  # numeric to ordered factor (like a grade)
             , Stage = as.ordered(Stage)
             , Trial.Type = as.ordered(Trial.Type)   # numeric to ordered factor
             , firstP3 = as.factor(firstP3))

        
  Prop_df <- data_prop %>%          # create df to calculate proportions of the specified variables
  group_by(Stage, Trial.Type) %>% 
  count(Stage,Trial.Type,Acc) %>%
  mutate(prop = prop.table(n))   
  
  # Prop_df <- Prop_df[order(-Acc),] # trying to reorder levels of Acc to change position of bars but for some reason this produces error msg:Error in exists(Acc) : object 'Acc' not found. 
 
  All.Resp.Prop <- ggplot(Prop_df, aes(Trial.Type, prop, fill = Acc)) +                                                   # define x and y axes, and which proportions should be plotted (fill)
    theme_classic() +                                                                                                     # specify aesthetic theme (no background lines)
      scale_y_continuous(labels = scales::percent) +                                                                      # show proportions as percentages 
        geom_bar(stat = 'identity', position = 'dodge') +                                                                 # plot bar chart with different response categories (inaccurate, accurate, and unexplained plotted next to each other 'dodge')
          labs(x = "Trial Type", y = "Proportion of Responses", fill = "Responses") +                                     # label axes and legend
            scale_fill_discrete(name = "Responses", labels = c("Inaccurate", "Accurate", "Unexplained")) +                # Specify levels of the legend 
              scale_x_discrete(labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal"))   # Rename tick mark labels on x-axis to specify trial type (use \n between words to add line break)
  
  #to save/export the bar chart as a .tiff file 
        
        tiff("All.Resp.Prop.tiff", units="in", width=5, height=5, res=300)                                         
          ggplot(Prop_df, aes(Trial.Type, prop, fill = Acc)) +                                                   
          theme_classic() +                                                                                                     
          scale_y_continuous(labels = scales::percent) +                                                                        
          geom_bar(stat = 'identity', position = 'dodge') +                                                                 
          labs(x = "Trial Type", y = "Proportion of Responses", fill = "Responses") +                                     
          scale_fill_discrete(name = "Responses", labels = c("Inaccurate", "Accurate", "Unexplained")) +                 
          scale_x_discrete(labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal"))    
        dev.off()
   
 # trying to find way change the colour of the bars without changing labels of legend!!!
        

  ## Plotting Proportion of INITIAL FIXATIONS  ##

  Initial.Fix <- data_prop[!is.na(data_prop$firstP3),]
  
  Initial.Fix <- Initial.Fix %>% 
    filter(Acc == 1) %>%
    group_by(Stage, Trial.Type) %>%
    count(Trial.Type,firstP3) %>%
    mutate(prop = prop.table(n)) 

  Initial.Fix.Prop <- ggplot(Initial.Fix, aes(Trial.Type, prop, fill = firstP3, width = 0.75)) +                          # define x and y axes, and which proportions should be plotted (fill)
    theme_classic() +                                                                                                     # specify aesthetic theme (no background lines)
      scale_y_continuous(labels = scales::percent) +                                                                      # show proportions as percentages 
        geom_bar(stat = 'identity', position = 'dodge') +                                                                 # plot bar chart with different response categories (inaccurate, accurate, and unexplained plotted next to each other 'dodge')
          labs(x = "Trial Type", y = "Proportion of initial fixations", fill = "Responses") +                             # label axes and legend
            scale_fill_discrete(name = "Cue Type", labels = c("Non-predictive", "Predictive", "NA")) +                    # Specify levels of the legend 
              scale_x_discrete(labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal"))   # Rename tick mark labels on x-axis to specify trial type (use \n between words to add line break) 
  
  #to save/export the bar chart as a .tiff file
  
  tiff("Initial.Fix.Prop.tiff", units="in", width=5, height=5, res=300)                 
  ggplot(Initial.Fix, aes(Trial.Type, prop, fill = firstP3, width = 0.75)) +                          
    theme_classic() +                                                                                                     
    scale_y_continuous(labels = scales::percent) +                                                                      
    geom_bar(stat = 'identity', position = 'dodge') +                                                                 
    labs(x = "Trial Type", y = "Proportion of initial fixations", fill = "Responses") +                             
    scale_fill_discrete(name = "Cue Type", labels = c("Non-predictive", "Predictive", "NA")) +                     
    scale_x_discrete(labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal"))    
  dev.off()
  
  ## Plotting Proportion of FINAL FIXATIONS  ##
    
  Final.Fix <- data_prop[!is.na(data_prop$lastP3),]
  
  Final.Fix <- Final.Fix %>% 
    filter(Acc == 1) %>%
    group_by(Stage, Trial.Type) %>%
    count(Trial.Type,lastP3) %>%
    mutate(prop = prop.table(n)) 
  
  Final.Fix.Prop <- ggplot(Final.Fix, aes(Trial.Type, prop, fill = lastP3, width = 0.75)) +                               # define x and y axes, and which proportions should be plotted (fill)
    theme_classic() +                                                                                                     # specify aesthetic theme (no background lines)
      scale_y_continuous(labels = scales::percent) +                                                                      # show proportions as percentages 
        geom_bar(stat = 'identity', position = 'dodge') +                                                                 # plot bar chart with different response categories (inaccurate, accurate, and unexplained plotted next to each other 'dodge')
          labs(x = "Trial Type", y = "Proportion of final fixations", fill = "Responses") +                               # label axes and legend
            scale_fill_discrete(name = "Cue Type", labels = c("Non-predictive", "Predictive", "NA")) +                    # Specify levels of the legend 
              scale_x_discrete(labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal"))   # Rename tick mark labels on x-axis to specify trial type (use \n between words to add line break) 

  #to save/export the bar chart as a .tiff file
  
  tiff("Final.Fix.Prop.tiff", units="in", width=5, height=5, res=300) 
  ggplot(Final.Fix, aes(Trial.Type, prop, fill = lastP3, width = 0.75)) +                               
    theme_classic() +                                                                                                     
    scale_y_continuous(labels = scales::percent) +                                                                      
    geom_bar(stat = 'identity', position = 'dodge')  +                                                                 
    labs(x = "Trial Type", y = "Proportion of final fixations", fill = "Responses") +                               
    scale_fill_discrete(name = "Cue Type", labels = c("Non-predictive", "Predictive", "NA")) +                     
    scale_x_discrete(labels=c("1" = "Stage 1\nTraining", "2" = "Stage 2\nNormal", "3" = "Stage 2\nReversal"))   
  dev.off()
  
 
  
  
  # #multiplot
  # # transform graphs into grobs
  # g1 <- ggplotGrob()
  # g2 <- ggplotGrob()
  # g3 <- ggplotGrob()
  # 
  # # draw a 2x2 grid with the grobs
  # grid.draw(rbind(cbind(g1,g2),cbind(g3,g4)))
  # 
  # #export multiplot as high resolution tiff file
  # multiplot <- grid.draw(rbind(cbind(g1,g2),cbind(g3,g4)))
  # tiff("multiplot.tiff", units="in", width=5, height=5, res=300)
  # grid.draw(rbind(cbind(g1,g2),cbind(g3,g4)))
  # dev.off()
  # 
  
  
  
  