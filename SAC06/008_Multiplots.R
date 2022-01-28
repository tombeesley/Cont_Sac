library(ggplot2)
library(patchwork)

#---------------RT & ACCURACY---------------#

#### EXPLORING DATA ###

# multiplot of normality tests for RT

tiff("RTnormalityMultiplot.ANN.tiff", units="in", width=6, height=5, res=300)
patchwork <- (DensityRT + ggtitle('RT Normality Tests (incl outliers)') + 
                theme(plot.title = element_text(hjust = 0.5))) / 
                  (TTboxplot.zRT | QQplot.RT) 
  patchwork + plot_annotation(tag_levels = 'A')                                   # add annotation to plots (e.g. plot A, B, and C)
dev.off()


# multiplot of normality tests for Accuracy

tiff("ACCnormalityMultiplot.ANN.tiff", units="in", width=6, height=5, res=300)
patchwork <- (DensityAcc + ggtitle('Accuracy Normality Tests (incl outliers)') + 
                theme(plot.title = element_text(hjust = 0.5))) / 
  (TTboxplot.Acc | QQplot.Acc) 
patchwork + plot_annotation(tag_levels = 'A')                                   # add annotation to plots (e.g. plot A, B, and C)
dev.off()


# multiplot of RT & ACC normality tests

# this works but unnested annotations also very busy! (too many plots)
tiff("NormalityMultiplot_RT_Acc.ANN.tiff", units="in", width=6, height=8, res=300)
patchwork <- DensityAcc / 
  (TTboxplot.Acc | QQplot.Acc) / 
  DensityRT / 
  (TTboxplot.zRT | QQplot.RT) 
patchwork + plot_annotation(tag_levels = c('A', '1'))                                 # add annotation to plots (can't make the A1,A2, B1, etc work)
dev.off()


## TEST ##
# nested annotations only seem to work with 3 plots but not more??
tiff("Test.tiff", units="in", width=7, height=5, res=300)
patchwork <- ((TTboxplot.Acc | QQplot.Acc) + plot_layout(tag_level = 'new')) /
  DensityAcc
patchwork + plot_annotation(tag_levels = c('A', '1'))
dev.off()


### COMPARING MEANS ###

## Normality tests on trial types 2 & 3 only

# RT Normality tests (Density & Q-Q plot)

tiff("NormalityMultiplot_RT.NO.tiff", units="in", width=4, height=5, res=300)
patchwork <- (ComparingMeansDensityRT / (QQ.ComparingMeans.RT))
patchwork + plot_annotation(tag_levels = 'A', 
                            title = "Normality tests on RT scores\n  (normal and reversal trials)",
                            theme = theme(plot.title = element_text(hjust = 0.5)))                                   # add annotation to plots (e.g. plot A, B, and C)
dev.off()

# Accuracy Normality tests (Density & Q-Q plot)

tiff("NormalityMultiplot_ACC.NO.tiff", units="in", width=4, height=5, res=300)
patchwork <- (ComparingMeansDensityAcc / (QQ.ComparingMeans.Acc))
patchwork + plot_annotation(tag_levels = 'A',
                            title = "Normality tests on accuracy scores\n  (normal and reversal trials)",
                            theme = theme(plot.title = element_text(hjust = 0.5)))                                                         # add annotation to plots (e.g. plot A, B, and C)
dev.off()


# Multiplot of RT and Accuracy Normality tests (Density & Q-Q plot)
tiff("NormalityMultiplot.NO.tiff", units="in", width=8, height=6, res=300)
((ComparingMeansDensityAcc | QQ.ComparingMeans.Acc) + plot_layout(tag_level = 'new')) /
  ((ComparingMeansDensityRT | QQ.ComparingMeans.RT) + plot_layout(tag_level = 'new')) +
  plot_annotation(tag_levels = c('A', '1'))   
dev.off()

##################################################################################

# # multiplot of F1 normality tests (after removing outliers)
# tiff("ACCnormalityMultiplot.ANN.tiff", units="in", width=6, height=5, res=300)
# patchwork <- (DensityF1 + ggtitle('Normality Tests of erroneous initial fixations') + 
#                 theme(plot.title = element_text(hjust = 0.5))) / 
#   ( | QQplot.Acc) 
# patchwork + plot_annotation(tag_levels = 'A')                                   # add annotation to plots (e.g. plot A, B, and C)
# dev.off()
# 
# 


##################################################################################




# MORE PATCHWORK TRICKS:

# if you want to add a blank space in multiplot
 + plot_spacer()  

# add annotation to plots (e.g. plot A, B, and C)
patchwork <- (p4 | p2) /
  p1
patchwork + plot_annotation(tag_levels = 'A')


# add annotation for nested plots (e.g. A1, A2, A3, and B1, B2, B3)
patchwork <- ((p4 | p2) + plot_layout(tag_level = 'new')) /
  p1
patchwork + plot_annotation(tag_levels = c('A', '1'))


# specify theme for all plots in multiplot at once
patchwork & theme_classic()

# specify layout of multiplot 
tiff("Multiplot.tiff", units="in", width=6, height=5, res=300)
layout <- '                                                                  
AA
BC
'
A + ggtitle('PLOT TITLE') +
  theme(plot.title = element_text(hjust = 0.5)) + B + C + plot_layout(design = layout) 
dev.off()


