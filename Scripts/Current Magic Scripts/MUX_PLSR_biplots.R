#### Script for making bi-plots for PLSR calibration data ####
### Author: Nick Hammond
### Last Edited: 09/06/2022

# Set wd, load packages
library(lubridate)
library(tidyverse)
library(magrittr)
require(transformr)
library(stringr)
library(readxl)
library(pls) 
library(scales)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(zoo)

setwd("./")

### Turnover Experiment ###

# Read in MUX PLSR predictions
MUX_preds = read_csv(paste0(getwd(),"/MagicData/MUX/Figures Files/MUX20_predictions_boot_111522.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime, tz="America/New_York")
MUX_preds = MUX_preds %>% select(-c('...1')) %>% mutate(ID = "Pred")

#### Read in FCR WQ data ####
dataWQ <- read_csv(paste0(getwd(),"/MagicData/MUX/Figures Files/MUX20_dataWQ_111522.csv"))
dataWQ$DateTime = ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ = dataWQ %>% select(-c('...1')) %>% mutate(ID = "Obs")

biplot_df = MUX_preds %>% filter(DateTime %in% dataWQ$DateTime) %>% 
  select(-c("uncerSMn_max", "uncerSMn_min", "uncerSFe_max", "uncerSFe_min",
            "uncerTMn_max", "uncerTMn_min", "uncerTFe_max", "uncerTFe_min"))

biplot_df = union(biplot_df,dataWQ)
biplot_df = biplot_df %>% pivot_wider(names_from = "ID", values_from = c("TFe_mgL","TMn_mgL",
                                                                         "SFe_mgL","SMn_mgL"))                    
# Subset to just the hypolimnion/epilimnion
biplot_df_hypo = biplot_df %>% filter(Depth_m %in% c(6.2,8,9))
biplot_df_epi = biplot_df %>% filter(Depth_m %in% c(0.1,1.6,3.8))


# Make Plots ! # 



# Turnover Experiment, Hypolimnion Models #

TFe = ggplot(biplot_df_hypo, aes(x=TFe_mgL_Obs,y=TFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Fe (mg/L)', y='PLSR Predicted Tot. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 4.75)  +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))
  
TMn = ggplot(biplot_df_hypo, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Mn (mg/L)', y='PLSR Predicted Tot. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 2.2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SFe = ggplot(biplot_df_hypo, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Fe (mg/L)', y='PLSR Predicted Sol. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.075, digits = 1)  +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SMn = ggplot(biplot_df_hypo, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Mn (mg/L)', y='PLSR Predicted Sol. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 2, digits = 2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))


# Turnover Experiment, Epilimnion Models #

TFe_epi = ggplot(biplot_df_epi, aes(x=TFe_mgL_Obs,y=TFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Fe (mg/L)', y='PLSR Predicted Tot. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 1.5) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

TMn_epi = ggplot(biplot_df_epi, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Mn (mg/L)', y='PLSR Predicted Tot. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.75) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SFe_epi = ggplot(biplot_df_epi, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Fe (mg/L)', y='PLSR Predicted Sol. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.096, digits = 2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SMn_epi = ggplot(biplot_df_epi, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Mn (mg/L)', y='PLSR Predicted Sol. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.7, digits = 2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

# Write plots to jpeg

patchwork_20 = (TFe_epi | TMn_epi | SFe_epi | SMn_epi) / (TFe | TMn | SFe | SMn)

jpeg('MUX20_biplots_111822.jpeg', width = 190, height = 120, units = 'mm', res = 600)

patchwork_20 + plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0)) 

dev.off()





### Oxygen ON Experiment ###

# Read in MUX PLSR predictions
MUX_preds = read_csv(paste0(getwd(),"/MagicData/MUX/Figures Files/MUX21_predictions_boot_051322.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime, tz="America/New_York")
MUX_preds = MUX_preds %>% select(-c('...1'))

#### Read in FCR WQ data ####
dataWQ <- read_csv(paste0(getwd(),"/MagicData/MUX/Figures Files/MUX21_dataWQ_051322.csv"))
dataWQ$DateTime = ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ = dataWQ %>% select(-c('...1','ID'))

MUX_preds = MUX_preds %>% select(-c("uncerSMn_max", "uncerSMn_min", "uncerSFe_max", "uncerSFe_min",
            "uncerTMn_max", "uncerTMn_min", "uncerTFe_max", "uncerTFe_min"))
biplot_df = left_join(dataWQ,MUX_preds,by = c("DateTime","Depth_m"))

colnames(biplot_df) = c("DateTime","Depth_m","TFe_mgL_Obs","TMn_mgL_Obs",
                       "SFe_mgL_Obs","SMn_mgL_Obs", "TFe_mgL_Pred","TMn_mgL_Pred",
                       "SFe_mgL_Pred","SMn_mgL_Pred")
                   
# Subset to just the hypolimnion/epilimnion
biplot_df_hypo = biplot_df %>% filter(Depth_m %in% c(6.2,8,9))
biplot_df_epi = biplot_df %>% filter(Depth_m %in% c(0.1,1.6,3.8))


# Make Plots ! # 



# Oxygen On Experiment, Hypolimnion Models #

TFe = ggplot(biplot_df_hypo, aes(x=TFe_mgL_Obs,y=TFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Fe (mg/L)', y='PLSR Predicted Tot. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 7) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

TMn = ggplot(biplot_df_hypo, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Mn (mg/L)', y='PLSR Predicted Tot. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 1) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SFe = ggplot(biplot_df_hypo, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Fe (mg/L)', y='PLSR Predicted Sol. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 7.2, digits = 2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SMn = ggplot(biplot_df_hypo, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Mn (mg/L)', y='PLSR Predicted Sol. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.95, digits = 2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))


# Oxygen On Experiment, Epilimnion Models #

TFe_epi = ggplot(biplot_df_epi, aes(x=TFe_mgL_Obs,y=TFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Fe (mg/L)', y='PLSR Predicted Tot. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.75) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

TMn_epi = ggplot(biplot_df_epi, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Tot. Mn (mg/L)', y='PLSR Predicted Tot. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.055) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8))

SFe_epi = ggplot(biplot_df_epi, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Fe (mg/L)', y='PLSR Predicted Sol. Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.65, digits = 2)+
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8)) 

SMn_epi = ggplot(biplot_df_epi, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Sol. Mn (mg/L)', y='PLSR Predicted Sol. Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.0165, digits = 2) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(color = "black", size=8),
        axis.text =  element_text(size = 6))

# Write plots to jpeg

patchwork_21 = (TFe_epi | TMn_epi | SFe_epi | SMn_epi) / (TFe | TMn | SFe | SMn)

jpeg('MUX21_biplots_111822.jpeg', width = 190, height = 120, units = 'mm', res = 600)

patchwork_21 + plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0)) 

dev.off()


