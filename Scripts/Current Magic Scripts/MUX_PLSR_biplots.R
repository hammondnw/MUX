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

setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")

### Turnover Experiment ###

# Read in MUX PLSR predictions
MUX_preds = read_csv(paste0(getwd(),"/Raw_predictions/MUX20_predictions_boot_050622.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime, tz="America/New_York")
MUX_preds = MUX_preds %>% select(-c('...1')) %>% mutate(ID = "Pred")

#### Read in FCR WQ data ####
dataWQ <- read_csv(paste0(getwd(),"/Raw_predictions/MUX20_dataWQ_050622.csv"))
dataWQ$DateTime = ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ = dataWQ %>% select(-c('...1','ID')) %>% mutate(ID = "Obs")

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
  labs(x='Lab Measured Total Fe (mg/L)', y='PLSR Predicted Total Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 4.75) 
  
TMn = ggplot(biplot_df_hypo, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Total Mn (mg/L)', y='PLSR Predicted Total Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 2.2) 

SFe = ggplot(biplot_df_hypo, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Fe (mg/L)', y='PLSR Predicted Soluble Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.075, digits = 1) 

SMn = ggplot(biplot_df_hypo, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Mn (mg/L)', y='PLSR Predicted Soluble Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 2, digits = 2) 


# Turnover Experiment, Epilimnion Models #

TFe_epi = ggplot(biplot_df_epi, aes(x=TFe_mgL_Obs,y=TFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Total Fe (mg/L)', y='PLSR Predicted Total Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 1.5) 

TMn_epi = ggplot(biplot_df_epi, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Total Mn (mg/L)', y='PLSR Predicted Total Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.75) 

SFe_epi = ggplot(biplot_df_epi, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Fe (mg/L)', y='PLSR Predicted Soluble Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.096, digits = 2) 

SMn_epi = ggplot(biplot_df_epi, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Mn (mg/L)', y='PLSR Predicted Soluble Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.7, digits = 2) 

# Write plots to jpeg

patchwork_epi = (TFe_epi | TMn_epi | SFe_epi | SMn_epi)

jpeg('MUX20_biplots_epi_090622.jpeg', width = 12, height = 3.5, units = 'in', res = 600)

patchwork_epi + plot_annotation(title = "Epilimnion")

dev.off()

patchwork_hypo = (TFe | TMn | SFe | SMn)

jpeg('MUX20_biplots_hypo_090622.jpeg', width = 12, height = 3.5, units = 'in', res = 600)

patchwork_hypo + plot_annotation(title = "Hypolimnion")

dev.off()





### Oxygen ON Experiment ###

# Read in MUX PLSR predictions
MUX_preds = read_csv(paste0(getwd(),"/Raw_predictions/MUX21_predictions_boot_051322.csv"))
MUX_preds$DateTime = ymd_hms(MUX_preds$DateTime, tz="America/New_York")
MUX_preds = MUX_preds %>% select(-c('...1'))

#### Read in FCR WQ data ####
dataWQ <- read_csv(paste0(getwd(),"/Raw_predictions/MUX21_dataWQ_051322.csv"))
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
  labs(x='Lab Measured Total Fe (mg/L)', y='PLSR Predicted Total Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 7) 

TMn = ggplot(biplot_df_hypo, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Total Mn (mg/L)', y='PLSR Predicted Total Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 1) 

SFe = ggplot(biplot_df_hypo, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Fe (mg/L)', y='PLSR Predicted Soluble Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 7.2, digits = 2) 

SMn = ggplot(biplot_df_hypo, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Mn (mg/L)', y='PLSR Predicted Soluble Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.95, digits = 2) 


# Oxygen On Experiment, Epilimnion Models #

TFe_epi = ggplot(biplot_df_epi, aes(x=TFe_mgL_Obs,y=TFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Total Fe (mg/L)', y='PLSR Predicted Total Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.75) 

TMn_epi = ggplot(biplot_df_epi, aes(x=TMn_mgL_Obs,y=TMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Total Mn (mg/L)', y='PLSR Predicted Total Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.055) 

SFe_epi = ggplot(biplot_df_epi, aes(x=SFe_mgL_Obs,y=SFe_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Fe (mg/L)', y='PLSR Predicted Soluble Fe (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.65, digits = 2) 

SMn_epi = ggplot(biplot_df_epi, aes(x=SMn_mgL_Obs,y=SMn_mgL_Pred)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='black') +
  theme_minimal() +
  labs(x='Lab Measured Soluble Mn (mg/L)', y='PLSR Predicted Soluble Mn (mg/L)') +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.y = 0.0165, digits = 2) 

# Write plots to jpeg

patchwork_epi = (TFe_epi | TMn_epi | SFe_epi | SMn_epi)

jpeg('MUX21_biplots_epi_090622.jpeg', width = 12, height = 3.5, units = 'in', res = 600)

patchwork_epi + plot_annotation(title = "Epilimnion")

dev.off()

patchwork_hypo = (TFe | TMn | SFe | SMn)

jpeg('MUX21_biplots_hypo_090622.jpeg', width = 12, height = 3.5, units = 'in', res = 600)

patchwork_hypo + plot_annotation(title = "Hypolimnion")

dev.off()
