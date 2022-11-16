# Make Box & Whisker Plots of Sampling Data used for Calibrating PLSR Models
# Author: Nick Hammond
# Last Edited: 09/21/2022

# Set wd, load packages, source code
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)
library(readxl)
library(pls) 
library(scales)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(zoo)


#data path and working directory locations
pathD<-"C:/Users/hammo/Documents/Magic Sensor PLSR/Raw_predictions/" #EDIT: Specify folder where data is located
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")

#Specify files for WQ data, FP overlaps, and the entire FP time series
WQ_20<-"MUX20_dataWQ_050622.csv"
WQ_21<-"MUX21_dataWQ_051322.csv"

#Select Desired Depths
Depths<-c("0.1",
  "1.6",
  "3.8",
  "5.0",
  "6.2", 
  "8.0", 
  "9.0"
)

#Select Desired Date Range
Begin_20<- c("2020-10-15 12:00")
End_20<- c("2020-11-09 15:00:00")

Begin_21<- c("2021-05-26 00:30:00") # Experiment start time: 5/26/2021 13:58
End_21<- c("2021-06-21 12:00:00") # Experiment end time: 6/21/2021 8:47



#### Read in FCR WQ data ####
dataWQ_20 <- read_csv(paste(pathD,WQ_20,sep=""))
dataWQ_21 <- read_csv(paste(pathD,WQ_21,sep=""))

#Subset to just include desired depths
dataWQ_20 <- dataWQ_20 %>%
  filter(Depth_m %in% as.numeric(Depths))

dataWQ_21 <- dataWQ_21 %>%
  filter(Depth_m %in% as.numeric(Depths)) 
  #filter(Reservoir == "FCR") %>%
  #filter(Site == 50) %>%
  #select(-c("Reservoir","Site","Flag_DateTime","Flag_TFe","Flag_TMn","Flag_SFe","Flag_SMn"))


#Subset to desired date range
dataWQ_20 = dataWQ_20[dataWQ_20$DateTime>Begin_20,]
dataWQ_20 = dataWQ_20[dataWQ_20$DateTime<End_20,]

dataWQ_21 = dataWQ_21[dataWQ_21$DateTime>Begin_21,]
dataWQ_21 = dataWQ_21[dataWQ_21$DateTime<End_21,]

# Convert depth variable to categorical
dataWQ_20$Depth_m = as.character(dataWQ_20$Depth_m)
dataWQ_21$Depth_m = as.character(dataWQ_21$Depth_m)


# Pivot Longer for plotting
dataWQ_20 = dataWQ_20 %>% select(DateTime, Depth_m,  TFe_mgL, TMn_mgL, SFe_mgL,  SMn_mgL) %>% rename("Total Fe" = TFe_mgL,
                                                                                                     "Total Mn" = TMn_mgL,
                                                                                                     "Soluble Fe" = SFe_mgL,
                                                                                                     "Soluble Mn" = SMn_mgL)
dataWQ_21 = dataWQ_21 %>% select(DateTime, Depth_m,  TFe_mgL, TMn_mgL, SFe_mgL,  SMn_mgL) %>% rename("Total Fe" = TFe_mgL,
                                                                                                     "Total Mn" = TMn_mgL,
                                                                                                     "Soluble Fe" = SFe_mgL,
                                                                                                     "Soluble Mn" = SMn_mgL)
dataWQ_20 = dataWQ_20 %>% pivot_longer(cols = c(3:6), names_to = "Variable", values_to = "Concentration")
dataWQ_21 = dataWQ_21 %>% pivot_longer(cols = c(3:6), names_to = "Variable", values_to = "Concentration")


### Make plots ###
box_20 = ggplot() +
  geom_boxplot(data = dataWQ_20, aes(Depth_m, Concentration, colour = Depth_m), size=2) +
  geom_point(data = dataWQ_20, aes(Depth_m, Concentration, colour = Depth_m), size=4) +
  labs(x = "Depth (m)", y = "Concentration (mg/L)") +
  ggtitle("Turnover Deployment") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    title = element_text(size=28),
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34),
    strip.text = element_text(size = 26)) +
  facet_wrap(~Variable,nrow = 2,scales = "free_y")

box_21 = ggplot() +
  geom_boxplot(data = dataWQ_21, aes(Depth_m, Concentration, colour = Depth_m), size=2) +
  geom_point(data = dataWQ_21, aes(Depth_m, Concentration, colour = Depth_m), size=4) +
  labs(x = "Depth (m)", y = "Concentration (mg/L)") +
  ggtitle("Oxygen On Deployment") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    title = element_text(size=28),
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34),
    strip.text = element_text(size = 26)) +
  facet_wrap(~Variable,nrow = 2,scales = "free_y")


png('MUX_WQ_boxplots_092122.png', width = 18, height = 24, units = 'in', res = 300)

box_20 / box_21 

dev.off()








#### Old Code ####

TFe_box = ggplot() +
  geom_boxplot(data = dataWQ_20, aes(Depth_m,TFe_mgL,colour = Depth_m),size=2) +
  geom_point(data = dataWQ_20, aes(Depth_m,TFe_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Total Fe (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34)) 

TMn_box = ggplot() +
  geom_boxplot(data = dataWQ_20, aes(Depth_m,TMn_mgL,colour = Depth_m), size=2) +
  geom_point(data = dataWQ_20, aes(Depth_m,TMn_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Total Mn (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34))

SFe_box = ggplot() +
  geom_boxplot(data = dataWQ_20, aes(Depth_m,SFe_mgL,colour = Depth_m),size=2) +
  geom_point(data = dataWQ_20, aes(Depth_m,SFe_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Soluble Fe (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34))

SMn_box = ggplot() +
  geom_boxplot(data = dataWQ_20, aes(Depth_m,SMn_mgL,colour = Depth_m),size=2) +
  geom_point(data = dataWQ_20, aes(Depth_m,SMn_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Soluble Mn (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34))

png('MUX20_WQ_boxplots_082422.png', width = 28, height = 20, units = 'in', res = 300)

(TFe_box | SFe_box) / (TMn_box | SMn_box) 

dev.off()


### 2021 ###

TFe_box = ggplot() +
  geom_boxplot(data = dataWQ_21, aes(Depth_m,TFe_mgL,colour = Depth_m),size=2) +
  geom_point(data = dataWQ_21, aes(Depth_m,TFe_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Total Fe (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34)) 

TMn_box = ggplot() +
  geom_boxplot(data = dataWQ_21, aes(Depth_m,TMn_mgL,colour = Depth_m), size=2) +
  geom_point(data = dataWQ_21, aes(Depth_m,TMn_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Total Mn (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34)) 

SFe_box = ggplot() +
  geom_boxplot(data = dataWQ_21, aes(Depth_m,SFe_mgL,colour = Depth_m),size=2) +
  geom_point(data = dataWQ_21, aes(Depth_m,SFe_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Soluble Fe (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34)) 

SMn_box = ggplot() +
  geom_boxplot(data = dataWQ_21, aes(Depth_m,SMn_mgL,colour = Depth_m),size=2) +
  geom_point(data = dataWQ_21, aes(Depth_m,SMn_mgL,colour = Depth_m),size=4) +
  labs(x = "Depth (m)", y = "Soluble Mn (mg/L)") +
  theme(legend.position = "none") +
  theme(
    axis.text.x = element_text(size= 30),
    axis.text.y.left = element_text(size= 30),
    axis.title.x = element_text(size=30),
    axis.title.y = element_text(color = "black", size=34)) 


png('MUX21_WQ_boxplots_082422.png', width = 28, height = 20, units = 'in', res = 300)

(TFe_box | SFe_box) / (TMn_box | SMn_box) 

dev.off()
