#*****************************************************************
#* TITLE:  MUX Manusript Graphical Abstract
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 14 Feb 2023
#*                                    
#* NOTES:  This script utilizes data files from '2_PLSR_MUX_bootstrap.R' 
#* to produce multi-panel plots comparing high- and low-frequency data
#*                                  
#*        
#*****************************************************************


# Load packages
library(lubridate)
library(tidyverse)
library(magrittr)
require(transformr)
library(stringr)
library(scales)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(zoo)


# Set working directory
setwd("./")


#### Read and format MUX PLSR predictions ####
path = "./MagicData/MUX/Figures Files/"
MUX_preds = read.csv(paste0(path,"MUX_predictions_boot_111622.csv"))
MUX_preds$DateTime = mdy_hm(MUX_preds$DateTime, tz="Etc/GMT+4")
# for plotting...
MUX_preds$Depth_m = as.numeric(MUX_preds$Depth_m)

# fill gap with NA's 
times = MUX_preds
times[c(1:nrow(times)),c(1:ncol(times))] = NA_real_
fill = seq.POSIXt(from= as.POSIXct("2021-05-31 12:00:00", tz="Etc/GMT+4"), to= as.POSIXct("2021-06-04 12:00:00", tz="Etc/GMT+4"), by= "51 min")
times = times[-c(length(fill)+1:nrow(MUX_preds)),]
deps = rep_len(c(0.1,1.6,3.8,6.2,8.0,9.0),length.out = length(fill))
times$DateTime = fill 
times$Depth_m = deps

MUX_preds = MUX_preds %>% bind_rows(times) %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>% ungroup(Depth_m)




#### Read and format TO-Deployment FCR WQ data (from PLSR models) ####
dataWQ_TO <- read_csv(paste0(path,"MUX20_dataWQ_111522.csv"))
dataWQ_TO$DateTime = ymd_hms(dataWQ_TO$DateTime, tz="America/New_York")
dataWQ_TO$DateTime = with_tz(dataWQ_TO$DateTime, tz = "Etc/GMT+4")
dataWQ_TO = dataWQ_TO %>% select(-c('...1'))
dataWQ_TO = dataWQ_TO %>% drop_na(TFe_mgL)

#### Read and format OO-Deployment FCR WQ data (from PLSR models) ####
dataWQ_OO <- read_csv(paste0(path,"MUX21_dataWQ_051322.csv"))
dataWQ_OO$DateTime = ymd_hms(dataWQ_OO$DateTime, tz="America/New_York")
dataWQ_OO$DateTime = with_tz(dataWQ_OO$DateTime, tz = "Etc/GMT+4")
dataWQ_OO = dataWQ_OO %>% select(-c('...1','ID'))
dataWQ_OO = dataWQ_OO %>% drop_na(TFe_mgL)


# vector to add turnover line
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00"), tz = "Etc/GMT+4"))
colnames(turnover)= c("Date")

# vector to add SSS line
SSS = as.data.frame(ymd_hm(c("2021-06-11 11:00"), tz = "Etc/GMT+4"))
colnames(SSS)= c("Date")

# Create variable for BeginTime and EndTime
Begin_time_20 = as.POSIXct("2020-10-16 00:00:00", tz = "Etc/GMT+4")
End_time_20 = as.POSIXct("2020-11-09 18:00:00", tz = "Etc/GMT+4")

# Create variable for BeginTime and EndTime
Begin_time_21 = as.POSIXct("2021-05-26 00:00:00", tz = "Etc/GMT+4") # 2021-06-04 starts after data gap
End_time_21 = as.POSIXct("2021-06-21 24:00:00", tz = "Etc/GMT+4")


TFe_weekly_TO = ggplot() +
  #geom_path(data=MUX_preds, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.8) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_path(data=dataWQ_TO, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=0.8) +
   geom_point(data=dataWQ_TO, aes(x=DateTime, y=TFe_mgL,  fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  ylim(c(-1,6)) +
  labs(x="Date",y="Total Fe (mg/L)", color = "Depth (m)") +
  theme_bw() +
  theme(legend.position="none")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time_20,End_time_20),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 10),
    axis.text.y.left = element_text(size= 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5)) 


TFe_MUX_TO = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.5) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ_TO, aes(x=DateTime, y=TFe_mgL,  fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  ylim(c(-1,6)) +
  labs(x="Date",y="Total Fe (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="none")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time_20,End_time_20),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 10),
    axis.text.y.left = element_text(size= 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5)) 


TFe_weekly_OO = ggplot() +
  #geom_path(data=MUX_preds, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.8) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
   geom_path(data=dataWQ_OO, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=0.8) +
   geom_point(data=dataWQ_OO, aes(x=DateTime, y=TFe_mgL,  fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  ylim(c(-2,10)) +
  labs(x="Date",y="Total Fe (mg/L)", color = "Depth (m)") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time_21,End_time_21),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 10),
    axis.text.y.left = element_text(size= 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5)) 


TFe_MUX_OO = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.5) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ_OO, aes(x=DateTime, y=TFe_mgL,  fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  ylim(c(-2,10)) +
  labs(x="Date",y="Total Fe (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time_21,End_time_21),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 10),
    axis.text.y.left = element_text(size= 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5)) 


#### Create png file of multipanel plots ####


jpeg('MUX_GraphicalAbstract_021623.jpeg', width = 160, height = 120, units = 'mm', res = 600)

(TFe_weekly_TO | TFe_weekly_OO) / (TFe_MUX_TO | TFe_MUX_OO) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

dev.off()
