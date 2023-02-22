
#*****************************************************************
#* TITLE:  MUx 2020 Results Figures Script
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 16 November 2022
#*                                    
#* NOTES:  This script utilizes data files from '2a_PLSR_MUX_2020.R', 'thermocline_SchmidtStability_calcs_MUX20.R',
#*         and the EDI catwalk and met station data packages to produce multi-panel plots of:
#*         - PLSR predicted Fe and Mn concentrations w/ PI's
#*         - Catwalk Data (temp, DO)
#*         - Schmidt stability
#*         - Metoorological data
#*         Figures 4 & 5 in the manuscript and SI figures XXX
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


#### Read and format catwalk data ####

# Specify directory and file name for data file
path = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/Covariate data/" # Catwalk and Met files are too large for Github, so I'm downloading them to my local directory
df_name = "FCR_Catwalk_2018_2021.csv"
#Download EDI catwalk dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/6/23a191c1870a5b18cbc17f2779f719cf"  
infile1 <- c(paste0(path,df_name,sep=""))
#download.file(inUrl1,infile1,method="curl")
#Import data as .csv file
catwalk = read_csv(paste(path,df_name,sep="")) 

#Select the variables we want
catwalk_exp = catwalk %>% select(Reservoir,Site,DateTime,RDO_mgL_5_adjusted,
                                 RDO_mgL_9_adjusted, EXODO_mgL_1,ThermistorTemp_C_surface,
                                 ThermistorTemp_C_1,ThermistorTemp_C_2, ThermistorTemp_C_3,
                                 ThermistorTemp_C_4, ThermistorTemp_C_8, ThermistorTemp_C_9,
                                 ThermistorTemp_C_5, ThermistorTemp_C_6, ThermistorTemp_C_7,
                                 EXOfDOM_QSU_1, EXOSpCond_uScm_1, EXOChla_ugL_1)
#### Convert DateTime to PosixCT
catwalk_exp$DateTime = ymd_hms(catwalk_exp$DateTime, tz="Etc/GMT+5") # Sensors are on EST (GMT+5) but converting to
catwalk_exp$DateTime = with_tz(catwalk_exp$DateTime, tz="Etc/GMT+4") # EDT (GMT+4) to match MUX data

# Select the reservoir, site, and date range we want
catwalk_exp = catwalk_exp %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")



# convert temp and DO to long format for plotting
therm_depths = data.frame(depth_m = c(0.1,1:9), depth = c("ThermistorTemp_C_surface",
                                                        paste0("ThermistorTemp_C_",rep(1:9))))
DO_depths = data.frame(depth_m = c(1.6,5,9), depth = c("EXODO_mgL_1","RDO_mgL_5_adjusted",
                                                       "RDO_mgL_9_adjusted"))
catwalk_exp_long = catwalk_exp %>% pivot_longer(cols=c(7:16),names_to = "depth", 
                                                values_to = "temperature") %>%
  left_join(therm_depths, by = "depth")

DO_long = catwalk_exp %>% 
  select(Reservoir, Site, DateTime, EXODO_mgL_1, RDO_mgL_5_adjusted,
         RDO_mgL_9_adjusted) %>%
    pivot_longer(cols=c(4:6),names_to = "depth", values_to = "DO_mgL") %>%
  left_join(DO_depths, by = "depth")

# filter temp by depth
catwalk_exp_long = catwalk_exp_long %>% filter(depth_m %in% c(0.1,1,2,3,4,5,6,7,8,9))



#### Read and format met data ####
# Specify directory and file name for data file
df_name = "FCR_Met_final_2015_2021.csv"
#Download EDI met dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/6/a5524c686e2154ec0fd0459d46a7d1eb"  
infile1 <- c(paste0(path,df_name,sep=""))
#download.file(inUrl1,infile1,method="curl") 
#Import data as .csv file
met = read_csv(paste(path,df_name,sep=""))
# Select the variables we want
met_exp = met %>% select(Reservoir,Site,DateTime,WindSpeed_Average_m_s,ShortwaveRadiationDown_Average_W_m2,
                         Rain_Total_mm, AirTemp_Average_C)
#Convert DateTime to PosixCT
# Sensors are on EST (GMT+5) but converting to EDT (GMT+4) to match MUX data
met_exp$DateTime = ymd_hms(met_exp$DateTime, tz="Etc/GMT+5") 
met_exp$DateTime = with_tz(met_exp$DateTime, tz="Etc/GMT+4")
#Select the reservoir, site, and date range we want
met_exp = met_exp %>% filter(Reservoir=="FCR" & Site==50) %>%
    filter(DateTime>"2020-10-16 13:10:00") %>%
    filter(DateTime<"2020-11-09 14:00:00")




#### Read and format MUX PLSR predictions ####
path = "./MagicData/MUX/Figures Files/"
MUX_preds = read.csv(paste0(path,"MUX_predictions_boot_111622.csv"))
MUX_preds$DateTime = mdy_hm(MUX_preds$DateTime, tz="Etc/GMT+4")
# Select date range for MUX predictions
MUX_preds = MUX_preds %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-09 14:00:00")
# for plotting...
MUX_preds$Depth_m = as.numeric(MUX_preds$Depth_m)


#### Read and format FCR WQ data (from PLSR models) ####
dataWQ <- read_csv(paste0(path,"MUX20_dataWQ_111522.csv"))
dataWQ$DateTime = ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ$DateTime = with_tz(dataWQ$DateTime, tz = "Etc/GMT+4")
dataWQ = dataWQ %>% select(-c('...1'))


#### Read in Thermocline depth data ####
#ThermoDepths = read_csv(paste0(getwd(),"/Data/Covariate data/FCR_ThermoDepth_MUX20_013122.csv"))
# Convert DateTime to PosixCT
#ThermoDepths$datetime = ymd_hms(ThermoDepths$datetime,tz="America/New_York")
# Smooth time series using moving average
#ThermoDepths = ThermoDepths %>% 
#  mutate(thermo.depth_roll = rollmean(thermo.depth,k=9,fill = NA))


#### Read in schmidt stability data ####
Schmidt = read_csv(paste0(path,"FCR_SchmidtStability_MUX20_082322.csv"))
#Convert DateTime to PosixCT
Schmidt$datetime = ymd_hms(Schmidt$datetime,tz="Etc/GMT+5")
Schmidt$datetime = with_tz(Schmidt$datetime, tz="Etc/GMT+4")

# vector to add turnover line
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00"), tz = "Etc/GMT+4"))
colnames(turnover)= c("Date")


rectangle_TMn <- data.frame(xmin = as.POSIXct(c("2020-10-26 10:40:00"),tz = "Etc/GMT+4"),
                           xmax = as.POSIXct(c("2020-10-28 09:17:00"), tz = "Etc/GMT+4"),
                           ymin = -Inf, ymax = Inf)

#### Multipanel Plots ####

# Create variable for BeginTime and EndTime
Begin_time = as.POSIXct("2020-10-16 00:00:00", tz = "Etc/GMT+4")
End_time = as.POSIXct("2020-11-09 18:00:00", tz = "Etc/GMT+4")

# MUX Predictions

TFe_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.8) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  labs(x="Date",y="Total Fe (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
    theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5))  

TMn_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=TMn_mgL, color= as.character(Depth_m)), size=0.8) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerTMn_min, ymax=uncerTMn_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TMn_mgL, fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  labs(x="Date",y="Total Mn (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  guides(fill= "none")+
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.spacing.x = unit(5,"mm"),
    #legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5)) +
    geom_rect(data = rectangle_TMn, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "gray", alpha = 0.5)
  
SFe_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=SFe_mgL, color= as.character(Depth_m)), size=0.8) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerSFe_min, ymax=uncerSFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=SFe_mgL, fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  labs(x="Date",y="Soluble Fe (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5))  

SMn_plot = ggplot() +
  geom_path(data=MUX_preds, aes(x=DateTime,y=SMn_mgL, color= as.character(Depth_m)), size=0.8) +
  geom_ribbon(data=MUX_preds, aes(ymin=uncerSMn_min, ymax=uncerSMn_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=SMn_mgL, fill = as.character(Depth_m)), size=1.5, shape = 21, color = "black") +
  labs(x="Date",y="Soluble Mn (mg/L)", color = "Depth (m)", fill="90% PI") +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #legend.box = "horizontal",
    #legend.spacing.x = unit(5,"mm"),
    panel.grid = element_line(color = "lightgrey", size = 0.5))  


# Thermocline depth & Schmidt stability
thermocline_plot = ggplot() +
  geom_path(data=ThermoDepths, aes(x=datetime, y=thermo.depth_roll), size=2, color = "black") +
  labs(x="Date", y="Thermocline Depth (m)")+
  #theme_ipsum() +
  #theme(legend.position=c(0.95,0.95))+
  scale_x_datetime(date_minor_breaks = "1 day", limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 3),
    axis.text.y.left = element_text(size= 3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=4),
    legend.text = element_text(size = 2),
    legend.title = element_text(size = 2),
    legend.key = element_rect(size=3),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black", size = 0.5))

schmidt_plot = ggplot() +
  geom_path(data=Schmidt, aes(x=datetime, y=schmidt.stability), size=0.8, color = "black") +
  labs(x="Date", y= expression("Schmidt Stability (J/"*"m"^2*")"))+
  theme_bw() +
  #theme(legend.position=c(0.95,0.95))+
  scale_x_datetime(date_minor_breaks = "1 day", limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5)) 


# Catwalk sensor data
DO_plot = ggplot() +
  geom_path(data=DO_long, aes(x=DateTime, y=DO_mgL, color = as.character(depth_m)), size=0.8) +
  labs(x="Date",y="DO (mg/L)",color="Depth (m)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  scale_colour_manual(values = c("#CD9600","#00BFC4","#FF61CC")) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5)) 

Cond_plot = ggplot() +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=EXOSpCond_uScm_1), size=1, color="blue") +
  xlab("Date") +
  ylab("Specific Cond. (uS/cm)") +
  ylim(c(35,50)) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 3),
    axis.text.y.left = element_text(size= 3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=4),
    legend.text = element_text(size = 2),
    legend.title = element_text(size = 2),
    legend.key = element_rect(size=3),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black", size = 0.5))

fdom_plot = ggplot() +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=EXOfDOM_QSU_1), size=1, color="brown") +
  xlab("Date") +
  ylab("fDOM (QSU)") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  ylim(c(18,24)) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  theme(
    axis.text.x = element_text(size= 3),
    axis.text.y.left = element_text(size= 3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=4),
    legend.text = element_text(size = 2),
    legend.title = element_text(size = 2),
    legend.key = element_rect(size=3),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black", size = 0.5))

Temp_plot = ggplot() +
  geom_path(data=catwalk_exp_long, aes(x=DateTime, y=temperature, color = as.factor(depth_m)), size=0.6) +
  labs(x="Date", y="Temp. (deg C)", color= "Depth (m)")+
  theme_bw() +
  #theme(legend.position=c(0.95,0.95))+
  scale_x_datetime(date_minor_breaks = "1 day", limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  guides(col = guide_legend(ncol = 2)) +
  #guides(col = guide_legend(byrow = T)) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    #legend.key = element_rect(size=1.8),
    legend.box.background = element_rect(),
    #legend.spacing.y = unit(0.001,"lines"),
    #legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5))
    



# Met data
SW_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=ShortwaveRadiationDown_Average_W_m2), size=1.5) +
  labs(x="Date",y="Shortwave Rad. Down (W/m2)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  theme(
    axis.text.x = element_text(size= 3),
    axis.text.y.left = element_text(size= 3),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=4),
    legend.text = element_text(size = 2),
    legend.title = element_text(size = 2),
    legend.key = element_rect(size=3),
    legend.box.background = element_rect(),
    panel.grid = element_line(color = "black", size = 0.5))

wind_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=WindSpeed_Average_m_s), size=1, color="black") +
  labs(x="Date",y="Avg Wind Speed (m/s)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    #legend.key = element_rect(size=1.8),
    legend.box.background = element_rect(),
    #legend.spacing.y = unit(0.001,"lines"),
    #legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5))

rain_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=Rain_Total_mm), size=1, color="black") +
  labs(x="Date",y="Total Rain (mm)") +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    #legend.key = element_rect(size=1.8),
    legend.box.background = element_rect(),
    #legend.spacing.y = unit(0.001,"lines"),
    #legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5))

AirTemp_plot = ggplot() +
  geom_path(data=met_exp, aes(x=DateTime, y=AirTemp_Average_C), size=1, color = "black") +
  labs(x="Date", y="Air Temp. (deg C)")+
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%b-%d")) +
  theme_bw() +
  theme(legend.position="right")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=1) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    #legend.key = element_rect(size=1.8),
    legend.box.background = element_rect(),
    #legend.spacing.y = unit(0.001,"lines"),
    #legend.box = "horizontal",
    panel.grid = element_line(color = "lightgrey", size = 0.5))

#### Create png file of multipanel plots ####

# Figure 4
jpeg('MUX20_schmidt_Temp_DO_TFe_TMn_FullDepths_FullTS_shaded_022223.jpeg', width = 210, height = 240, units = 'mm', res = 600)

schmidt_plot / Temp_plot / DO_plot / TFe_plot / TMn_plot + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0))
 
dev.off()


# Figure SI_MUX20_SFe_SMn_predictions
jpeg('MUX20_Schmidt_Temp_DO_SFe_SMn_FullDepths_FullTS_021623.jpeg', width = 190, height = 240, units = 'mm', res = 600)

schmidt_plot / Temp_plot / DO_plot / SFe_plot / SMn_plot + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

dev.off()

# Figure SI_MUX20_met_data
jpeg('MUX20_Temp_AirTemp_Wind_rain_FullDepths_FullTS_120322.jpeg', width = 210, height = 240, units = 'mm', res = 600)

 Temp_plot / AirTemp_plot / wind_plot / rain_plot  + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0))

dev.off()



#### To plot soluble:totals ratios ####

# Convert negative values to zero
MUX_preds = MUX_preds %>% mutate(TFe_mgL = if_else(TFe_mgL > 0, TFe_mgL, 0),
                                 TMn_mgL = if_else(TMn_mgL > 0, TMn_mgL, 0),
                                 SFe_mgL = if_else(SFe_mgL > 0, SFe_mgL, 0),
                                 SMn_mgL = if_else(SMn_mgL > 0, SMn_mgL, 0))
# Calculate soluble:total ratios
MUX_preds = MUX_preds %>% mutate(Fe_ratio = SFe_mgL / TFe_mgL,
                                 Mn_ratio = SMn_mgL / TMn_mgL,
                                 Fe_ratio = if_else(Fe_ratio > 1, 1, Fe_ratio), # If ratios > 1, set to 1
                                 Mn_ratio = if_else(Mn_ratio > 1, 1, Mn_ratio)) %>%
  group_by(Depth_m) %>%                                    # rolling average
  mutate(Fe_ratio_ma10 = rollmean(Fe_ratio,k=10,fill = NA),
         Mn_ratio_ma10 = rollmean(Mn_ratio,k=10,fill = NA)) %>%
  ungroup(Depth_m)

# Split MUX_preds by depth (for plotting)
MUX_preds_hypo_20 = MUX_preds %>% filter(Depth_m > 3.8)
MUX_preds_epi_20 = MUX_preds %>% filter(Depth_m <= 3.8)

dataWQ_hypo_20 = dataWQ %>% filter(Depth_m > 3.8)
dataWQ_epi_20 = dataWQ %>% filter(Depth_m <= 3.8)





#### code for plotting Sol:Tot Fe and Mn on same plot (at all three hypo depths) ####
#mux_preds_ratios_20 = MUX_preds_hypo %>% rename(Fe = Fe_ratio_ma10, Mn = Mn_ratio_ma10) %>% 
#  pivot_longer(cols = c(18:19),names_to = "variable", values_to = "ratio") # %>% 
  #filter(Depth ==9)


Fe_ratio_plot_20 = ggplot() +
  geom_path(data=MUX_preds_hypo_20, aes(x=DateTime,y=Fe_ratio_ma10, color= as.factor(Depth_m)), size=0.8) +
  labs(x="Date",y="Soluble:Total Fe", title = "Turnover Deployment", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  theme_bw() +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  scale_colour_manual(values = c("#00C19F","#619CFF","#FF61C3")) +
  #facet_wrap(~variable, nrow = 2) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    #legend.position = "none",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    title = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
    #strip.text = element_text(size=15)
  )

Mn_ratio_plot_20 = ggplot() +
  geom_path(data=MUX_preds_hypo_20, aes(x=DateTime,y=Mn_ratio_ma10, color= as.factor(Depth_m)), size=0.8) +
  labs(x="Date",y="Soluble:Total Mn", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  theme_bw() +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  scale_colour_manual(values = c("#00C19F","#619CFF","#FF61C3")) +
  #facet_wrap(~variable, nrow = 2) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    #legend.position = "none",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    #title = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
    #strip.text = element_text(size=15)
  )

Fe_ratio_plot_21 = ggplot() +
  geom_path(data=MUX_preds_hypo_21, aes(x=DateTime,y=Fe_ratio_ma10, color= as.factor(Depth_m)), size=0.8) +
  labs(x="Date",y="Soluble:Total Fe", title = "Oxygen On Deployment", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  theme_bw() +
  geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time_21,End_time_21),
                   labels = date_format("%Y-%m-%d")) +
  scale_colour_manual(values = c("#00C19F","#619CFF","#FF61C3")) +
 # facet_wrap(~variable, nrow = 2) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    #legend.position = "none",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
    title = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
    #strip.text = element_text(size=15)
  )

Mn_ratio_plot_21 = ggplot() +
  geom_path(data=MUX_preds_hypo_21, aes(x=DateTime,y=Mn_ratio_ma10, color= as.factor(Depth_m)), size=0.8) +
  labs(x="Date",y="Soluble:Total Mn", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  theme_bw() +
  geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=0.8) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time_21,End_time_21),
                   labels = date_format("%Y-%m-%d")) +
  scale_colour_manual(values = c("#00C19F","#619CFF","#FF61C3")) +
  #facet_wrap(~variable, nrow = 2) +
  theme(
    axis.text.x = element_text(size= 12),
    axis.text.y.left = element_text(size= 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=12),
    #legend.position = "none",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.key = element_rect(size=2),
    legend.box.background = element_rect(),
   # title = element_text(size = 12),
    panel.grid = element_line(color = "lightgrey", size = 0.5)
    #strip.text = element_text(size=15)
  )

png('MUX_TFe_TMn_Ratios_hypo_FullTS_022122.png', width = 190, height = 200, units = 'mm', res = 600)


Fe_ratio_plot_20 / Mn_ratio_plot_20 / Fe_ratio_plot_21 / Mn_ratio_plot_21 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 12, hjust = 0, vjust = 0)) 
  


dev.off()







#### Old Code ####

Fe_ratio_plot = ggplot() +
  geom_point(data=MUX_preds_hypo, aes(x=DateTime,y=Fe_ratio_ma10, color= as.character(Depth_m)), size=3) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=3.5) +
  labs(x="Date",y="Fe Soluble:Total", color = "Depth (m)") +
  ylim(0,1) +
  theme(legend.position="right")+
  #ggtitle("Predicted Total Fe and Sensor Data at 1.6m") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect()
  )


Mn_ratio_plot = ggplot() +
  geom_point(data=MUX_preds_hypo, aes(x=DateTime,y=Mn_ratio_ma10, color= as.character(Depth_m)), size=3) +
  #geom_ribbon(data=MUX_preds, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m)), size=3.5) +
  labs(x="Date",y="Mn Soluble:Total", color = "Depth (m)") +
  #ylim(0,1) +
  theme(legend.position="right")+
  #ggtitle("Predicted Total Fe and Sensor Data at 1.6m") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=2) +
  #theme_ipsum() +
  scale_x_datetime(date_minor_breaks = "1 day", 
                   limits = c(Begin_time,End_time),
                   labels = date_format("%Y-%m-%d")) +
  theme(
    axis.text.x = element_text(size= 36),
    axis.text.y.left = element_text(size= 36),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size=37),
    legend.text = element_text(size = 32),
    legend.title = element_text(size = 34),
    legend.box.background = element_rect()
  )




# Merge MUX metals predictions dataframe with catwalk dataframe
#combined = merge(MUX_preds,catwalk_exp,by="DateTime",all = TRUE)

surface = MUX_preds %>% filter(Depth == 0.1)
one_m = MUX_preds %>% filter(Depth == 1.6)
three_m = MUX_preds %>% filter(Depth == 3.8)
five_m = MUX_preds %>% filter(Depth == 5.0)
six_m = MUX_preds %>% filter(Depth == 6.2)
eight_m = MUX_preds %>% filter(Depth == 8.0)
nine_m = MUX_preds %>% filter(Depth == 9.0)


# Value used to transform the data
coeff_r <- (4)

# A few constants
FeColor <- "#69b3a2"
DOColor <- rgb(0.2, 0.6, 0.9, 1)
TempColor <- rgb(0.2, 0.6, 0.9, 1)

# DO vs. Fe Plot
png('MUX_Fe_DO_post_turnover.png', width = 15, height = 12, units = 'in', res = 300)
DO_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgL), size=2, color=FeColor) + 
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted/coeff_r), size=2, color=DOColor) +
  xlab("Date") +
  scale_y_continuous(
    # Features of the first axis
    name = "TFe_mgL",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="DO (mg/L)")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = FeColor, size=25),
    axis.title.y.right = element_text(color = DOColor, size=25)
  ) 
#ggtitle(" ISCO Discharge and Fe Loads 2018")
DO_plot
dev.off()


# Temp vs. Fe Plot
png('MUX_Fe_Temp_post_turnover.png', width = 15, height = 12, units = 'in', res = 300)
Temp_plot = ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgL), size=2, color=FeColor) + 
  geom_path(data=catwalk_exp, aes(x=DateTime, y=ThermistorTemp_C_1 /coeff_r), size=2, color=TempColor) +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=ThermistorTemp_C_9 /coeff_r), size=2, color=TempColor) +
  xlab("Date") +
  scale_y_continuous(
    # Features of the first axis
    name = "TFe_mgL",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff_r, name="Temp (deg C)")
  ) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_text(size= 22),
    axis.text.y.left = element_text(size= 22),
    axis.text.y.right = element_text(size= 22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = FeColor, size=25),
    axis.title.y.right = element_text(color = TempColor, size=25)
  ) +
ggtitle("MUX Predicted TFe at 1.6m, Temperature at 1m")
Temp_plot
dev.off()




# alternate code
DO_plot <- ggplot() +
  geom_path(data=nine_m, aes(x=DateTime,y=TFe_mgL)) +
  geom_path(data=catwalk_exp, aes(x=DateTime, y=RDO_mgL_9_adjusted), colour="blue") +
  #ylim(0, 0.5)+
  labs(x="Date", y = "TFe_mgL", title = "Predicted TFe") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")
DO_plot


turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")

png("One_m_only_033021.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=MUX_one, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth)), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  ylim(0,8)+
  scale_color_manual(values=c("#D39200"))+
  labs(x="Date", y = "Total Fe (mg/L)", title = "Total Iron Pre- and Post-Turnover 2020 (High-Frequency Observations)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)") +
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()


MUX_one = MUX_preds %>% filter(Depth == 1.6)



#MUX_preds = MUX_preds %>%
#  group_by(Depth) %>%
#  mutate(TFe_ma10 = rollmean(TFe_mgL,k=10,fill = NA)) %>%
#  ungroup(Depth)

# Split MUX_preds by depth (for plotting)
#MUX_preds_hypo = MUX_preds %>% filter(Depth > 3.8)
#MUX_preds_epi = MUX_preds %>% filter(Depth <= 3.8)



#D39200 = 1.6m
#619CFF = 8m
#FF61C3 = 9m
#00C19F = 6.2m


