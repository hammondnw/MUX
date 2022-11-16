# PLSR Script for 2021 MUX Predictions of Fe and Mn
# Authors: Nick Hammond
# Last Updated: 07/02/2021



#### Set up your packages, data location, and working directory. Load functions. ####

#packages needed
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

#data path and working directory locations
pathD<-"C:/Users/hammo/Documents/Magic Sensor PLSR/Data/" #EDIT: Specify folder where data is located
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")

#load functions
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_function.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_data_prep_function_2021.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_num_components_function.R')


#### Specify input files, depths, date ranges, and parameters ####

#Specify files for WQ data, FP overlaps, and the entire FP time series
WQ_name<-"Metals_2021.xlsx"
FPcaldata_name<-"MUX_FP_Overlaps_2021.csv"
TimeSeriesFP_name<-"MUX_FP_TS_2021.csv"

#Select Desired Depths
Depths<-c("0.1",
          "1.6",
          "3.8",
          "5.0"
          #"6.2", 
          #"8.0",
          #"9.0"
)

#Select Desired Date Range
Begin_time<- c("2021-05-26 00:30:00") # Experiment start time: 5/26/2021 13:58

End_time<- c("2021-06-21 12:00:00")   # Experiment end time: 6/21/2021 8:47
                                      #make sure this is after the last FP *and* sampling times!

#Select WQ parameter (e.g. "TFe")
WQparam <- c("TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL") 



#### Run function to clean/prep data for PLSR ####
data_prep(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam)
# May want to add code to check that this worked properly! 


# Remove outliers #  (don't run this code every time!!!!)
# Remove last sample data from 9m at 2021-06-07 14:49:59 because it is an outlier
# Doing this manually for now #
# 

dataWQ= dataWQ[-c(35),]
dataCalFP = dataCalFP[-c(35),]

#Remove the really low 9m sample from 2021-06-04 16:27:59

dataWQ= dataWQ[-c(4),]
dataCalFP = dataCalFP[-c(4),]

#Remove outlier 9m sample from 2021-05-26 15:06:44

dataWQ= dataWQ[-c(3),]
dataCalFP = dataCalFP[-c(3),]

# Remove the 24 hr sampling data (except first sample) # 
#dataWQ= dataWQ[-c(19:36),]
#dataCalFP = dataCalFP[-c(19:36),]

#### Use the PLSR model to identify the correct number of components for each param using RMSE ####
# code addapted from CCC original PLSR script
num_comps(param="TFe_mgL",dataWQ,dataCalFP,TS_FP)

#### Run the function for a single parameter ####
param<-"TFe_mgL"
ncomp=15
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

plot(RMSEP(fit), legendpos = "topright")

png("RMSEP21_TFe_epi_4comp_0out_080421.png",width = 9, height = 4, units = 'in', res = 300)
plot(RMSEP(fit), legendpos = "topright")
dev.off()

#############
#Choose the number that is at the bottom of the curve, plus 1. 
############

ncomp.onesigma <- selectNcomp(fit, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(fit, method = "randomization", plot = TRUE)

#### Run the function for a single parameter ####
param<-"TFe_mgL"
ncomp=4
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

# Bi-plot of pred vs. obs
png("Biplot21_TFe_epi_4comp_0out_080421.png",width = 9, height = 4, units = 'in', res = 300)
plot(WQ,as.matrix(WQP),
     xlab=paste("measured",param),
     ylab=c("PLSR_predicted"))
fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
abline(fit2)
dev.off()

# loading plot
png("Loading21_TFe_epi_4comp_0out_080421.png",width = 9, height = 5, units = 'in', res = 300)
plot(fit, "loading", comps = 1:4, legendpos = "topright")
abline(h = 0)
dev.off()

#If there are obvious outliers, run whats in the PLSR loop, then click on the points.
#This will give you a location of which datapoints are outliers, and you can then 
#remove them from the WQ and dataCalFP dataframes.

out <- sapply(list(WQ,as.matrix(WQP)),"[",identify(WQ,as.matrix(WQP)))
out

#Make sure that your datetimes are formatted correctly before plotting
TS_conc$DateTime <- as.POSIXct(TS_conc$DateTime, format="%Y-%m-%d %H:%M:%S")
dataWQ$DateTime <- as.POSIXct(dataWQ$DateTime, format="%m/%d/%y %H:%M")
colnames(dataWQ)[2] <- "Depth"   #rename this column for plotting

# assign the predictions to the correct column in the TS_conc matrix. This portion of the script will
# change for each parameter. change number in "sd(as.numeric(fit$residuals[,,X]))" to match number of components,
#  change column names (i.e. "TS_conc$uncerNO2_max") to match parameter.
TS_conc$uncerTFe_max <- NA
TS_conc$uncerTFe_min <- NA
TS_conc$uncerTFe_max <- WQP_TS + 1.96*sd(as.numeric(fit$residuals[,,3])) #max uncert
TS_conc$uncerTFe_min <- WQP_TS - 1.96*sd(as.numeric(fit$residuals[,,3])) #min uncert
TS_conc$uncerTFe_max <- unlist(TS_conc$uncerTFe_max)
TS_conc$uncerTFe_min <- unlist(TS_conc$uncerTFe_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,(3)]<-WQP_TS #for TFe
#TS_conc[,(4)]<-WQP_TS  #for TMn
TS_conc[,(5)]<-WQP_TS #for SFe


#### Statistics ####

# Plot residuals
png("Resid21_TFe_epi_4comp_0out_080421.png",width = 9, height = 4, units = 'in', res = 300)
par(mfrow=c(1,2))
hist(fit$residuals,main = "Residuals model")
qqnorm(fit$residuals, pch = 1, frame = FALSE)
qqline(fit$residuals, col = "steelblue", lwd = 2)
dev.off()

#Calculate RMSE
RMSE_run <-sqrt(mean((WQ-WQP$`Pfit[1:length(Pfit)]` )^2))
RMSE_run

#Calculate R2 of pred vs obs
R2_run <- lm(WQP$`Pfit[1:length(Pfit)]`~WQ)
summary(R2_run)


#### Visualize Results ####

#Create vector of HOX ON date (for plotting)
SSS = as.data.frame(ymd_hm(c("2021-06-11 11:00")))
colnames(SSS)= c("Date")

TS_conc$Depth = as.numeric(TS_conc$Depth)
#TS_conc_all$Depth = as.numeric(TS_conc_all$Depth)

# Plot all depths 
png("Pred21_TFe_epi_4comp_0out_080421.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=TS_conc, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth)), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth))) +
  #geom_path(data=WQ_all, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth))) +
  ylim(-1,3)+
  labs(x="Date", y = "Total Fe (mg/L)", title = "PLSR Results (MUX vs. Obs)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)")
  geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()

# Plot a single depth

# First, create a dataframe for each depth's predictions and WQ 
nine_m = TS_conc %>% filter(Depth == '9.0')
nine_m_WQ = dataWQ %>% filter(Depth == 9.0)

eight_m = TS_conc %>% filter(Depth == '8.0')
eight_m_WQ = dataWQ %>% filter(Depth == 8.0)

six_m = TS_conc %>% filter(Depth == '6.2')
six_m_WQ = dataWQ %>% filter(Depth == 6.2)

five_m = TS_conc %>% filter(Depth == '5.0')
five_m_WQ = dataWQ %>% filter(Depth == 5.0)

three_m = TS_conc %>% filter(Depth == '3.8')
three_m_WQ = dataWQ %>% filter(Depth == 3.8)

one_m = TS_conc %>% filter(Depth == '1.6')
one_m_WQ = dataWQ %>% filter(Depth == 1.6)

surface = TS_conc %>% filter(Depth == '0.1')
surface_WQ = dataWQ %>% filter(Depth == 0.1)

# Line plot for a single depth
png("MUX_TFe_2020_one_epi_013_9comp.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=one_m, aes(x=DateTime,y=TFe_mgL), size=0.5) +
  geom_ribbon(data=one_m, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=one_m_WQ, aes(x=DateTime, y=TFe_mgL), colour="blue") +
  #ylim(0, 7)+
  labs(x="Date", y = "TFe_mgL", title = "1.6m, epi model, 9comps, CV-RMSEP=0.14, R2=0.94 ") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
TFe_plot
dev.off()



### SAVE TS_CONC results so you can go back later and re-combine all depths ###

epi_results = TS_conc
#meta_results = TS_conc
hypo_results = TS_conc

TS_conc_all = rbind(epi_results,hypo_results)
TS_conc_all = TS_conc_all %>% group_by(Depth) %>% arrange(-desc(DateTime)) %>%
  ungroup(Depth)

#write csv of predictions
write.csv(TS_conc_all,"MUX_Oct_Nov_2020_predictions_021121.csv")


## Save dataWQ, for plotting purposes ## 

hypo_WQ = dataWQ
epi_WQ = dataWQ

WQ_all = rbind(hypo_WQ, epi_WQ)
WQ_all = WQ_all %>% group_by(Depth) %>% arrange(-desc(DateTime)) %>% ungroup(Depth)

# Remove values from 24 hr sampling (except the first sampling time), just for plotting purposes
WQ_low_freq = WQ_all[-c(7:40),]

## Plot WQ data (without 24 hr sampling)
#TFe
png("MUX_TFe_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  #geom_path(data=WQ_all, aes(x=DateTime,y=TFe_mgL, color= as.factor(Depth)), size=0.5) +
  geom_point(data=WQ_all, aes(x=DateTime,y=TFe_mgL, color= as.factor(Depth)), size=0.8) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.factor(Depth))) +
  ylim(0, 8)+
  labs(x="Date", y = "Total Fe (mg/L)", title = "Total Iron Pre- and Post-Turnover 2020 (Weekly Sampling)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()

#TMn
png("MUX_TMn_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
TMn_plot <- ggplot() +
  geom_path(data=WQ_low_freq, aes(x=DateTime,y=TMn_mgL, color= as.factor(Depth)), size=0.5) +
  geom_point(data=WQ_low_freq, aes(x=DateTime,y=TMn_mgL, color= as.factor(Depth)), size=0.8) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTMn_min, ymax=uncerTMn_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TMn_mgL, colour= as.factor(Depth))) +
  ylim(0, 4)+
  labs(x="Date", y = "Total Mn (mg/L)", title = "Total Manganese Pre- and Post-Turnover 2020 (Weekly Sampling)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TMn_plot
dev.off()