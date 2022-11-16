
# PLSR Script for 1.6m SCAN Predictions of Fe and Mn
# Authors: Nick Hammond
# Last Updated: 11/06/2020


#### Set up your packages, data location, and working directory ####

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
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_SCAN_data_prep_function.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_num_components_function.R')

#### Specify input files, depths, date ranges, and parameters ####

#Specify files for WQ data, FP overlaps, and the entire FP time series
WQ_name<-"FCR_Jan_Nov_2020.xlsx"
FPcaldata_name<-"SSCAN_FP_Overlaps_2020.csv"
TimeSeriesFP_name<-"SSCAN_FP_TS_2020.csv"

#Select Desired Depths
Depths<-c(#"0.1",
  "1.6"
  #"3.8"
  #"5.0",
  #"6.2", 
  #"8.0", 
  #"9.0"
)

#Select Desired Date Range
Begin_time<- as.POSIXct(c("2020-04-10 01:00:00"), format = "%Y-%m-%d %H:%M:%S",tz='UTC')
End_time<- as.POSIXct(c("2020-11-09 15:00:00"), format = "%Y-%m-%d %H:%M:%S",tz='UTC') #make sure this is after the last FP *and* sampling times!

#Select WQ parameter (e.g. "TFe")
WQparam <- c("TFe_mgl","TMn_mgl","SFe_mgl","SMn_mgl") 

#### Run function to clean/prep data for PLSR ####
data_prep(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam)
# May want to add code to check that this worked properly! 

# Remove outliers #  (don't run this code every time!!!!)
 #dataWQ= dataWQ[-c(9,21),]
 #dataCalFP = dataCalFP[-c(9,21),]


#### Use the PLSR model to identify the correct number of components for each param using RMSE ####
# code addapted from CCC original PLSR script
num_comps(param="TFe_mgl",dataWQ,dataCalFP,TS_FP)


#### Run the function for a single parameter ####
param<-"TFe_mgl"
ncomp=15
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

plot(RMSEP(fit), legendpos = "topright")

#############
#Choose the number that is at the bottom of the curve, plus 1. 
############

# Choose number of components using the 'one sigma' and 'permutation' methods
ncomp.onesigma <- selectNcomp(fit, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(fit, method = "randomization", plot = TRUE)


#### Re-Run the function for a single parameter, using the correct number of comps ####
param<-"TFe_mgl"
ncomp=5
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)


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
TS_conc$uncerTFe_max <- WQP_TS + 1.96*sd(as.numeric(fit$residuals[,,5])) #max uncert
TS_conc$uncerTFe_min <- WQP_TS - 1.96*sd(as.numeric(fit$residuals[,,5])) #min uncert
TS_conc$uncerTFe_max <- unlist(TS_conc$uncerTFe_max)
TS_conc$uncerTFe_min <- unlist(TS_conc$uncerTFe_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,(2)]<-WQP_TS #for TFe
#TS_conc[,(5)]<-WQP_TS #for SFe

#### Statistics ####

# Plot residuals
png("resid_normlty_hypo.png",width = 9, height = 4, units = 'in', res = 300)
par(mfrow=c(1,2))
hist(fit$residuals,main = "Hypolimnion model")
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

#Create vector of turnover date (for plotting)
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")


# Plot 
png("SCAN_TFe_2020_full_5comp_042021.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=TFe_mgl), size=0.5) +
  geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgl),colour="blue") +
  ylim(0,3.5)+
  coord_cartesian(xlim=(c(as.POSIXct("2020-10-16 13:00:00", tz='UTC'),as.POSIXct("2020-11-09 15:00:00",tz='UTC'))))+
  labs(x="Date", y = "Total Fe (mg/L)", title = "1.6m SCAN, full ts fit, 5 comps, CV-RMSEP=0.21,R2=0.70") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")+
  labs(color= "Depth (m)")
  #geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()
