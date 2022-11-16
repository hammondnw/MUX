
#*****************************************************************
#* TITLE:   PLSR Script for 2020 MUX Predictions of Fe and Mn with bootstrap predictive intervals
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 09/26/2022
#*                                    
#* NOTES:  This script combines all MUX FP files from 2021, plots data for QA/QC, 
#*         matches WQ sampling data to FP data, and creates data files for MUX FP
#*         Overlaps (for PLSR calibration) plus the entire FP time series (for prediction 
#*         w/ fitted PLSR models).
#*         
#*        The WQ sampling times do not always match up perfectly with a MUX FP
#*        measurement time. This is because we collected grab samples using the 
#*        van dorn sampler, not from the MUX itself. We tried to time our grab sampling
#*        to match that of the MUX, but there are cases when the closest FP measurement
#*        is up to ~ 4 hr off from the sampling time.
#*****************************************************************

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
library(enpls)

#data path and working directory locations
pathD<-"C:/Users/hammo/Documents/Magic Sensor PLSR/Data/" #EDIT: Specify folder where data is located
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")

#load functions
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_function.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_function_boot.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_data_prep_function.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_num_components_function.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_function_enpls.R')

#### Specify input files, depths, date ranges, and parameters ####

#Specify files for WQ data, FP overlaps, and the entire FP time series
WQ_name<-"Metals_2014_2021.csv"
FPcaldata_name<-"MUX_FP_Overlaps_Oct_Nov_2020.csv"
TimeSeriesFP_name<-"MUX_FP_TS_2020.csv"

#Select Desired Depths
Depths<-c("0.1",
  "1.6",
  "3.8"
  #"5.0",
  #"6.2", 
  #"8.0", 
  #"9.0"
)

#Select Desired Date Range
Begin_time<- c("2020-10-15 12:00")    # Joyful Beginning: "2020-10-15 12:00"
End_time<- c("2020-11-09 15:00:00")  # Bitter End: "2020-11-09 15:00:00" # Turnover: "2020-11-02 12:00:00"
                                    # make sure this is after the last FP *and* sampling times!

#Select WQ parameter (e.g. "TFe")
WQparam <- c("TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL") 



#### Run function to clean/prep data for PLSR ####
data_prep(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam)
# May want to add code to check that this worked properly! 

#### Remove lower wavelengths to correct fouling ####
dataCalFP = dataCalFP[,-c(1:20)] # 20 = < 250nm; 40 = < 300nm
TS_FP = TS_FP[,-c(1:20)]


# Variable indicating parameter to be modeled
param = "TFe_mgL" 

# Identify Outliers #
maxcomp = 5 # set the maximum number of components for the enpls fit function (which automatically determines components)
reps = 50 # set the number of Monte Carlo repetitions 

# Run enpls function and look at outlier plot to identify values that 
# should potentially be deleted

PLSR_enpls(param,dataCalFP,dataWQ,maxcomp=maxcomp,reptimes=reps)

# If there are any outliers that should be deleted, set them to
# NA in the dataWQ dataframe (columns: 3. Tot Fe, 4. Tot Mn, 5. Sol Fe, 6. Sol Mn)

dataWQ[c(12),param] = NA_real_



#### Identify number of components ####
##   Run the function for a single parameter and look at RMSEP curve ##
param<-"TFe_mgL"
ncomp=15
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

plot(RMSEP(fit), legendpos = "topright")

png("RMSEP20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300)
plot(RMSEP(fit), legendpos = "topright",main = param)
dev.off()

#############
#Choose the number that is at the bottom of the curve, plus 1. 
############


#### Run the function for a single parameter to generate predictions and PI ####
param<-"TFe_mgL"
ncomp=5
PLSR_SCAN_boot(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

# Bi-plot of pred vs. obs
png("Biplot20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300)
plot(WQ,as.matrix(WQP),
     xlab=paste("Measured",param),
     ylab=paste("PLSR Predicted",param),
     xlim = c(0,max(WQ,as.matrix(WQP))),
     ylim = c(0,max(WQ,as.matrix(WQP))))
fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
abline(a=0,b=1)
dev.off()

# loading plot
png("Loading20_TFe_epi_5comp_1out_050622.png",width = 9, height = 5, units = 'in', res = 300)
plot(fit, "loading", comps = 1:ncomp, legendpos = "topright")
abline(h = 0)
dev.off()


#Make sure that your datetimes are formatted correctly before plotting
TS_conc$DateTime <- as.POSIXct(TS_conc$DateTime, format="%Y-%m-%d %H:%M:%S")
dataWQ$DateTime <- as.POSIXct(dataWQ$DateTime, format="%m/%d/%y %H:%M:%S")


# assign the predictions to the correct column in the TS_conc matrix. This portion of the script will
# change for each parameter. change number in "sd(as.numeric(fit$residuals[,,X]))" to match number of components,
#  change column names (i.e. "TS_conc$uncerNO2_max") to match parameter.
TS_conc$uncerTFe_max <- NA
TS_conc$uncerTFe_min <- NA
TS_conc$uncerTFe_max <- WQP_TS + pred_int[2,] #max uncert
TS_conc$uncerTFe_min <- WQP_TS + pred_int[1,] #min uncert
TS_conc$uncerTFe_max <- unlist(TS_conc$uncerTFe_max)
TS_conc$uncerTFe_min <- unlist(TS_conc$uncerTFe_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,param]<-WQP_TS 


#### Statistics ####

# Plot residuals
png("Resid20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300)
par(mfrow=c(1,2))
hist(fit$residuals[,,ncomp],main = "Residuals model")
qqnorm(fit$residuals[,,ncomp], pch = 1, frame = FALSE)
qqline(fit$residuals[,,ncomp], col = "steelblue", lwd = 2)
shapiro.test(fit$residuals[,,ncomp]) # if p < 0.05, the residuals are NOT normal
dev.off()


#### Visualize Results ####

#Create vector of turnover date (for plotting)
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")

TS_conc$Depth_m = as.numeric(TS_conc$Depth_m)

# Plot all depths 
png("Pred20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=TS_conc, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.5) +
  geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m))) +
  #ylim(0,7)+
  labs(x="Date", y = "SFe (mg/L)", title = "PLSR Preds w/ bootstrap PI") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)",fill="90% PI")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()



### SAVE TS_CONC results so you can go back later and re-combine all depths ###

#epi_results = TS_conc
#hypo_results = TS_conc

## Save dataWQ, for plotting purposes ## 

#hypo_WQ = dataWQ
#epi_WQ = dataWQ

TS_conc_all = rbind(epi_results,hypo_results)
TS_conc_all = TS_conc_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>%
  ungroup(Depth_m)


WQ_all = rbind(hypo_WQ, epi_WQ)
WQ_all = WQ_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>% ungroup(Depth_m)


#write csv of predictions
write.csv(TS_conc_all,"MUX20_predictions_boot_050622.csv")

#write csv of dataWQ
write.csv(WQ_all,"MUX20_dataWQ_050622.csv")













#### Old Code ####

# Remove values from 24 hr sampling (except the first sampling time), just for plotting purposes
WQ_low_freq = WQ_all[-c(7:40),]

## Plot WQ data (without 24 hr sampling)
#TFe
png("MUX_TFe_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  #geom_path(data=WQ_all, aes(x=DateTime,y=TFe_mgL, color= as.factor(Depth_m)), size=0.5) +
  geom_point(data=WQ_all, aes(x=DateTime,y=TFe_mgL, color= as.factor(Depth_m)), size=0.8) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.factor(Depth_m))) +
  ylim(0, 8)+
  labs(x="Date", y = "Total Fe (mg/L)", title = "Total Iron Pre- and Post-Turnover 2020 (Weekly Sampling)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth_m (m)")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()

#TFe
png("MUX_TFe_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=WQ_low_freq, aes(x=DateTime,y=TFe_mgL, color= as.factor(Depth_m)), size=0.5) +
  geom_point(data=WQ_low_freq, aes(x=DateTime,y=TFe_mgL, color= as.factor(Depth_m)), size=0.8) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.factor(Depth_m))) +
  ylim(0, 4)+
  labs(x="Date", y = "Total Mn (mg/L)", title = "Total Manganese Pre- and Post-Turnover 2020 (Weekly Sampling)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()


# Plot a single depth

# First, create a dataframe for each depth's predictions and WQ 
nine_m = TS_conc %>% filter(Depth_m == '9.0')
nine_m_WQ = dataWQ %>% filter(Depth_m == 9.0)

eight_m = TS_conc %>% filter(Depth_m == '8.0')
eight_m_WQ = dataWQ %>% filter(Depth_m == 8.0)

six_m = TS_conc %>% filter(Depth_m == '6.2')
six_m_WQ = dataWQ %>% filter(Depth_m == 6.2)

five_m = TS_conc %>% filter(Depth_m == '5.0')
five_m_WQ = dataWQ %>% filter(Depth_m == 5.0)

three_m = TS_conc %>% filter(Depth_m == '3.8')
three_m_WQ = dataWQ %>% filter(Depth_m == 3.8)

one_m = TS_conc %>% filter(Depth_m == '1.6')
one_m_WQ = dataWQ %>% filter(Depth_m == 1.6)

surface = TS_conc %>% filter(Depth_m == '0.1')
surface_WQ = dataWQ %>% filter(Depth_m == 0.1)

# Line plot for a single depth
png("MUX_TFe_2020_pred_surface_epi_013_9comp.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=surface, aes(x=DateTime,y=TFe_mgL), size=0.5) +
  geom_ribbon(data=surface, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=surface_WQ, aes(x=DateTime, y=TFe_mgL), colour="blue") +
  #ylim(0, 7)+
  labs(x="Date", y = "TFe_mgL", title = "Predicted TFe at 0.1m, epi only model") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
TFe_plot
dev.off()