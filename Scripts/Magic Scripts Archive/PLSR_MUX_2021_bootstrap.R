#*****************************************************************
#* TITLE:   PLSR Analysis Script for 2021 MUX Predictions of Fe and Mn with bootstrap predictive intervals
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 09/26/2022
#*                                    
#* NOTES:  This script utilizes data files from '1b_Overlaps_MUX_2021.R' to fit PLSR
#*        models using MUX FP data (i.e., the time series of absorbance spectra) and FCR Fe and Mn sampling data, 
#*        then makes predictions of total and soluble Fe and Mn concentrations with 90% bootstrap predictive intervals. 
#*        The individual steps of the analysis include:
#*        1. Read in data files and prepare data for model fitting
#*        2. Remove lower wavelengths in absorbance spectra to correct for fouling
#*        3. Identify and remove outliers in calibration data using the enpls function
#*        4. Identify the optimal number of components for each PLSR model
#*        5. Fit PLSR model to data and calculate 90 % bootstrap predictive intervals
#*        6. Assess model statistics and visualize results
#*        7. Create data file for predicted values with 90 % predictive intervals
#*        
#*        While certain steps in this process are automated using custom functions, there are other
#*        steps which must be performed manually and involve some subjective decisions on the part of
#*        the analyst. These include:
#*        1. Specifying data to be included in each PLSR model
#*           - Variable (Total Fe, Total Mn, Soluble Fe, or Soluble Mn): we only include a single response variable for each model
#*           - Time Interval: this script uses data from the entire 2021 MUX deployment
#*           - Depths: we fit separate models for the epilimnion (0.1, 1.6, and 3.8m) and
#*                      hypolimnion (6.2, 8, and 9m). 5m data is excluded
#*           - Absorbance data: we remove wavelengths below 250nm to minimize the fouling signal in the time series
#*           - Outliers: This is one of the most subjective steps. Outliers are determined using the function 'enpls.od',
#*             but we only excluded these outliers if it led to a substantial improvement in model fit AND 
#*             we had a second reason to justify their removal 
#*             (e.g., measurements collected during heavy fouling, extremely high/low concentration values)
#*             Details on which outliers were removed and the final sample size of calibration for each model
#*             can be found in Table 1 of the MUX manuscript and in Nick's 'PLSR Model Assessment' Google Sheets.
#*        2. Choosing the number of components for each PLSR model
#*           - This is also a somewhat subjective step, but we are using the RMSEP vs. num. comps
#*            curve generated from cross-validation to determine how many comps to use
#*           - Generally, it's best to use the lowest number of components possible
#*           - The number of components should not greatly exceed 10% * n     
#*        3. This workflow is currently an iterative process; in other words, the process is 
#*           repeated for each variable + depth layer combination (e.g., TFe epi, TFe hypo, SFe epi,...)
#*           Results from each model are saved in the 'TS_conc' dataframe and can be written to .csv at the end.
#*           WQ data are also saved as a dataframe and written to .csv at the end.
#*                                   
#*        
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
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_data_prep_function_2021.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_num_components_function.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_function_enpls.R')


#### Specify input files, depths, date ranges, and parameters ####

#Specify files for WQ data, FP overlaps, and the entire FP time series
WQ_name<-"Metals_2014_2021.csv"
FPcaldata_name<-"MUX_FP_Overlaps_2021.csv"
TimeSeriesFP_name<-"MUX_FP_TS_2021.csv"

#Select Desired Depths
Depths<-c(#"0.1",
          #"1.6",
          #"3.8"
          #"5.0",
          "6.2", 
          "8.0",
          "9.0"
)

#Select Desired Date Range
Begin_time<- c("2021-05-26 00:00:00") # Full TS: "2021-05-26 00:00:00" Experiment start time: 5/26/2021 13:58
                                      # Start at HOx-ON: "2021-06-11 12:00:00"
End_time<- c("2021-06-21 14:00:00")   # Full TS: "2021-06-21 14:00:00" Experiment end time: 6/21/2021 8:47
#make sure this is after the last FP *and* sampling times!

#Select WQ parameter (e.g. "TMn")
WQparam <- c("TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL") 



#### Run function to clean/prep data for PLSR ####
data_prep(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam)



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

dataWQ[c(12,15),param] = NA_real_





#### Identify number of components ####
##   Run the function for a single parameter and look at RMSEP curve ##
param<-"TFe_mgL"
ncomp=15
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

plot(RMSEP(fit), legendpos = "topright")

png("RMSEP21_TFe_epi_4comp_1out_051322.png",width = 9, height = 4, units = 'in', res = 300)
plot(RMSEP(fit), legendpos = "topright",main = param)
dev.off()

#############
#Choose the number that is at the bottom of the curve, plus 1. 
############


#### Run the function for a single parameter to generate predictions and PI ####
param<-"TFe_mgL"
ncomp=4
PLSR_SCAN_boot(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)


# Bi-plot of pred vs. obs
png("Biplot21_TFe_epi_4comp_1out_051322.png",width = 9, height = 4, units = 'in', res = 300)
plot(WQ,as.matrix(WQP),
     xlab=paste("measured",param),
     ylab=paste("PLSR Predicted",param),
     xlim = c(0,max(WQ,as.matrix(WQP))),
     ylim = c(0,max(WQ,as.matrix(WQP))))
fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
abline(a=0,b=1)
dev.off()

# loading plot
png("Loading21_TFe_epi_4comp_1out_051322.png",width = 9, height = 5, units = 'in', res = 300)
plot(fit, "loading", comps = 1:ncomp, legendpos = "topright")
abline(h = 0)
dev.off()


#Make sure that your datetimes are formatted correctly before plotting
TS_conc$DateTime <- as.POSIXct(TS_conc$DateTime, format="%Y-%m-%d %H:%M:%S")
dataWQ$DateTime <- as.POSIXct(dataWQ$DateTime, format="%m/%d/%y %H:%M")

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
TS_conc[,(param)]<-WQP_TS 

#### Statistics ####

# Plot residuals
png("Resid21_TFe_epi_4comp_1out_051322.png",width = 9, height = 4, units = 'in', res = 300)
par(mfrow=c(1,2))
hist(fit$residuals[,,ncomp],main = "Residuals model")
qqnorm(fit$residuals[,,ncomp], pch = 1, frame = FALSE)
qqline(fit$residuals[,,ncomp], col = "steelblue", lwd = 2)
shapiro.test(fit$residuals[,,ncomp]) # if p < 0.05, the residuals are NOT normal
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

TS_conc$Depth_m = as.numeric(TS_conc$Depth_m)
#TS_conc_all$Depth = as.numeric(TS_conc_all$Depth)

# Plot all depths 
png("Pred21_TFe_epi_4comp_1out_051322.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=TS_conc, aes(x=DateTime,y=TFe_mgL, color= as.character(Depth_m)), size=0.5) +
  geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = as.character(Depth_m)), alpha = 0.3)+
  geom_point(data=dataWQ, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m))) +
  #geom_path(data=WQ_all, aes(x=DateTime, y=TFe_mgL, colour= as.character(Depth_m))) +
  #ylim(0,7)+
  labs(x="Date", y = "TFe (mg/L)", title = "PLSR Preds w/ bootstrap PI") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)",fill="90% PI")+
geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TFe_plot
dev.off()



### SAVE TS_CONC results so you can go back later and re-combine all depths ###

epi_results = TS_conc
#meta_results = TS_conc
hypo_results = TS_conc

TS_conc_all = rbind(epi_results,hypo_results)
TS_conc_all = TS_conc_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>%
  ungroup(Depth_m)

#write csv of predictions
write.csv(TS_conc_all,"MUX21_predictions_boot_051322.csv")




## Save dataWQ, for plotting purposes ## 

hypo_WQ = dataWQ
epi_WQ = dataWQ

WQ_all = rbind(hypo_WQ, epi_WQ)
WQ_all = WQ_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>% ungroup(Depth_m)

#write csv of dataWQ
write.csv(WQ_all,"MUX21_dataWQ_051322.csv")



# Remove values from 24 hr sampling (except the first sampling time), just for plotting purposes
WQ_low_freq = WQ_all[-c(7:40),]

## Plot WQ data (without 24 hr sampling)
#SFe
png("MUX_SFe_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
SFe_plot <- ggplot() +
  #geom_path(data=WQ_all, aes(x=DateTime,y=SFe_mgL, color= as.factor(Depth)), size=0.5) +
  geom_point(data=WQ_all, aes(x=DateTime,y=SFe_mgL, color= as.factor(Depth)), size=0.8) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerSFe_min, ymax=uncerSFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=SFe_mgL, colour= as.factor(Depth))) +
  ylim(0, 8)+
  labs(x="Date", y = "TMn (mg/L)", title = "Total Iron Pre- and Post-Turnover 2020 (Weekly Sampling)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
TMn_plot
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


### Old code ###

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
png("MUX_TMn_2020_one_epi_013_9comp.png",width = 9, height = 4, units = 'in', res = 300) 
TMn_plot <- ggplot() +
  geom_path(data=one_m, aes(x=DateTime,y=TMn_mgL), size=0.5) +
  geom_ribbon(data=one_m, aes(ymin=uncerTMn_min, ymax=uncerTMn_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=one_m_WQ, aes(x=DateTime, y=TMn_mgL), colour="blue") +
  #ylim(0, 7)+
  labs(x="Date", y = "TMn_mgL", title = "1.6m, epi model, 9comps, CV-RMSEP=0.14, R2=0.94 ") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
TMn_plot
dev.off()

