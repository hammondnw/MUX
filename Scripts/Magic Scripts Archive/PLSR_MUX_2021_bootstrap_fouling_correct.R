# PLSR Script for 2021 MUX Predictions of Fe and Mn with bootstrap predictive intervals
# Applying different methods for fouling correction
# Authors: Nick Hammond
# Last Updated: 10/11/2021



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
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_function_boot.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_data_prep_function_2021.R')
source('ManualDownloadsSCCData/Scripts/Current Magic Scripts/PLSR_num_components_function.R')


#### Specify input files, depths, date ranges, and parameters ####

#Specify files for WQ data, FP overlaps, and the entire FP time series
WQ_name<-"MUX_WQ_2021_doubles.csv"
FPcaldata_name<-"MUX_FP_Overlaps_2021_doubles.csv"
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
mean_TFe = mean(dataWQ$TFe_mgL)
mean_SFe = mean(dataWQ$SFe_mgL)
mean_TMn = mean(dataWQ$TMn_mgL)
mean_SMn = mean(dataWQ$SMn_mgL)
sd_TFe = sd(dataWQ$TFe_mgL)
sd_SFe = sd(dataWQ$SFe_mgL)
sd_TMn = sd(dataWQ$TMn_mgL)
sd_SMn = sd(dataWQ$SMn_mgL)

dataWQ = dataWQ %>% mutate(TFe_mgL = if_else(TFe_mgL < mean_TFe+3*sd_TFe & 
                                               TFe_mgL > mean_TFe-3*sd_TFe, TFe_mgL, NA_real_),
                           SFe_mgL = if_else(SFe_mgL < mean_SFe+3*sd_SFe & 
                                               SFe_mgL > mean_SFe-3*sd_SFe, SFe_mgL, NA_real_),
                           TMn_mgL = if_else(TMn_mgL < mean_TMn+3*sd_TMn & 
                                               TMn_mgL > mean_TMn-3*sd_TMn, TMn_mgL, NA_real_),
                           SMn_mgL = if_else(SMn_mgL < mean_SMn+3*sd_SMn & 
                                               SMn_mgL > mean_SMn-3*sd_SMn, SMn_mgL, NA_real_))

#dataWQ= dataWQ[-c(15),]
#dataCalFP = dataCalFP[-c(15),]

#Remove the really low 9m sample from 2021-06-04 16:27:59

#dataWQ= dataWQ[-c(12),]
#dataCalFP = dataCalFP[-c(12),]

#Remove outlier 9m sample from 2021-05-26 15:06:44

#dataWQ= dataWQ[-c(4),]
#dataCalFP = dataCalFP[-c(4),]

# Remove the 24 hr sampling data (except first sample) # 
#dataWQ= dataWQ[-c(19:36),]
#dataCalFP = dataCalFP[-c(19:36),]


#### Remove lower wavelengths to correct fouling ####
dataCalFP = dataCalFP[,-c(1:20)] # 10 = <225nm; 20 = < 250nm; 30 = <275nm; 40 = < 300nm
TS_FP = TS_FP[,-c(1:20)]


#### Use the PLSR model to identify the correct number of components for each param using RMSE ####
# code addapted from CCC original PLSR script
num_comps(param="SMn_mgL",dataWQ,dataCalFP,TS_FP)

#### Run the function for a single parameter ####
param<-"SMn_mgL"
ncomp=15
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

plot(RMSEP(fit), legendpos = "topright")

png("RMSEP21_SMn_epi_6comp_1out_020922.png",width = 9, height = 4, units = 'in', res = 300)
plot(RMSEP(fit), legendpos = "topright")
dev.off()

#############
#Choose the number that is at the bottom of the curve, plus 1. 
############

ncomp.onesigma <- selectNcomp(fit, method = "onesigma", plot = TRUE)
ncomp.permut <- selectNcomp(fit, method = "randomization", plot = TRUE)

#### Run the function for a single parameter ####
param<-"SMn_mgL"
ncomp=3
PLSR_SCAN_boot(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

# Bi-plot of pred vs. obs
png("Biplot21_SMn_epi_6comp_1out_020922.png",width = 9, height = 4, units = 'in', res = 300)
plot(WQ,as.matrix(WQP),
     xlab=paste("measured",param),
     ylab=c("PLSR_predicted"),
     xlim = c(0,max(WQ,as.matrix(WQP))),
     ylim = c(0,max(WQ,as.matrix(WQP))))
fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
abline(a=0,b=1)
dev.off()

# loading plot
png("Loading21_SMn_epi_6comp_1out_020922.png",width = 9, height = 5, units = 'in', res = 300)
plot(fit, "coef", comps = 1:5, legendpos = "topright")
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
TS_conc$uncerSMn_max <- NA
TS_conc$uncerSMn_min <- NA
TS_conc$uncerSMn_max <- WQP_TS + pred_int[2,] #max uncert
TS_conc$uncerSMn_min <- WQP_TS + pred_int[1,] #min uncert
TS_conc$uncerSMn_max <- unlist(TS_conc$uncerSMn_max)
TS_conc$uncerSMn_min <- unlist(TS_conc$uncerSMn_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,(3)]<-WQP_TS #for Tot Fe
TS_conc[,(4)]<-WQP_TS  #for Tot Mn
TS_conc[,(5)]<-WQP_TS #for Sol Fe
TS_conc[,(6)]<-WQP_TS #for Sol Mn

#### Statistics ####

# Plot residuals
png("Resid21_SMn_epi_6comp_1out_020922.png",width = 9, height = 4, units = 'in', res = 300)
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

TS_conc$Depth = as.numeric(TS_conc$Depth)
#TS_conc_all$Depth = as.numeric(TS_conc_all$Depth)

# Plot all depths 
png("Pred21_SMn_epi_6comp_1out_020922.png",width = 9, height = 4, units = 'in', res = 300) 
SMn_plot <- ggplot() +
  geom_path(data=TS_conc, aes(x=DateTime,y=SMn_mgL, color= as.character(Depth)), size=0.5) +
  geom_ribbon(data=TS_conc, aes(ymin=uncerSMn_min, ymax=uncerSMn_max, x=DateTime, fill = as.character(Depth)), alpha = 0.3)+
  geom_point(data=dataWQ, aes(x=DateTime, y=SMn_mgL, colour= as.character(Depth))) +
  #geom_path(data=WQ_all, aes(x=DateTime, y=SMn_mgL, colour= as.character(Depth))) +
  #ylim(0,7)+
  labs(x="Date", y = "Total Mn (mg/L)", title = "PLSR Preds w/ bootstrap PI") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)",fill="90% PI")+
  geom_vline(data=SSS, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
SMn_plot
dev.off()


##### Save TS_conc as a new df ####

### Splits ###

full = TS_conc
#split_pre = TS_conc
#split_post = TS_conc

full = full %>% mutate(time_series = "Full")
split_pre = split_pre %>% mutate(time_series = "Split")
split_post = split_post %>% mutate(time_series = "Split")

split_df = union(full,split_pre)
split_df = union(split_df,split_post)

split_df = split_df %>% arrange(-desc(DateTime))

write.csv(split_df,"MUX21_fouling_corr_splits_040822.csv")


### Doubles ###

#normal = TS_conc
#doubles = TS_conc
#doubles_250 = TS_conc

normal = normal %>% mutate(correction = "no correction")
doubles = doubles %>% mutate(correction = "doubles")
doubles_250 = doubles_250 %>% mutate(correction = "doubles_250")

doubles_df = union(normal,doubles)
doubles_df = union(doubles_df,doubles_250)

doubles_df = doubles_df %>% arrange(-desc(DateTime))

write.csv(doubles_df,"MUX21_fouling_corr_doubles_031522.csv")

### Subs ###

#full = TS_conc
#sub_225 = TS_conc
#sub_250 = TS_conc
#sub_275 = TS_conc
sub_300 = TS_conc

full = full %>% mutate(correction = "full")
sub_225 = sub_225 %>% mutate(correction = "225nm")
sub_250 = sub_250 %>% mutate(correction = "250nm")
sub_275 = sub_275 %>% mutate(correction = "275nm")
sub_300 = sub_300 %>% mutate(correction = "300nm")

fouling_df = union(full,sub_225)
fouling_df = union(fouling_df,sub_250)
fouling_df = union(fouling_df,sub_275)
fouling_df = union(fouling_df,sub_300)

fouling_df = fouling_df %>% arrange(desc(DateTime))

write.csv(fouling_df,"MUX21_fouling_corr_031122.csv")

### SAVE TS_CONC results so you can go back later and re-combine all depths ###

epi_results = TS_conc
#meta_results = TS_conc
hypo_results = TS_conc

TS_conc_all = rbind(epi_results,hypo_results)
TS_conc_all = TS_conc_all %>% group_by(Depth) %>% arrange(-desc(DateTime)) %>%
  ungroup(Depth)

#write csv of predictions
write.csv(TS_conc_all,"MUX21_predictions_boot_020922.csv")




## Save dataWQ, for plotting purposes ## 

hypo_WQ = dataWQ
epi_WQ = dataWQ

WQ_all = rbind(hypo_WQ, epi_WQ)
WQ_all = WQ_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>% ungroup(Depth_m)

#write csv of dataWQ
write.csv(WQ_all,"MUX21_dataWQ_021122.csv")



# Remove values from 24 hr sampling (except the first sampling time), just for plotting purposes
WQ_low_freq = WQ_all[-c(7:40),]

## Plot WQ data (without 24 hr sampling)
#SMn
png("MUX_SMn_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
SMn_plot <- ggplot() +
  #geom_path(data=WQ_all, aes(x=DateTime,y=SMn_mgL, color= as.factor(Depth)), size=0.5) +
  geom_point(data=WQ_all, aes(x=DateTime,y=SMn_mgL, color= as.factor(Depth)), size=0.8) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerSMn_min, ymax=uncerSMn_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=SMn_mgL, colour= as.factor(Depth))) +
  ylim(0, 8)+
  labs(x="Date", y = "Total Fe (mg/L)", title = "Total Iron Pre- and Post-Turnover 2020 (Weekly Sampling)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")+
  labs(color= "Depth (m)")+
  geom_vline(data=turnover, aes(xintercept=Date), linetype="dashed", color="black", size=0.8)
SMn_plot
dev.off()

#SMn
png("MUX_SMn_Oct_Nov_2020_epi_hypo_WQ_022221.png",width = 9, height = 4, units = 'in', res = 300) 
SMn_plot <- ggplot() +
  geom_path(data=WQ_low_freq, aes(x=DateTime,y=SMn_mgL, color= as.factor(Depth)), size=0.5) +
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

