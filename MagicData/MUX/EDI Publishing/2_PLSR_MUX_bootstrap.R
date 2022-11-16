#*****************************************************************
#* TITLE:   PLSR Analysis Script for MUX Predictions of Fe and Mn with bootstrap predictive intervals
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 11/08/2022
#*                                    
#* NOTES:  This script utilizes data files from '1_Overlaps_MUX.R' and metals data from EDI to fit PLSR
#*        models using MUX FP data (i.e., the time series of absorbance spectra) and FCR Fe and Mn sampling data, 
#*        then makes predictions of total and soluble Fe and Mn concentrations with 90% bootstrap predictive intervals.
#*        This script can be run for either deployment, just change 'FPcaldata_name', the 
#*        date range, and the data prep function to the desired year.
#*        The individual steps of the analysis include:
#*        1. Read in data files and prepare data for model fitting
#*        2. Remove lower wavelengths in absorbance spectra to correct for fouling
#*        3. Identify and remove outliers in calibration data using the enpls function
#*        4. Identify the optimal number of components for each PLSR model
#*        5. Fit PLSR model to data and calculate 90 % bootstrap predictive intervals
#*        6. Assess model statistics and visualize results
#*        7. Create data files for predicted values with 90 % predictive intervals
#*        
#*        While certain steps of this process are automated using custom functions, there are other
#*        steps which must be performed manually and involve some subjective decisions on the part of
#*        the analyst. These include:
#*        1. Specifying data to be included in each PLSR model
#*           - Variable (Total Fe, Total Mn, Soluble Fe, or Soluble Mn): we only include a single response variable for each model
#*           - Time Interval: this script uses data from the entire 2020 or 2021 MUX deployment
#*           - Depths: we fit separate models for the epilimnion (0.1, 1.6, and 3.8m) and
#*                      hypolimnion (6.2, 8, and 9m). 5m data is excluded
#*           - Absorbance FP data: we remove wavelengths below 250nm to minimize the fouling signal in the time series
#*           - Outliers: This is one of the most subjective steps. Outliers are determined using the function 'enpls.od',
#*             but we only excluded these outliers if it led to a substantial improvement in model fit AND 
#*             we had a second reason to justify their removal (e.g., measurements collected during heavy fouling, extremely high/low concentration values)
#*             Details on which outliers were removed and the final sample size of calibration for each model
#*             can be found in Table 1 of the MUX manuscript and in Nick's 'PLSR Model Assessment' Google Sheets (https://docs.google.com/spreadsheets/d/1QCNT1cm_9-TcTgFnwmTRoLfQbLxGvEYSnfg7I8iG-yo/edit#gid=0).
#*        2. Choosing the number of components for each PLSR model
#*           - This is also a somewhat subjective step, but we are using the RMSEP vs. num. comps
#*            curve generated from cross-validation to determine how many comps to use
#*           - The rule of thumb is to choose the point at the bottom of the curve, plus 1
#*           - I sometimes chose 1-2 less components than this method suggested
#*           - If adding an additional component helps the model explain more Y-variance (WQ), but not much more X-variance (absorbance), then this
#*             component is likely over-fitting the model
#*           - The number of components should not greatly exceed 10% * n 
#*               
#*        3. This workflow is currently an iterative process; in other words, the process is 
#*           repeated for each variable + depth layer combination (e.g., TFe epi, TFe hypo, SFe epi,...)
#*           Results from each model are saved in the 'TS_conc' dataframe and can be written to .csv at the end. Just be sure
#*           to save the results from the first depth layer before re-running the data prep function (same applies to the WQ data).
#*           WQ data are also saved as a dataframe and written to .csv at the end.
#*                                   
#*        
#*****************************************************************

#### Set up your packages, data location, and working directory. Load functions. ####

#packages needed
library(lubridate)
library(tidyverse)
library(pls) 
library(scales)
library(enpls)

# Set working directory
setwd("./")

#load functions
source('./Scripts/Current Magic Scripts/PLSR_function.R')
source('./Scripts/Current Magic Scripts/PLSR_function_boot.R')
source('./Scripts/Current Magic Scripts/PLSR_data_prep_function_2020.R')
source('./Scripts/Current Magic Scripts/PLSR_data_prep_function_2021.R')
source('./Scripts/Current Magic Scripts/PLSR_num_components_function.R')
source('./Scripts/Current Magic Scripts/PLSR_function_enpls.R')


#### 1. Read in data files and prepare data for model fitting ####

#Specify files for WQ data, FP overlaps, and the entire FP time series
pathD<-"./MagicData/MUX/Modeling Files/" # Specify folder where EDI data will be downloaded
WQ_name<-"Metals_2014_2021.csv" # Specify name of WQ file
FPcaldata_name<-"MUX_FP_Overlaps_2021.csv" # Name of FP 'overlaps' file (for calibration)
TimeSeriesFP_name<-"MUX_FP_TS.csv" # Name of FP time series file (for prediction)

# Select Desired Depths
# For this study, we are dividing PLSR models by the epilimnion (0.1, 1.6, and 3.8m) and 
# hypolimnion (6.2, 8.0, and 9.0m). Metalimnion data (5.0m) is not included.
Depths<-c("0.1",
          "1.6",
          "3.8"
          #"5.0",
          #"6.2", 
          #"8.0", 
          #"9.0"
)

# Select Desired Date Range
#2020
#Begin_time<- c("2020-10-15 12:00")
#End_time<- c("2020-11-09 15:00:00") 
# 2021
Begin_time<- c("2021-05-26 00:00:00") 
End_time<- c("2021-06-21 14:00:00") 

#Select WQ parameters that you want to include at this depth (usually TFe, TMn, SFe, and SMn)
WQparam <- c("TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL") 


# Run function to clean/prep data for PLSR
#data_prep_20(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam)
data_prep_21(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam)


#### 2. Remove lower wavelengths in absorbance spectra to correct for fouling ####
dataCalFP = dataCalFP[,-c(1:20)] # 20 = < 250nm; 40 = < 300nm
TS_FP = TS_FP[,-c(1:20)]


# Now that your data is prepped, you can begin fitting PLSR models for each variable

# Variable indicating parameter to be modeled
param = "TFe_mgL" 

#### 3. Identify and remove outliers in calibration data using the enpls function ####
maxcomp = 5 # set the maximum number of components for the enpls fit function (which automatically determines components)
# 10%*n = maxcomp (+ 1-2 comps, if desired)
reps = 50 # set the number of Monte Carlo repetitions 
# generally it's better to have more reps (the default is 500), but 50 is more computationally efficient and using > 50 reps
# doesn't significantly change the results for this analysis

# Run enpls function and look at outlier plot to identify values that should potentially be deleted

PLSR_enpls(param,dataCalFP,dataWQ,maxcomp=maxcomp,reptimes=reps)

# Cutoffs are based on 3*sd of monte carlo predictive error mean (x-axis) and sd (y-axis)
# Points in the upper left are x-outliers (blue), points in the lower right are y-outliers (red),
# and points in the upper right are outliers in both directions (green).
# Criteria for removing outliers (2 required):
#  - if the point is far beyond the 3*sd cutoff
#  - if there is a substantial improvement in model fit when removed
#  - if there is a secondary justification for removal (e.g., fouling, anomalous samples)
# If there are any outliers that should be deleted, set them to NA in the dataWQ dataframe 
# (The pls function will automatically remove the rows from the dataCalFP matrix)

dataWQ[c(44),param] = NA_real_


#### 4. Identify the optimal number of components for each PLSR model ####

##   Run the function for a single parameter and look at RMSEP curve
ncomp=15 # max number of components to try
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)

#png("RMSEP20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300)
plot(RMSEP(fit), legendpos = "topright",main = param)
#dev.off()

# Choose the number at the bottom of the curve, +/- 1

#### 5. Fit PLSR model to data, generate predictions, and calculate 90 % bootstrap predictive intervals for a single parameter ####
param<-"TFe_mgL"
ncomp=4
PLSR_SCAN_boot(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)


#### 6. Assess model diagnostics, statistics, and visualize results ####

# Bi-plot of pred vs. obs
#png("Biplot20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300)
plot(WQ,as.matrix(WQP),
     xlab=paste("Measured",param),
     ylab=paste("PLSR Predicted",param),
     xlim = c(0,max(WQ,as.matrix(WQP))),
     ylim = c(0,max(WQ,as.matrix(WQP))))
fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
abline(a=0,b=1)
#dev.off()

# loading plot -- to assess how PLSR assigned coefficients to each x-variable (wavelength)
#png("Loading20_TFe_epi_5comp_1out_050622.png",width = 9, height = 5, units = 'in', res = 300)
#plot(fit, "loading", comps = 1:ncomp, legendpos = "topright")
#abline(h = 0)
#dev.off()

#Make sure that your datetimes are formatted correctly before plotting
TS_conc$DateTime <- as.POSIXct(TS_conc$DateTime, format="%Y-%m-%d %H:%M:%S")
dataWQ$DateTime <- as.POSIXct(dataWQ$DateTime, format="%m/%d/%y %H:%M:%S")


# assign the predictions to the correct column in the TS_conc matrix. 
# Before running these lines, change the names of the columns to match the variable you're working with
# E.g., if your model is for SMn, then specify it as 'TS_conc$uncerSMn_min' and 'TS_conc$uncerSMn_max'
TS_conc$uncerTFe_max <- NA
TS_conc$uncerTFe_min <- NA
TS_conc$uncerTFe_max <- WQP_TS + pred_int[2,] #max uncert
TS_conc$uncerTFe_min <- WQP_TS + pred_int[1,] #min uncert
TS_conc$uncerTFe_max <- unlist(TS_conc$uncerTFe_max)
TS_conc$uncerTFe_min <- unlist(TS_conc$uncerTFe_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,param]<-WQP_TS 


# Statistics

# Plot residuals
#png("Resid20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300)
par(mfrow=c(1,2))
hist(fit$residuals[,,ncomp],main = "Residuals model")
qqnorm(fit$residuals[,,ncomp], pch = 1, frame = FALSE)
qqline(fit$residuals[,,ncomp], col = "steelblue", lwd = 2)
shapiro.test(fit$residuals[,,ncomp]) # if p < 0.05, the residuals are NOT normal
#dev.off()


# Visualize Results 

#Create vector of turnover date (for plotting)
turnover = as.data.frame(ymd_hm(c("2020-11-02 12:00")))
colnames(turnover)= c("Date")
#Create vector of HOX ON date (for plotting)
SSS = as.data.frame(ymd_hm(c("2021-06-11 11:00")))
colnames(SSS)= c("Date")

TS_conc$Depth_m = as.numeric(TS_conc$Depth_m)

# Plot all depths 
#png("Pred20_TFe_epi_5comp_1out_050622.png",width = 9, height = 4, units = 'in', res = 300) 
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
#dev.off()


# (Save data before re-running data prep function!)



#### 7. Create data files for predicted values with 90 % predictive intervals ####

# SAVE TS_CONC results so you can go back later and re-combine all depths

epi_results = TS_conc
#hypo_results = TS_conc

## Save dataWQ, for plotting purposes ## 

epi_WQ = dataWQ
#hypo_WQ = dataWQ


# Combine epi and hypo TS_conc results
TS_conc_all = rbind(epi_results,hypo_results)
TS_conc_all = TS_conc_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>%
  ungroup(Depth_m)

# Combine epi and hypo WQ data
WQ_all = rbind(hypo_WQ, epi_WQ)
WQ_all = WQ_all %>% group_by(Depth_m) %>% arrange(-desc(DateTime)) %>% ungroup(Depth_m)


#write csv of predictions
write.csv(TS_conc_all,"./MagicData/MUX/Figures Files/MUX20_predictions_boot_101022.csv")

#write csv of dataWQ
write.csv(WQ_all,"./MagicData/MUX/Figures Files/MUX20_dataWQ_101022.csv")

