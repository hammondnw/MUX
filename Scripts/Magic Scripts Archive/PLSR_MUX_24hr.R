# PLSR Script for MUX Predictions of Fe and Mn from the 24hr Overnight Experiment
# Authors: Nick Hammond
# Last Updated: 11/07/2020


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
pathD<-"C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/Data/" #EDIT: Specify folder where data is located
setwd("C:/Users/hammo/OneDrive/Documents/Magic Sensor PLSR/")


#### Read in FCR WQ data ####
WQ<-"24HR_Overnight_Oct_16_17_2020.xlsx" #Specify file where data is located 
dataWQ <- read_xlsx(path=paste(pathD,WQ,sep=""))

# Remove last sample data from 9m because it is an outlier (don't run this code every time!!!!)
dataWQ= dataWQ[-49,]

#Subset to only include data that matches up with MUX fp data
#dataWQ = dataWQ[dataWQ$Date>"2020-06-16 14:00",]

dataWQ = dataWQ[,-c(1,2)]


#Create vector of WQ parameter names (will use this later on in assigning column names to output matrix)
WQparam <- c("TFe_mgl","TMn_mgl","SFe_mgl","SMn_mgl") 

#### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
# This step reads in the file of overlapping MUX/SCAN data and field data. 

FPcaldata_name<-"MUX_FP_Overlaps_24hr.csv"
dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
colnames(dataCalFP)<-c("ID","Date/Time","status",seq(200,750,2.5),"Valve", "Measurement Time") #Add column names

# Filter out the air valve measurements (valve #7)
dataCalFP <- dataCalFP %>%
  filter(Valve!=12)

# Remove last observation that corresponds to anomalous 9m sample (don't run this code every time!!!!)
dataCalFP = dataCalFP[-49,]


#Create vector of times and ID from the FP file
timesCalFP<-dataCalFP[,1:2] 

dataCalFP<-dataCalFP[,-c(1:3)] #Remove ID, Date/Time, and status columns
dataCalFP<-dataCalFP[,-215:-223] #Remove NAs at high wavelengths
dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix

#This replaces the ID and Date from the original dataWQ with the exact values
#from the SCAN so that manual values can be plotted later on in the TS plots

dataWQ$ID<-timesCalFP[,1]
dataWQ$DateTime<- ymd_hms(timesCalFP[,2], tz="Etc/GMT+4")


#### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) ####
# This is the 2020 SCAN data 

TimeSeriesFP_name<-"MUX_FP_TS_2020.csv"
TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",", skip=1)  #Import Time Series data as .csv file
colnames(TS_FP)<-c("ID","Date","status",seq(200,750,2.5), "Valve", "Measurement Time") #Add column names

# Filter out the air valve measurements (valve #7)
TS_FP <- TS_FP %>%
  filter(Valve!=12)

TS_FP<-TS_FP[,-c(218:224)]
TS_FP<-TS_FP[,-1]


TS_FP$Date = as.POSIXct(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S")

#Subset date range
TS_FP = TS_FP[TS_FP$Date>"2020-10-16 12:00",]
TS_FP = TS_FP[TS_FP$Date<"2020-10-17 14:30",]

Dat<-strptime(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time

# Convert Valve number to Depth for the TS_conc matrix
Valves = as.data.frame(TS_FP$Valve)
colnames(Valves)=c("valve")
valve_depth <- data.frame(
  valve = c (1:7), 
  Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
  stringsAsFactors = FALSE
)
Valves= Valves %>% 
  left_join(valve_depth, by="valve")

Depth_lab = Valves %>% 
  select(Depth)

#### Create matrix to store calculated concentrationss:TS_conc ####

TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],6))  #Create data frame for date/time and predicted values
TS_conc[,1]<- Depth_lab
TS_conc[,2]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
colnames(TS_conc)<-c("Depth", "DateTime",WQparam) #Add column names
TS_FP<-TS_FP[,(-1:-2)]
TS_FP<-TS_FP[,(-215:-216)]
TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix


#### function which does the calibration and then calculates conc from the TS ####
# for a given chemical parameter (param).  It does the calibration for a given 
#number of components (ncomp)

PLSR_SCAN<-function(param,dataCalFP,dataWQ,TS_FP,ncomp,yesplot=FALSE){
  
  WQ<-data.matrix(subset(dataWQ,select=param)) #Make matrix of the param values
  temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
  temp<-temp[complete.cases(temp),] #removes the rows containing NAs
  WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
  dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
  
  
  fit<-plsr(WQ~data.matrix(dataFP),ncomp=ncomp,validation="CV")  #PLSR model to predict param with cross validation
  summary(fit)  #See summary of PLSR model to choose number of components
  Pfit<-predict(fit,dataFP,ncomp=ncomp,type=c("response")) #Predict concentrations based on PLSR model
  #x11()
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  Pfit_TS<-predict(fit,TS_FP,ncomp=ncomp,type=c("response"))
  WQP_TS<-as.data.frame(matrix(0,1,dim(TS_FP)[1])) #Create data frame for predicted Time series values
  WQP_TS<-as.data.frame(Pfit_TS[1:length(Pfit_TS)])  #Insert predicted param values into data frame
  
  if (yesplot==TRUE){
    plot(WQ,as.matrix(WQP),
         xlab=paste("measured",param,"?g/L",sep=" "),
         ylab=c("PLSR_predicted")) #Compare predicted and lab values of param
    
    fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
    abline(fit2)
    summary(fit2)
    tp_resid <- resid(fit2)
  }
  
  assign("WQP_TS",WQP_TS,env=.GlobalEnv)
}



# Use the PLSR model to identify the correct number of components for each param using RMSE
# code addapted from CCC original PLSR script

ncomps = c(1:10)
param="TFe_mgl" #the nutrient you want to find the number of comps for 
WQ<-data.matrix(subset(dataWQ,select=param)) #matrix of the param values
temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
temp<-temp[complete.cases(temp),] #removes the rows containing NAs
WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
RMSE <- numeric(length(ncomps))

############
# Loop 1:10 components through the PLSR model and plot the RMSE of thepredictions
############

for (i in 1:length(ncomps)) {  
  fit<-plsr(WQ~data.matrix(dataFP),ncomps[i],validation="CV")  #PLSR model to predict param with cross validation
  summary(fit)  #See summary of PLSR model to choose number of components
  Pfit<-predict(fit,dataFP,ncomps[i],type=c("response")) #Predict NO3-N concentrations based on PLSR model
  #x11()
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  Pfit_TS<-predict(fit,TS_FP, ncomps[i],type=c("response"))
  WQP_TS<-as.data.frame(matrix(0,1,dim(TS_FP)[1])) #Create data frame for predicted Time series values
  WQP_TS<-as.data.frame(Pfit_TS[1:length(Pfit_TS)])  #Insert predicted param values into data frame
  RMSE[i]<-sqrt(mean((WQ-WQP$`Pfit[1:length(Pfit)]` )^2)) #write out rmse values for each # of components
  
}
plot(RMSE) #plot RMSE curve

#############
#Choose the number that is at the bottom of the curve, plus 1. 
############

#### Run the function for a single parameter ####
param<-"TFe_mgl"
ncomp=10
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
TS_conc$uncerTFe_max <- WQP_TS + 1.96*sd(as.numeric(fit$residuals[,,10])) #max uncert
TS_conc$uncerTFe_min <- WQP_TS - 1.96*sd(as.numeric(fit$residuals[,,10])) #min uncert
TS_conc$uncerTFe_max <- unlist(TS_conc$uncerTFe_max)
TS_conc$uncerTFe_min <- unlist(TS_conc$uncerTFe_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,(3)]<-WQP_TS #for TFe
#TS_conc[,(5)]<-WQP_TS #for SFe
#TS_conc[,(4)]<-WQP_TS #for TMn
#TS_conc[,(6)]<-WQP_TS #for SMn

# Subset dataWQ to 24 hr sampling period
#dataWQ_24 <- dataWQ[dataWQ$Date>"2020-10-16 08:00",]

# Now plot the results
png("MUX_SMn_2020_pred.png",width = 9, height = 4, units = 'in', res = 300) 
SMn_plot <- ggplot() +
  geom_path(data=TS_conc, aes(x=DateTime,y=SMn_mgl, color= Depth), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerSMn_min, ymax=uncerSMn_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=dataWQ, aes(x=DateTime, y=SMn_mgl, colour= as.factor(Depth))) +
  ylim(0,2.5)+
  labs(x="Date", y = "SMn_mgl", title = "Predicted SMn at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")
SMn_plot
dev.off()

#plot a single depth
nine_m = TS_conc %>% filter(Depth == '9.0')
nine_m_WQ = dataWQ %>% filter(Depth == 9.0)

five_m = TS_conc %>% filter(Depth == '5.0')
five_m_WQ = dataWQ %>% filter(Depth == 5.0)

one_m = TS_conc %>% filter(Depth == '1.6')
one_m_WQ = dataWQ %>% filter(Depth == 1.6)

surface = TS_conc %>% filter(Depth == '0.1')
surface_WQ = dataWQ %>% filter(Depth == 0.1)

three_m = TS_conc %>% filter(Depth == '3.8')
three_m_WQ = dataWQ %>% filter(Depth == 3.8)

six_m = TS_conc %>% filter(Depth == '6.2')
six_m_WQ = dataWQ %>% filter(Depth == 6.2)

eight_m = TS_conc %>% filter(Depth == '8.0')
eight_m_WQ = dataWQ %>% filter(Depth == 8.0)

png("MUX_TFe_24hr_pred_nine_m.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_point(data=nine_m, aes(x=DateTime,y=TFe_mgl), size=1.5) +
  #geom_ribbon(data=five_m, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=nine_m_WQ, aes(x=DateTime, y=TFe_mgl), colour="blue", size=2) +
  #ylim(0,1)+
  labs(x="Date", y = "Total Iron (mg/L)", title = "Predicted Total Fe at 9.0m")+
  #scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  #scale_color_continuous(labels=c("Predicted","Observed"))+
  #theme_classic()+
  theme(legend.position="none")
TFe_plot
dev.off()

#write csv of predictions
write.csv(TS_conc,"MUX_24hr_TMn_predictions_111620.csv")


#Calculate RMSE
RMSE_run <-sqrt(mean((WQ-WQP$`Pfit[1:length(Pfit)]` )^2))
RMSE_run

#Calculate R2 of pred vs obs
R2_run <- lm(WQP$`Pfit[1:length(Pfit)]`~WQ)
summary(R2_run)

