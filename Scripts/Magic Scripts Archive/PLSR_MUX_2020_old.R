
# PLSR Script for 2020 MUX Predictions of Fe and Mn
# Authors: Nick Hammond
# Last Updated: 01/29/2021


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

#load functions
source('Scripts/PLSR_function.R')

#### Read in FCR WQ data ####
WQ<-"FCR_Jan_Nov_2020.xlsx" #Specify file where data is located 
dataWQ <- read_xlsx(path=paste(pathD,WQ,sep=""))

#Subset to just include 1.6m depth
#dataWQ <- dataWQ %>%
  #filter(Depth_m == 1.6)

#Subset to desired date range
dataWQ = dataWQ[dataWQ$Date>"2020-10-15 12:00",]

#Create vector of WQ parameter names (will use this later on in assigning column names to output matrix)
WQparam <- c("TFe_mgl","TMn_mgl","SFe_mgl","SMn_mgl") 

#### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
# This step reads in the file of overlapping MUX/SCAN data and field data. 

FPcaldata_name<-"MUX_FP_Overlaps_Oct_Nov_2020.csv"
dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
colnames(dataCalFP)<-c("ID","Date/Time","status",seq(200,750,2.5),"Valve", "Measurement Time") #Add column names

# Filter out the air valve measurements (valve #7)
dataCalFP <- dataCalFP %>%
  filter(Valve!=12)

#Subset to desired date range
dataCalFP = dataCalFP[dataCalFP$Date>"2020-10-15 12:00",]

#Create vector of times and ID from the FP file
timesCalFP<-dataCalFP %>% select(ID,`Date/Time`)

dataCalFP <- dataCalFP %>% select(!c(ID,`Date/Time`,status)) #Remove ID, Date/Time, and status columns
dataCalFP<- dataCalFP %>% select(!c(`735`:`750`)) #Remove NAs at high wavelengths
dataCalFP<- dataCalFP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
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

#Subset to desired date range
TS_FP = TS_FP[TS_FP$Date>"2020-10-16 13:00",]

TS_FP<- TS_FP %>% select(!c(`735`:`750`)) #remove columns for wavelengths above 735nm because those are all NA
TS_FP<- TS_FP %>% select(!c(ID)) #remove ID column


TS_FP$Date = as.POSIXct(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S")
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


TS_FP<- TS_FP %>% select(!c(Date,status)) #remove 'Date' and 'status' columns
TS_FP<- TS_FP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix

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
ncomp=9
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
TS_conc$uncerTFe_max <- WQP_TS + 1.96*sd(as.numeric(fit$residuals[,,9])) #max uncert
TS_conc$uncerTFe_min <- WQP_TS - 1.96*sd(as.numeric(fit$residuals[,,9])) #min uncert
TS_conc$uncerTFe_max <- unlist(TS_conc$uncerTFe_max)
TS_conc$uncerTFe_min <- unlist(TS_conc$uncerTFe_min)

# Assign WQP_TS to correct parameter column in TS_conc dataframe.
TS_conc[,(3)]<-WQP_TS #for TFe
#TS_conc[,(4)]<-WQP_TS  #for TMn
#TS_conc[,(5)]<-WQP_TS #for SFe

# Plot residuals
hist(fit$residuals)
qqnorm(fit$residuals, pch = 1, frame = FALSE)
qqline(fit$residuals, col = "steelblue", lwd = 2)


# Now plot the results
png("MUX_TFe_Oct_Nov_2020_pred.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_path(data=TS_conc, aes(x=DateTime,y=TFe_mgl, color= Depth), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=Date, y=TFe_mgl, colour= as.factor(Depth))) +
  ylim(0, 5)+
  labs(x="Date", y = "TFe_mgl", title = "Predicted TFe") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="right")
TFe_plot
dev.off()

#plot a single depth
nine_m = TS_conc %>% filter(Depth == '9.0')
nine_m_WQ = dataWQ %>% filter(Depth == 9.0)

five_m = TS_conc %>% filter(Depth == '5.0')
five_m_WQ = dataWQ %>% filter(Depth == 5.0)

one_m = TS_conc %>% filter(Depth == '1.6')
one_m_WQ = dataWQ %>% filter(Depth == 1.6)

png("MUX_TFe_2020_pred_five_m.png",width = 9, height = 4, units = 'in', res = 300) 
TFe_plot <- ggplot() +
  geom_point(data=nine_m, aes(x=DateTime,y=TFe_mgl), size=0.5) +
  geom_ribbon(data=nine_m, aes(ymin=uncerTFe_min, ymax=uncerTFe_max, x=DateTime, fill = "band"), alpha = 0.2)+
  geom_point(data=nine_m_WQ, aes(x=Date, y=TFe_mgl), colour="blue") +
  ylim(0, 7)+
  labs(x="Date", y = "TFe_mgl", title = "Predicted TFe at 9.0m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
TFe_plot
dev.off()

#Calculate RMSE
RMSE_run <-sqrt(mean((WQ-WQP$`Pfit[1:length(Pfit)]` )^2))
RMSE_run

#Calculate R2 of pred vs obs
R2_run <- lm(WQP$`Pfit[1:length(Pfit)]`~WQ)
summary(R2_run)

#write csv of predictions
write.csv(TS_conc,"MUX_Oct_Nov_2020_predictions_111620.csv")
