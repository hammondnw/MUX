################################################################################
#PLSR model for 2020 SSCAN data using 2019 regression
#Rachel Corrigan
#August 2020
################################################################################
# This code is for running a PLSR for the 2020 SCAN data. This can be adapted for any 
# depth or combination of depths, or years. All that you need is the grab sample water
# quality data, overlapping mux or scan data with the WQ data, and the mux/scan data for
# the full timeseries that you want chem predictions for. You will just alter the files
# that you import. 

#First, load in all required packages

library(pls)  #Load the pls package
library(lubridate)
library(dplyr)
library(stringr)
library(scales)

#Set up your data locations and working directory
pathD<-"/Users/rachelcorrigan/Dropbox/fcr/MagicData/" #Specify folder where data is located
setwd("/Users/rachelcorrigan/Dropbox/fcr/MagicData/")

################################################################################
#Read in FCR WQ data
################################################################################
#The format of the water chem data from the lab is in correct format. 
#This steps are just cleaning and removing some extra columns (not necessary, just me), and 
#assigning column names. You could merge the metals data with this data, and just add the 
#column names. Thats what I did for 2018 analysis.
# The WQ data used in this analysis are from 2019. Update this to 2020 when that becomes available.
# This has been subset to 1.6m.

WQ<-"onedepth2019WQoverlaps.csv" #Specify file where data is located  
dataWQ<-read.table(file=paste(pathD,WQ,sep=""),sep=",",header = TRUE)  #Import data as .csv file
dataWQ=dataWQ[,-c(16:24)]
dataWQ=dataWQ[,-c(1:4)]
WQparam <- c("TN_ugL","TP_ugL","NH4_ugL","NO3NO2_ugL","SRP_ugL","DOC_mgL","DIC_mgL", "DC_mgL", "DN_mgL")   


################################################################################
#### Reading of  FingerPrint (FP) file corresponding to lab concentrations for 
#### calibration hence dataCalFP
#### 
#This step reads in the file of overlapping MUX/SCAN data and field data. Again, 
#this is from 2019, and is the MUX overlap with 2019 wq data at 1.6m.

FPcaldata_name<-"onedepth2019FPoverlaps.csv"
dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
colnames(dataCalFP)<-c("ID","Date/Time","status",seq(200,730,2.5)) #Add column names

#subset if doing depth specific model (i.e., 0.1 and 1.6, 3-6, 8-9, etc.)
#dataCalFP <- dataCalFP %>%
#  filter(ID == 1 | ID == 2 | ID == 3) #%>%
#  filter(ID == 2) %>%
#  filter(ID == 3)
#
#
timesCalFP<-dataCalFP[,1:2] 
dataCalFP<-dataCalFP[,-c(1:3)]
dataCalFP<-dataCalFP[,-214:-216] #Remove NO3-N values and NAs at high wavelengths
dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix

################################################################################
#### This replaces the ID and Date from the original dataWQ with the exact values
#### from the SCAN so that manual values can be plotted later on in the TS plots
dataWQ$ID<-timesCalFP[,1]
dataWQ$DateTime<-timesCalFP[,2]



################################################################################
#### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) 
#### 
# This is the 2020 SCAN data. 

TimeSeriesFP_name<-"SSCAN_DATA_08282020.csv"
TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",", skip=1)  #Import Time Series data as .csv file
TS_FP<-TS_FP[,-c(217:225)]
TS_FP<-TS_FP[,-1]

colnames(TS_FP)<-c("Date","status",seq(200,730,2.5)) #Add column names
TS_FP$Date = as.POSIXct(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S")

#if doing depth specific
#TS_FP <- TS_FP %>%
#  filter(port != 10 | port !=11 | port !=12) #%>%
#  filter(port !=11) %>%
#  filter(port !=12)

Dat<-strptime(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time

################################################################################
####  Create matrix to store calculated concentrationss:TS_conc
TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],11))  #Create data frame for date/time and predicted NO3-N values
TS_conc[,1]<-1.6
TS_conc[,2]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
colnames(TS_conc)<-c("Depth", "DateTime",WQparam) #Add column names
TS_FP<-TS_FP[,(-1:-2)]
TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix


#######################
#Specify number of components for wq param
ncomp=9 #7 for TP and TN

################################################################################
#### function which does the calibration and then calculates conc from the TS for
#### a given chemical parameter (param).  It does the calibration for a given 
#### number of components (ncomp)
################################################################################

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
 


################################################################################
#### Example for running the function for one parameter only
################################################################################

param<-"NO3NO2_ugL"
ncomp=10
PLSR_SCAN(param,dataCalFP,dataWQ,TS_FP,ncomp, yesplot=TRUE)


##################
# Using the PLSR model to identify the correct number of components for each nutrient based on the RMSE
# Using code addapted from CCC original PLSR script
##################

ncomps = c(1:10)
param="NH4_ugL" #the nutrient you want to find the number of comps for 
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



#############################
#If there are obvious outliers, run whats in the PLSR loop, then click on the points.
#This will give you a location of which datapoints are outliers, and you can then 
#remove them from the WQ and dataCalFP dataframes. 

out <- sapply(list(WQ,as.matrix(WQP)),"[",identify(WQ,as.matrix(WQP)))
out
############################
############################
# If you are getting negative predictions, try taking the log10 of WQ (field data) on this line:
# fit<-plsr(WQ~data.matrix(dataFP),ncomp=ncomp,validation="CV")  #PLSR model to predict param with cross validation
# so that it is:
# fit<-plsr(log10(WQ)~data.matrix(dataFP),ncomp=ncomp,validation="CV")  #PLSR model to predict param with cross validation
# This has helped me resolve issues with negative predictions.
# Then you'll need to do "TS_conc[,()]<-10^(WQP_TS)" to convert the predictions.


# Make sure that your datetimes are formatted correctly before plotting
TS_conc$DateTime <- as.POSIXct(TS_conc$DateTime, format="%Y-%m-%d %H:%M:%S")
dataWQ$DateTime <- as.POSIXct(dataWQ$DateTime, format="%m/%d/%y %H:%M")
colnames(dataWQ)[2] <- "Depth"   #rename this column for plotting

############################
#If you are running a model with more than one depths, assign
#depths to ports, if depths are not already assigned.
TS_conc$Depth = NA
TS_conc$Depth[TS_conc$port == 1] = 0.1
TS_conc$Depth[TS_conc$port == 2] = 1.6
TS_conc$Depth[TS_conc$port == 3] = 3.8
TS_conc$Depth[TS_conc$port == 4] = 5.0
TS_conc$Depth[TS_conc$port == 5] = 6.2
TS_conc$Depth[TS_conc$port == 6] = 8.0
TS_conc$Depth[TS_conc$port == 7] = 9.0

############################
#Run what is inside the PLSR loop, and then do these steps to calc uncertainty and
#assign the predictions to the correct column in the TS_conc matrix.
TS_conc$uncerNO2_max <- NA
TS_conc$uncerNO2_min <- NA
TS_conc$uncerNO2_max <- WQP_TS + 1.96*sd(as.numeric(fit$residuals[,,10])) #max uncert
TS_conc$uncerNO2_min <- WQP_TS - 1.96*sd(as.numeric(fit$residuals[,,10])) #min uncert
TS_conc$uncerNO2_max <- unlist(TS_conc$uncerNO2_max)
TS_conc$uncerNO2_min <- unlist(TS_conc$uncerNO2_min)

sd(as.numeric(fit$residuals[,,9])) #check uncertainty
#TS_conc[,(7)]<-10^(WQP_TS) #for SRP
#TS_conc[,(8)]<-10^(WQP_TS) #for DOC
#TS_conc[,(5)]<-WQP_TS #for NH4
TS_conc[,(6)]<-WQP_TS #for N03N02
#TS_conc[,(9)]<-WQP_TS #for DIC
#TS_conc[,(10)]<-10^WQP_TS #for DC
#TS_conc[,(11)]<-10^WQP_TS #for DN

### Now plot your results
NO3_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=NO3NO2_ugL), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerNO3_min, ymax=uncerNO3_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=NO3NO2_ugL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("NO3NO2 (", mu, "g/L)")), title = "Predicted NO3NO2 at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
NO3_test



########################################
########################################
#Most of this below here is just me messing around with plotting, but this code may be useful.

srp_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=SRP_ugL), size=0.5, alpha=0.5) +
  # scale_colour_manual(name='', values=c('Predict'='black', 'Observe'='blue'), guide=guide_legend(),
  #                     labels=c("Predict", "Observe"))+
  #geom_ribbon(data=TS_conc, aes(ymin=uncerSRP_min, ymax=uncerSRP_max, x=DateTime, fill = "band"), alpha = 0.5)+
  #scale_fill_identity(name = '', labels = c('uncert'), guide=guide_legend()) +
  #geom_point(data=dataWQ, aes(x=DateTime, y=SRP_ugL), colour="blue") +
  #scale_shape_manual(values=c(16),guide=guide_legend(override.aes=list(colour="blue",size=c(2))))+
  #ylim(-5, 0)+
  labs(x="Date", y = expression(paste("SRP (", mu, "g/L)")), title = "Predicted SRP at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
srp_test


#onedept_srp <- TS_conc
#onedept_doc <- TS_conc
#onedoc_test <- doc_test
doc_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=DOC_mgL), size = 0.5, alpha=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDOC_min, ymax=uncerDOC_max, x=DateTime, fill = "band"), alpha = 0.5)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DOC_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("DOC mg/L)")), title = "Predicted DOC from 1.6m SCAN, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
doc_test

NH4_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=NH4_ugL), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerNH4_min, ymax=uncerNH4_max, x=DateTime, fill = "band"), alpha = 0.5)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=NH4_ugL), colour="blue") +
  ylim(-5, 80)+
  labs(x="Date", y = expression(paste("NH4 (", mu, "g/L)")), title = "Predicted NH4 at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
NH4_test

NO3_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=NO3NO2_ugL), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerNO3_min, ymax=uncerNO3_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=NO3NO2_ugL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("NO3NO2 (", mu, "g/L)")), title = "Predicted NO3NO2 at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
NO3_test


DIC_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=DIC_mgL), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDIC_min, ymax=uncerDIC_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DIC_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("DIC mg/L)")), title = "Predicted vs. Obs DIC at 1.6m, 2019") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
DIC_test

DC_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=DC_mgL), size=0.5, alpha=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDC_min, ymax=uncerDC_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DC_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("DC mg/L)")), title = "Predicted DC at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
DC_test

DN_test <- ggplot() +
  geom_point(data=TS_conc, aes(x=DateTime,y=DN_mgL), size=0.5, alpha=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("DN mg/L)")), title = "Predicted DN at 1.6m, 2020") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
DN_test

#plot over just two days to see finer scale variation
srp_test_2day <- ggplot() +
  geom_line(data=subset(TS_conc, TS_conc$DateTime >= "2018-10-07 05:19:59" & TS_conc$DateTime <= "2018-10-16 05:19:59"),
            aes(x=DateTime,y=SRP_ugL, group=factor(Depth))) +
  geom_ribbon(data=subset(TS_conc, TS_conc$DateTime >= "2018-10-07 05:19:59" & TS_conc$DateTime <= "2018-10-16 05:19:59"), 
              aes(ymin=uncerSRP_min, ymax=uncerSRP_max, x=DateTime, fill = "band"), alpha = 0.3)+
  geom_point(data=subset(dataWQ, dataWQ$DateTime >= "2018-10-07 09:00:00" & dataWQ$DateTime <= "2018-10-16 09:00:00"), 
             aes(x=DateTime, y=SRP_ugL), colour="blue") +
  labs(x="Date", y = expression(paste("SRP (", mu, "g/L)"))) +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  facet_wrap(~Depth, ncol=1) +
  theme(legend.position="none")
ggsave("~/srp_test_2day.pdf", width = 7, height = 12)

nh4_test_2day <- ggplot() +
  geom_point(data=subset(TS_conc, TS_conc$DateTime >= "2020-05-01" & TS_conc$DateTime <= "2020-05-07"),
            aes(x=DateTime,y=NH4_ugL), colour="blue") +
  #geom_ribbon(data=subset(TS_conc, TS_conc$DateTime >= "2018-10-07 05:19:59" & TS_conc$DateTime <= "2018-10-16 05:19:59"), 
             # aes(ymin=uncerNH4_min, ymax=uncenh4_max, x=DateTime, fill = "band"), alpha = 0.3)+
  #geom_point(data=subset(dataWQ, dataWQ$DateTime >= "2018-10-07 09:00:00" & dataWQ$DateTime <= "2018-10-16 09:00:00"), 
            # aes(x=DateTime, y=NH4_ugL), colour="blue") +
  labs(x="Date", y = expression(paste("NH4 (", mu, "g/L)"))) +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  #facet_wrap(~Depth, ncol=1) +
  theme(legend.position="none")
ggsave("~/srp_test_2day.pdf", width = 7, height = 12)
nh4_test_2day

no3_test_2day <- ggplot() +
  geom_point(data=subset(TS_conc, TS_conc$DateTime >= "2020-05-01" & TS_conc$DateTime <= "2020-05-07"),
             aes(x=DateTime,y=NO3NO2_ugL),colour='blue') +
  #geom_ribbon(data=subset(TS_conc, TS_conc$DateTime >= "2018-10-07 05:19:59" & TS_conc$DateTime <= "2018-10-16 05:19:59"), 
  # aes(ymin=uncerNH4_min, ymax=uncenh4_max, x=DateTime, fill = "band"), alpha = 0.3)+
  #geom_point(data=subset(dataWQ, dataWQ$DateTime >= "2018-10-07 09:00:00" & dataWQ$DateTime <= "2018-10-16 09:00:00"), 
  # aes(x=DateTime, y=NH4_ugL), colour="blue") +
  labs(x="Date", y = expression(paste("NO3NO2 (", mu, "g/L)"))) +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  #facet_wrap(~Depth, ncol=1) +
  theme(legend.position="none")
no3_test_2day

####################
#making multiplanel plots

twoall_nit <- ggarrange(nh4_test_2day, no3_test_2day, ncol=1, nrow=2)
twoall_nit

twoall_doc <- ggarrange(two_top_doc_test,two_bot_doc_test, ncol=1, nrow=2)
twoall_doc
ggsave("./DOC_1wkpredict_2020.pdf")
all_doc <- ggarrange(top_doc_test,bot_doc_test, ncol=1, nrow=2)
all_doc
ggsave("./DOC_muxandscan_2020.pdf")

twoall_srp <- ggarrange(two_top_srp_test,two_bot_srp_test, ncol=1, nrow=2)
twoall_srp
ggsave("./SRP_1wkpredict_2020.pdf")
srpdoc_1wk <- ggarrange(twoall_doc, twoall_srp, ncol=2, nrow=1)
srpdoc_1wk
ggsave("./SRPDOC_1wkpredict_2020.pdf")

all_srp <- ggarrange(top_srp_test, bot_srp_test, ncol=1, nrow=2)
all_srp
ggsave("./SRP_muxvscan_2020.pdf")





write.csv(TS_conc, "TS_PREDICTIONS_2020_16m.csv")
TS_conc_chem <- TS_conc[,-12:-23]
write.csv(TS_conc_chem, "TS_PREDICTIONS2019_16m_chem_only.csv")
TS_conc_chem$Reservoir<- "FCR"
TS_conc_chem$Site <- "50"
TS_conc_chem$Depth <- 1.6
TS_conc_chem <- TS_conc
TS_format <- TS_conc_chem[,c(12,13,2,1,5:8, 10, 11)]
write.csv(TS_format, "TS_PREDICTIONS2020_FORMATTED.csv")

chem20_plot <- ggarrange(doc_test, srp_test, NH4_test, NO3_test,ncol=2, nrow=3)
chem20_plot
all_chem_plot <- ggarrange(doc_test, srp_test, NH4_test, NO3_test, DC_test, DN_test, ncol=2, nrow=3)
all_chem_plot

predict_19 <- read.csv("TS_PREDICTIONS2019_FORMATTED.csv")
predict_19$DateTime<- as.POSIXct(predict_19$DateTime, format="%Y-%m-%d %H:%M:%S")


###aggregating to hourly just for curiosity. 


split_hour19 = cut(as.POSIXct(predict_19$DateTime), breaks = "60 mins") # summrise given mins
predict_19$hour = split_hour19 # make hourly vaiable
ag19 = aggregate(. ~ hour, predict_19, mean)

predict_20 <- read.csv("TS_PREDICTIONS2020_FORMATTED.csv")
predict_20$DateTime<- as.POSIXct(predict_20$DateTime, format="%Y-%m-%d %H:%M:%S")

split_hour20 = cut(as.POSIXct(predict_20$DateTime), breaks = "60 mins") # summrise given mins
predict_20$hour = split_hour20 # make hourly vaiable
ag20 = aggregate(. ~ hour, predict_20, mean)

ag19<-ag19[,c(-2,-5)]
ag19<-ag19[,c(2,3,1,4,5:11)]
colnames(ag19)[3]<-"DateTime"
write.csv(ag19, "TS19_hourly_predict_formatted.csv")

ag20<-ag20[,c(-2,-5)]
ag20<-ag20[,c(2,3,1,4,5:10)]
colnames(ag20)[3]<-"DateTime"
write.csv(ag20, "TS20_hourly_predict_formatted.csv")

#mean_19<- aggregate(predict_19["NH4_ugL", "NO3NO2_ugL", "SRP_ugL", "DOC_mgL", "DC_mgL", "DN_mgL"], 
 #         list(hour=cut(as.POSIXct(predict_19$DateTime), "hour")),
  #        mean)



