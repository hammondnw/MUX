####
#### Function for prepping 1.6m SCAN data for PLSR ####
####


data_prep = function(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam){
  
  #### Read in FCR WQ data ####
  dataWQ <- read_xlsx(path=paste(pathD,WQ_name,sep=""))
  
  #Subset to just include desired depths
  dataWQ <- dataWQ %>%
    filter(Depth_m %in% as.numeric(Depths))
  
  #Subset to desired date range
  dataWQ = dataWQ[dataWQ$Date>Begin_time,]
  dataWQ = dataWQ[dataWQ$Date<End_time,]
  
  
  #### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
  # This step reads in the file of overlapping MUX/SCAN data and field data. 
  dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
  colnames(dataCalFP)<-c("ID","Date","status",seq(200,750,2.5)) #Add column names
  
  #Subset to desired date range
  dataCalFP = dataCalFP[dataCalFP$Date>Begin_time,]
  dataCalFP = dataCalFP[dataCalFP$Date<End_time,]
  
  #Create vector of times and ID from the FP file
  timesCalFP<-dataCalFP %>% select(ID,`Date`)
  
  dataCalFP <- dataCalFP %>% select(!c(ID,`Date`,status)) #Remove ID, Date, and status columns
  dataCalFP<- dataCalFP %>% select(!c(`732.5`:`750`)) #Remove NAs at high wavelengths
  dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix
  
  #This replaces the ID and Date from the original dataWQ with the exact values
  #from the SCAN so that manual values can be plotted later on in the TS plots
  
  dataWQ$ID<-timesCalFP[,1]
  dataWQ$DateTime<- ymd_hms(timesCalFP[,2], tz="UTC")
  
  
  #### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) ####
  # This is the 2020 SCAN data 
  TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",", skip=1)  #Import Time Series data as .csv file
  colnames(TS_FP)<-c("ID","Date","status",seq(200,750,2.5)) #Add column names
  TS_FP$Date = as.POSIXct(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S",tz='UTC')
  
  #Subset to desired date range
  TS_FP = TS_FP[TS_FP$Date>Begin_time,]
  TS_FP = TS_FP[TS_FP$Date<End_time,]
  
  TS_FP<- TS_FP %>% select(!c(`732.5`:`750`)) #remove columns for wavelengths above 732.5nm because those are all NA
  TS_FP<- TS_FP %>% select(!c(ID)) #remove ID column
  
  
  Dat<-strptime(TS_FP$Date, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time
  
  #### Create a dataframe to store outputs (TS_conc) ####
  
  #Create matrix to store calculated concentrationss:TS_conc 
  TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],5))  #Create data frame for date/time and predicted values
  TS_conc[,1]<- as.character(Dat, "%Y-%m-%d %H:%M:%S")
  colnames(TS_conc)<-c("DateTime",WQparam) #Add column names
  TS_conc$DateTime = as.POSIXct(TS_conc$DateTime,tz='UTC')
  
  # Finish cleaning up time series dataframe
  TS_FP<- TS_FP %>% select(!c(Date,status)) #remove 'Date' and 'status' columns
  TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix
  
  assign("dataCalFP",dataCalFP,env=.GlobalEnv)
  assign("dataWQ",dataWQ,env=.GlobalEnv)
  assign("TS_FP",TS_FP,env=.GlobalEnv)
  assign("TS_conc",TS_conc,env=.GlobalEnv)
  
}
