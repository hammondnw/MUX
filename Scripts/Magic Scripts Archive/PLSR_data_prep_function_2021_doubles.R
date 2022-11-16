###
#### Function for prepping data for PLSR ####
#### with fouling corrections ###


data_prep = function(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam){
  
  #### Read in FCR WQ data ####
  dataWQ <- read_csv(paste(pathD,WQ_name,sep=""))
  
  #Subset to just include desired depths, Reservoir, and Site
  # Remove 'Flag' columns
  dataWQ <- dataWQ %>%
    filter(Depth_m %in% as.numeric(Depths)) %>%
    select(-c("...1"))
  
  #Subset to desired date range
  dataWQ = dataWQ[dataWQ$DateTime>Begin_time,]
  dataWQ = dataWQ[dataWQ$DateTime<End_time,]
  
  # Remove samples from 06-02-2021 because the MUX was not deployed during that time
  dataWQ = dataWQ %>% filter(date(DateTime) != "2021-06-02")
  
  #### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
  # This step reads in the file of overlapping MUX/SCAN data and field data. 
  dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
  colnames(dataCalFP)<-c("ID","DateTime","status",seq(200,750,2.5),"Valve", "Measurement Time") #Add column names
  
  
  # Filter out the air valve measurements (valve #7)
  dataCalFP <- dataCalFP %>%
    filter(Valve!=9 | Valve!=10)
  
  # Subset to just include desired depths
  # First we need to convert valve # to depth
  Valves = as.data.frame(dataCalFP$Valve)
  colnames(Valves)=c("valve")
  valve_depth <- data.frame(
    valve = c (1:7), 
    Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
    stringsAsFactors = FALSE
  )
  Valves = Valves %>% 
    left_join(valve_depth, by="valve") %>% 
    select(Depth)
  dataCalFP = cbind(dataCalFP,Valves)
  # Now we can subset based on depth
  dataCalFP <- dataCalFP %>%
    filter(as.numeric(Depth) %in% as.numeric(Depths))
  #remove 'Depth' column
  dataCalFP <- dataCalFP %>% 
    select(!c(Depth))
  
  #Subset to desired date range
  dataCalFP = dataCalFP[dataCalFP$DateTime>Begin_time,]
  dataCalFP = dataCalFP[dataCalFP$DateTime<End_time,]
  
  #Create vector of times and ID from the FP file
  timesCalFP<-dataCalFP %>% select(ID,`DateTime`)
  
  dataCalFP <- dataCalFP %>% select(!c(ID,`DateTime`,status)) #Remove ID, DateTime, and status columns
  dataCalFP<- dataCalFP %>% select(!c(`735`:`750`)) #Remove NAs at high wavelengths
  dataCalFP<- dataCalFP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
  dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix
  
  #This replaces the ID and Date from the original dataWQ with the exact values
  #from the SCAN so that manual values can be plotted later on in the TS plots
  
  dataWQ$ID<-timesCalFP[,1]
  dataWQ$DateTime<- ymd_hms(timesCalFP[,2], tz="America/New_York")
  
  
  #### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) ####
  # This is the 2020 SCAN data 
  TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",", skip=1)  #Import Time Series data as .csv file
  colnames(TS_FP)<-c("ID","DateTime","status",seq(200,750,2.5), "Valve", "Measurement Time","Depth") #Add column names
  
  # Filter out the air valve measurements (valve #7)
  TS_FP <- TS_FP %>%
    filter(Valve!=9 | Valve!=10)
  
  # Remove 'Depth' Column - we'll add this back in later
  TS_FP = TS_FP %>% select(-c("Depth"))
  
  # Subset to just include desired depths
  # First we need to convert valve # to depth
  Valves = as.data.frame(TS_FP$Valve)
  colnames(Valves)=c("valve")
  valve_depth <- data.frame(
    valve = c (1:7), 
    Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
    stringsAsFactors = FALSE
  )
  Valves = Valves %>% 
    left_join(valve_depth, by="valve") %>% 
    select(Depth)
  TS_FP = cbind(TS_FP,Valves)
  # Now we can subset based on depth
  TS_FP <- TS_FP %>%
    filter(as.numeric(Depth) %in% as.numeric(Depths))
  #remove 'Depth' column
  TS_FP <- TS_FP %>% 
    select(!c(Depth))
  
  #Subset to desired date range
  TS_FP = TS_FP[TS_FP$DateTime>Begin_time,]
  TS_FP = TS_FP[TS_FP$DateTime<End_time,]
  
  TS_FP<- TS_FP %>% select(!c(`735`:`750`)) #remove columns for wavelengths above 735nm because those are all NA
  TS_FP<- TS_FP %>% select(!c(ID)) #remove ID column
  
  TS_FP$DateTime = as.POSIXct(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S")
  Dat<-strptime(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time
  
  #### Create a dataframe to store outputs (TS_conc) ####
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
  
  #Create matrix to store calculated concentrationss:TS_conc 
  TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],6))  #Create data frame for DateTime and predicted values
  TS_conc[,1]<- Depth_lab
  TS_conc[,2]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
  colnames(TS_conc)<-c("Depth", "DateTime",WQparam) #Add column names
  
  # Finish cleaning up time series dataframe
  TS_FP<- TS_FP %>% select(!c(DateTime,status)) #remove 'Date' and 'status' columns
  TS_FP<- TS_FP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
  TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix
  
  assign("dataCalFP",dataCalFP,env=.GlobalEnv)
  assign("dataWQ",dataWQ,env=.GlobalEnv)
  assign("TS_FP",TS_FP,env=.GlobalEnv)
  assign("TS_conc",TS_conc,env=.GlobalEnv)
  
}
