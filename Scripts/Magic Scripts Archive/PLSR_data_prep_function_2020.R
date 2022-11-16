####
#### Function for prepping 2020 data for PLSR ####
####


data_prep_20 = function(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam){
  
#### Read in FCR WQ data ####
  
  #Download EDI metals dataset
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/455/6/57912981e3e857c85924484806492446" 
  infile1 <- c(paste0(pathD,WQ_name,sep=""))
  download.file(inUrl1,infile1,method="curl")  
  
dataWQ <- read_csv(paste(pathD,WQ_name,sep=""))

dataWQ = dataWQ %>% filter(Reservoir == "FCR" & Site == 50) %>% 
  select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)

#Subset to just include desired depths
dataWQ <- dataWQ %>%
filter(Depth_m %in% as.numeric(Depths))

#Subset to desired date range
dataWQ$DateTime = ymd_hms(dataWQ$DateTime,tz="America/New_York")
dataWQ = dataWQ[dataWQ$DateTime>Begin_time,]
dataWQ = dataWQ[dataWQ$DateTime<End_time,]


#### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
# This step reads in the file of overlapping MUX/SCAN data and field data. 
dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
colnames(dataCalFP)<-c("ID","DateTime","status",seq(200,732.5,2.5),"Valve", "Measurement Time") #Add column names

# Filter out the air valve measurements (valve #10, which was wrongly recorded as valve 12 in 2020)
dataCalFP <- dataCalFP %>%
  filter(Valve!=12)

# Subset to just include desired depths
  # First we need to convert valve # to depth
Valves = as.data.frame(dataCalFP$Valve)
colnames(Valves)=c("valve")
valve_depth <- data.frame(
  valve = c (1:7), 
  Depth_m= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
  stringsAsFactors = FALSE
)
Valves = Valves %>% 
  left_join(valve_depth, by="valve") %>% 
  select(Depth_m)
dataCalFP = cbind(dataCalFP,Valves)
  # Now we can subset based on depth
dataCalFP <- dataCalFP %>%
  filter(as.numeric(Depth_m) %in% as.numeric(Depths))
  #remove 'Depth' column
dataCalFP <- dataCalFP %>% 
  select(!c(Depth_m))

#Subset to desired date range
dataCalFP = dataCalFP[dataCalFP$DateTime>Begin_time,]
dataCalFP = dataCalFP[dataCalFP$DateTime<End_time,]

#Create vector of times and ID from the FP file
timesCalFP<-dataCalFP %>% select(ID,DateTime)

dataCalFP <- dataCalFP %>% select(!c(ID,DateTime,status)) #Remove ID, DateTime, and status columns
dataCalFP<- dataCalFP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix

#This replaces the ID and Date from the original dataWQ with the exact values
#from the SCAN so that manual values can be plotted later on in the TS plots

dataWQ$ID<-timesCalFP[,1]
dataWQ$DateTime<- ymd_hms(timesCalFP[,2], tz="America/New_York")


#### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) ####
# This is the 2020 SCAN data 
TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",",header = T)  #Import Time Series data as .csv file
colnames(TS_FP)<-c("DateTime","Valve","Depth_m","wavelength","absorbance") #Add column names

# Filter out the air valve measurements (valve #7)
TS_FP <- TS_FP %>%
  filter(Valve!=12)

# Subset to just include desired depths

# subset based on depth
TS_FP <- TS_FP %>%
  filter(as.numeric(Depth_m) %in% as.numeric(Depths))
#remove 'Depth' column
TS_FP <- TS_FP %>% 
  select(!c(Depth_m))

#Subset to desired date range
TS_FP = TS_FP[TS_FP$DateTime>Begin_time,]
TS_FP = TS_FP[TS_FP$DateTime<End_time,]

# pivot wider
TS_FP = TS_FP %>% pivot_wider(names_from = wavelength, values_from = absorbance)

TS_FP$DateTime = as.POSIXct(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S")
Dat<-strptime(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time

#### Create a dataframe to store outputs (TS_conc) ####
# Convert Valve number to Depth for the TS_conc matrix
Valves = as.data.frame(TS_FP$Valve)
colnames(Valves)=c("valve")
valve_depth <- data.frame(
  valve = c (1:7), 
  Depth_m= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
  stringsAsFactors = FALSE
)
Valves= Valves %>% 
  left_join(valve_depth, by="valve")

Depth_lab = Valves %>% 
  select(Depth_m)

#Create matrix to store calculated concentrationss:TS_conc 
TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],6))  #Create data frame for date/time and predicted values
TS_conc[,1]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
TS_conc[,2]<- Depth_lab
colnames(TS_conc)<-c("DateTime", "Depth_m",WQparam) #Add column names

# Finish cleaning up time series dataframe
TS_FP<- TS_FP %>% select(!c(DateTime,Valve)) #remove 'Date' and 'status' columns
TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix

assign("dataCalFP",dataCalFP,env=.GlobalEnv)
assign("dataWQ",dataWQ,env=.GlobalEnv)
assign("TS_FP",TS_FP,env=.GlobalEnv)
assign("TS_conc",TS_conc,env=.GlobalEnv)

}
