
#*****************************************************************
#* TITLE:   MUX Data Prep and Overlaps Script
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 11/07/2022
#*                                    
#* NOTES:  This script combines all MUX FP files from 2020 and 2021, plots data for QA/QC, 
#*         matches WQ sampling data to FP data, and creates data files for MUX FP
#*         Overlaps (for PLSR calibration) plus the entire FP time series (for prediction 
#*         w/ fitted PLSR models).
#*         
#*        The WQ sampling times do not always match up perfectly with a MUX FP
#*        measurement time. This is because we collected grab samples using the 
#*        van dorn sampler, not from the MUX itself. We tried to time our grab sampling
#*        to match that of the MUX, but there are cases when the closest FP measurement
#*        is up to ~ 1 hr off from the sampling time in 2020 and up to ~ 4 hr off from the sampling time in 2021.
#*****************************************************************


#### Load Packages ####

library(lubridate)
library(tidyverse)

#set working directory
setwd('./')


#### Load in 2020 MUX FP files from GitHub ####

# Create a character string of all MUX FP file names 
muxfiles<-list.files(path="./MagicData/MUX/FP File/2020", pattern = ".FP")

# Prepare data frame w/column headers 
mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5)))), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(NA,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="America/New_York")

#Read in all files within folder in Github
for(i in 1:length(muxfiles)){
  if(file.size(paste0("./MagicData/MUX/FP File/2020/",muxfiles[i]))>4000){
    temp<-read.table(file=paste0("./MagicData/MUX/FP File/2020/",muxfiles[i]),skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="America/New_York")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}


# Data before 2020-06-16 13:39 is messy
# Just subsetting to data after Oct 1st, since that encompasses the Turnover Deployment
# Joyful Beginning: "2020-10-15 12:00"
mux_only=obs2[obs2$DateTime>c("2020-10-01 12:00"),]
mux_only=mux_only[order(mux_only$DateTime),]


# Convert valve # to depth
Valves = as.data.frame(mux_only$Valve)
colnames(Valves)=c("Valve")
Valve_depth <- data.frame(
  Valve = c (1:12), 
  Depth= c("0.1","1.6","3.8","5","6.2", "8", "9", "NA", "acid_r", "air","NA", "air"),
  stringsAsFactors = FALSE
)
Valves = Valves %>% 
  left_join(Valve_depth, by="Valve") %>% 
  select(Depth)
mux_only = cbind(mux_only,Valves)


# Create a long-format dataframe for plotting
mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")

# Plot data (every 10nm at each depth) to check for completeness and any potential
# errors in data prep
ggplot(data = filter(mux_only_long, wavelength %in% c(seq(200,700,10)))) +
  geom_point(aes(x = DateTime, y = absorbance, color = as.numeric(wavelength))) +
  facet_wrap(~Depth, nrow = 4)


# Plot wavelength vs. absorption over time for a single depth 
# This can be done for any depth -- just change Depth == "9" to the desired depth
# Visually check for outliers or anomalous patterns in the absorption spectrum
ggplot() +
  geom_point(data = filter(mux_only_long, Depth == "9" & DateTime > as.POSIXct("2020-10-28 12:00")), 
             aes(x=as.numeric(wavelength),y=absorbance, colour = DateTime)) 


#### Create new df with final dataset ####
MUX20 = mux_only_long

#### Load in 2020 WQ data (entire dataset in csv format) ####

# Specify directory and file name for data file
pathWQ = "./MagicData/MUX/Modeling Files/"
WQ = "Metals_2014_2021.csv"
#Download EDI metals dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/455/6/57912981e3e857c85924484806492446" 
infile1 <- c(paste0(pathWQ,WQ,sep=""))
download.file(inUrl1,infile1,method="curl")

# Read in dataframe and convert DateTime and Depth variables to proper format
dataWQ<-read_csv(paste(pathWQ,WQ,sep="")) #Import data as .csv file
dataWQ$DateTime <- ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ$Depth_m <- as.character(dataWQ$Depth_m)

# Filter dataWQ to just the necessary data
dataWQ = dataWQ %>% filter(Reservoir == "FCR" & Site == 50) %>% 
  filter(DateTime > "2020-10-16 12:00:00") %>% 
  filter(DateTime < "2020-11-11 12:00:00") %>% 
  select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)


#### Match WQ times with MUX times to find the reading closest to the sampling time ####
WQtimes <- dataWQ %>% select(DateTime,Depth_m)

# Find the FP reading that is closest in time to each WQ data point (at each depth)
df.final<-mux_only %>% filter(Depth == WQtimes$Depth_m[1]) %>% 
  mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1]))  %>% 
  slice_min(abs(time_diff))
for (i in 2:nrow(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- mux_only %>% filter(Depth == WQtimes$Depth_m[i]) %>% 
    mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))  %>% 
    slice_min(abs(time_diff))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2020 = df.final

# Plot to check that the overlaps correspond to WQ sampling times
# The values should fall along the 1:1 line
plot(WQtimes$DateTime[which(WQtimes$Depth_m=="9")],MUX_FP_Overlaps_2020$DateTime[which(MUX_FP_Overlaps_2020$Depth =="9")],
     xlab = "Sampling Times", ylab = "MUX times")
abline(a=0,b=1)

# What's the time difference between the sampling time and MUX FP measurement time?
plot(MUX_FP_Overlaps_2020$DateTime, MUX_FP_Overlaps_2020$time_diff/3600,
     xlab = "DateTime", ylab = "time difference (hr)")
abline(a=0,b=0)

# What's the max time difference, out of all overlaps pairs?
max(MUX_FP_Overlaps_2020$time_diff)/60/60

# Remove Depth and time_diff columns 
MUX_FP_Overlaps_2020 = MUX_FP_Overlaps_2020 %>% select(-c(Depth,time_diff))


#Write Overlaps to csv
write.csv(MUX_FP_Overlaps_2020, file = "./MagicData/MUX/Modeling Files/MUX_FP_Overlaps_2020.csv")


# Clean up 'MUX' before writing to csv
# Remove unnecessary columns and filter out NA's
MUX20 = MUX20 %>% select(-c(Status,`Measurement time`)) %>% 
  filter(!is.na(DateTime))

# Final plot to check (every 10nm at each depth)
# before writing to csv
ggplot(data = filter(MUX20, wavelength %in% c(seq(200,700,100)))) +
  geom_point(aes(x = DateTime, y = absorbance, color = as.numeric(wavelength))) +
  facet_wrap(~Depth, nrow = 4)

# Write full time series of MUX FP data to csv
write.csv(MUX20, file = "./MagicData/MUX/Modeling Files/MUX_FP_TS_2020.csv", row.names = F)





#### Load in 2021 FP files from GitHub for the MUX ####

# Create a character string of all MUX FP file names
muxfiles<-list.files(path="./MagicData/MUX/FP File/2021", pattern = ".FP")

# Prepare data frame w/column headers
mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5)))), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(NA,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="America/New_York")

#Read in all files within folder in Github
for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(paste0("./MagicData/MUX/FP File/2021/",muxfiles[i]))>4000){
    temp<-read.table(file=paste0("./MagicData/MUX/FP File/2021/",muxfiles[i]),skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="America/New_York")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}


# Fix timestamp at 05-27-2021 23:43:29 (It didn't get recorded, for some reason)
obs2[207,1] = mdy_hms("05-27-2021 23:43:29",tz="America/New_York")


# Ensure data are in chronological order
# Subset to date range of MUX Oxygen On Deployment 
mux_only=obs2[obs2$DateTime>c("2021-05-01 13:39"),] 
mux_only=mux_only[order(mux_only$DateTime),]


# Convert valve # to depth
Valves = as.data.frame(mux_only$Valve)
colnames(Valves)=c("Valve")
Valve_depth <- data.frame(
  Valve = c (1:12), 
  Depth= c("0.1","1.6","3.8","5","6.2", "8", "9", "NA", "acid_r", "air","NA", "water"),
  stringsAsFactors = FALSE
)
Valves = Valves %>% 
  left_join(Valve_depth, by="Valve") %>% 
  select(Depth)
mux_only = cbind(mux_only,Valves)


# Create a long-format dataframe for plotting
mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")

# Plot data (every 10nm at each depth) to check for completeness and any potential
# errors in data prep
ggplot(data = filter(mux_only_long, wavelength %in% c(seq(200,700,10)))) +
  geom_point(aes(x = DateTime, y = absorbance, color = as.numeric(wavelength))) +
  facet_wrap(~Depth, nrow = 4, scales = "free_y")


# Plot wavelength vs. absorption over time for a single depth 
# This can be done for any depth -- just change Depth == "9" to the desired depth
# Visually check for outliers or anomalous patterns in the absorption spectrum
ggplot() +
  geom_point(data = filter(mux_only_long, Depth == "9" & DateTime > as.POSIXct("2021-06-10 12:00")), 
             aes(x=as.numeric(wavelength),y=absorbance, colour = DateTime)) 



#### Create new df with final dataset ####
MUX21 = mux_only_long

#### Load in 2021 WQ data (entire dataset in csv format) ####

# Specify directory and file name for data file
pathWQ = "./MagicData/MUX/Modeling Files/"
WQ = "Metals_2014_2021.csv"
#Download EDI metals dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/455/6/57912981e3e857c85924484806492446" 
infile1 <- c(paste0(pathWQ,WQ,sep=""))
download.file(inUrl1,infile1,method="curl")

# Read in dataframe and convert DateTime and Depth variables to proper format
dataWQ<-read_csv(paste(pathWQ,WQ,sep="")) #Import data as .csv file
dataWQ$DateTime <- ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ$Depth_m <- as.character(dataWQ$Depth_m)


# Filter dataWQ to just the necessary data
dataWQ = dataWQ %>% filter(Reservoir == "FCR" & Site == 50) %>% 
  filter(DateTime > "2021-05-26 00:00:00") %>% 
  filter(DateTime < "2021-06-22 00:00:00") %>% 
  select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)



#### Match WQ times with MUX times to find the reading closest to the sampling time ####

WQtimes <- dataWQ %>% select(DateTime,Depth_m)

#Subset date range to Oxygen On Deployment
WQtimes <- WQtimes[WQtimes$DateTime>"2021-05-26 00:00:00",]
WQtimes <- WQtimes %>% mutate(date = date(DateTime))

# Remove samples from 06-02-2021 because the MUX was not deployed during that time
WQtimes <- WQtimes %>% filter(date(DateTime) != "2021-06-02")

# Find the FP reading that is closest in time to each WQ data point (at each depth)
df.final<-mux_only %>% filter(Depth == WQtimes$Depth_m[1]) %>% 
  mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1]))  %>% 
  slice_min(abs(time_diff))
for (i in 2:nrow(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- mux_only %>% filter(Depth == WQtimes$Depth_m[i]) %>% 
    mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))  %>% 
    slice_min(abs(time_diff))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2021 = df.final

# Plot to check that the overlaps correspond to WQ sampling times
# The values should fall along the 1:1 line
plot(WQtimes$DateTime[which(WQtimes$Depth_m=="9")],MUX_FP_Overlaps_2021$DateTime[which(MUX_FP_Overlaps_2021$Depth =="9")],
     xlab = "Sampling Times", ylab = "MUX times")
abline(a=0,b=1)

# What's the time difference between the sampling time and MUX FP measurement time?
plot(MUX_FP_Overlaps_2021$DateTime, MUX_FP_Overlaps_2021$time_diff/3600,
     xlab = "DateTime", ylab = "time difference (hr)")
abline(a=0,b=0)

# What's the max time difference, out of all overlaps pairs?
max(MUX_FP_Overlaps_2021$time_diff)/60/60

# Remove Depth and time_diff columns 
MUX_FP_Overlaps_2021 = MUX_FP_Overlaps_2021 %>% select(-c(Depth,time_diff))


#Write to csv
write.csv(MUX_FP_Overlaps_2021, file = "./MagicData/MUX/Modeling Files/MUX_FP_Overlaps_2021.csv")



# Clean up 'MUX' before writing to csv
# Remove unnecessary columns and filter out NA's
MUX21 = MUX21 %>% select(-c(Status,`Measurement time`)) %>% 
  filter(!is.na(DateTime))

# Final plot to check (every 10nm at each depth)
# before writing to csv
ggplot(data = filter(MUX21, wavelength %in% c(seq(200,700,100)))) +
  geom_point(aes(x = DateTime, y = absorbance, color = as.numeric(wavelength))) +
  facet_wrap(~Depth, nrow = 4, scales = "free_y")

# Write full time series of MUX FP data to csv
write.csv(MUX21, file = "./MagicData/MUX/Modeling Files/MUX_FP_TS_2021.csv", row.names = F)


# Combine both years 
MUX = bind_rows(MUX20,MUX21)

# write full dataset to csv
write.csv(MUX, file = "./MagicData/MUX/Modeling Files/MUX_FP_TS.csv", row.names = F)

