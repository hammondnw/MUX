
# Script to load in 2021 FP data for the MUX, do a quality check on the data,
# then match WQ data to FP data and write csv files of FP overlaps and the entire FP time series
# Authors: Nick Hammond
# Last Updated: 05/04/2022


#### Load Packages ####

#packages needed
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)
library(readxl)

#set working directory
setwd('C:/Users/hammo/Documents/Magic Sensor PLSR/ManualDownloadsSCCData/MagicData')


#### Load in FP files from GitHub for the MUX ####
# Valve  # Depth
#   1       0.1
#   2       1.6
#   3       3.8
#   4       5.0
#   5       6.2
#   6       8.0
#   7       9.0
#   8       purge
#   9       acid
#   10      air
#   12      DI

# Create a character string of all MUX FP file names
muxfiles<-list.files(path="./FP_2021", pattern = ".FP")

# Prepare data frame w/column headers
mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5)))), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(NA,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="America/New_York")

#Read in all files within folder in Github
for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(paste0("./FP_2021/",muxfiles[i]))>4000){
    temp<-read.table(file=paste0("./FP_2021/",muxfiles[i]),skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="America/New_York")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}


# Fix timestamp at 05-27-2021 23:43:29
obs2[207,1] = mdy_hms("05-27-2021 23:43:29",tz="America/New_York")


# Ensure data are in chronological order
# Subset to date range of MUX Oxygen On Deployment 
# May go back and clean this up later
mux_only=obs2[obs2$DateTime>c("2021-05-01 13:39"),] #change to not default to UTC, should be 14:15 in GMT 4
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

# Plot data
plot(obs2$DateTime,obs2$`255`)


# Create a long-format dataframe for plotting
mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")

# Plot data
ggplot(data = filter(mux_only_long, wavelength %in% c(seq(200,700,10)))) +
  geom_point(aes(x = DateTime, y = absorbance, color = as.numeric(wavelength))) +
  facet_wrap(~Depth, nrow = 4)


# Plot wavelength vs. absorption over time for a single depth 
ggplot() +
  geom_point(data = filter(mux_only_long, Depth == "9" & DateTime > as.POSIXct("2021-06-10 12:00")), 
             aes(x=wavelength,y=absorbance, colour = DateTime)) 



#### Create new df with final dataset ####
MUX = mux_only

#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")
pathWQ = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/"
WQ = "Metals_2014_2021.csv"
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
WQtimes$Depth_m <- as.character(WQtimes$Depth_m)
WQtimes$DateTime <- ymd_hms(WQtimes$DateTime, tz="America/New_York")

#Subset date range to Oxygen On Deployment
WQtimes <- WQtimes[WQtimes$DateTime>"2021-05-26 00:00:00",]
WQtimes <- WQtimes %>% mutate(date = date(DateTime))

# Remove samples from 06-02-2021 because the MUX was not deployed during that time
WQtimes <- WQtimes %>% filter(date(DateTime) != "2021-06-02")


df.final<-MUX %>% filter(Depth == WQtimes$Depth_m[1]) %>% 
  mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1]))  %>% 
  slice_min(abs(time_diff))
for (i in 2:nrow(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- MUX %>% filter(Depth == WQtimes$Depth_m[i]) %>% 
    mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))  %>% 
    slice_min(abs(time_diff))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2021 = df.final

#Plot to check
plot(WQtimes$DateTime[which(WQtimes$Depth_m=="9")],MUX_FP_Overlaps_2021$DateTime[which(MUX_FP_Overlaps_2021$Depth =="9")],
     xlab = "Sampling Times", ylab = "MUX times")
abline(a=0,b=1)

plot(MUX_FP_Overlaps_2021$DateTime, MUX_FP_Overlaps_2021$time_diff/3600,
     xlab = "DateTime", ylab = "time difference (hr)")
abline(a=1,b=0)

max(MUX_FP_Overlaps_2021$time_diff)/60/60

MUX_FP_Overlaps_2021 = MUX_FP_Overlaps_2021 %>% select(-c(Depth,time_diff))


#Write to csv
write.csv(MUX_FP_Overlaps_2021, file = "MUX_FP_Overlaps_2021.csv")


# Write full time series of MUX FP data to csv
write.csv(MUX, file = "MUX_FP_TS_2021.csv")
