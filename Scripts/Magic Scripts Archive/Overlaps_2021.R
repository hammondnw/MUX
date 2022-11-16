
# Script to load in FP data for both 1.6m SCAN and MUX, 
# then match WQ data to FP data 
# Authors: Nick Hammond
# Last Updated: 07/02/2021


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

#### Load in FP files from GitHub for the 1.6m SCAN ####

#create list with files from 1.6m SCAN (".fp" retrieves all files from 1.6m SCAN bc they all have a lower case fp)
fp.files20<-list.files(path="./FP_2020", pattern = ".fp")
fp.files21<-list.files(path="./FP_2021", pattern = ".fp")
fp.files <- c(fp.files20,fp.files21)


### Read in first file
obs<-read.table(file=paste0("./FP_2020/",fp.files[1]),skip=1,header=TRUE, row.names = NULL,
                fill= TRUE, sep = "\t") #read in first file
obs$Date.Time=ymd_hms(obs$Date.Time, tz = "America/New_York")

#reads in all files within folder in Github
for(i in 2:length(fp.files)){
  if(file.exists(paste0("./FP_2020/",fp.files[i])))
  { 
    temp<-read.table(file=paste0("./FP_2020/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    temp$Date.Time=ymd_hms(temp$Date.Time, tz = "America/New_York")
    obs<-rbind(obs,temp)}
  if(file.exists(paste0("./FP_2021/",fp.files[i])))
  { 
    temp<-read.table(file=paste0("./FP_2021/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    temp$Date.Time=ymd_hms(temp$Date.Time, tz = "America/New_York")
    obs<-rbind(obs,temp)}
}


# Remove duplicated rows (resulting from overlap in files)
obs = unique(obs)

#create list of the first line of each .fp file to make sure SCAN ID is consistent (QAQC)
id = list()
for(i in 2:length(fp.files)){
  if(file.exists(paste0("./FP_2020/",fp.files[i]))){
    abc = read.table(file=paste0("./FP_2020/",fp.files[i]),nrows=1,header=FALSE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    id = rbind(id, abc)
  }
  if(file.exists(paste0("./FP_2021/",fp.files[i]))){
    abc = read.table(file=paste0("./FP_2021/",fp.files[i]),nrows=1,header=FALSE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    id = rbind(id, abc)
  }
}
print(id)

#rename variables for wavelength columns
header = c("DateTime","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header

#plot some data from the combined file to check that all the data is there
plot(obs$DateTime,obs$'725')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(obs)){
  time1 = obs$DateTime[i-1]
  time2 = obs$DateTime[i]
  int = interval(time1,time2)
  if(int_length(int) > (24*60*60)){
    print(int)
  }
}

SSCAN = obs


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

muxfiles<-list.files(path="./FP_2021", pattern = ".FP")

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5)))), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(NA,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="America/New_York")

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(paste0("./FP_2021/",muxfiles[i]))>4000){
    temp<-read.table(file=paste0("./FP_2021/",muxfiles[i]),skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="America/New_York")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}

# Plot data
plot(obs2$DateTime,obs2$`255`)

# Fix timestamp at 05-27-2021 23:43:29
obs2[207,1] = mdy_hms("05-27-2021 23:43:29",tz="America/New_York")

# Ensure data are in chronological order
# Subset to date range after 2020-06-16 13:39 because data before that is messy
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

#### Correct Fouling in MUX data (convert valve # to depths, subset to date range, create new df of air-corrected absorbance) ####

# Create df of just air valve readings
mux_only_air = mux_only %>% filter(Depth=="air")

mux_only_matches = mux_only_air %>% slice_min(order_by = abs(as.numeric(mux_only$DateTime[1]) - as.numeric(mux_only_air$DateTime)), with_ties = FALSE)
for(i in 2:nrow(mux_only)){
  air_near = mux_only_air %>% slice_min(order_by = abs(as.numeric(mux_only$DateTime[i]) - as.numeric(mux_only_air$DateTime)), with_ties = FALSE)
  mux_only_matches = rbind(mux_only_matches,air_near)
}  

mux_only_corrected = mux_only
for(i in 1:nrow(mux_only)){
  
  mux_only_corrected[i,c(3:223)] = mux_only_corrected[i,c(3:223)] - mux_only_matches[i,c(3:223)]
  
}

# Pivot longer for plotting
mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance") %>%
  filter(wavelength %in% c(seq(200,700,100)))
mux_corrected_long=mux_only_corrected%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance") %>%
  filter(wavelength %in% c(seq(200,700,100)))
mux_air_long=mux_only_air%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance") %>%
  filter(wavelength %in% c(seq(200,700,100)))

mux_corrected_long = mux_corrected_long %>% select(DateTime, Depth, wavelength, absorbance) %>% 
  arrange(DateTime) %>% filter(Depth == "9") %>%
  mutate(Data = "corrected") #%>% rename(absorbance_cor = absorbance)

mux_air_long = mux_air_long %>%  select(DateTime, Depth, wavelength, absorbance) %>% 
  arrange(DateTime) %>% mutate(Data = "air") 

plot_df = mux_only_long %>% select(DateTime, Depth, wavelength, absorbance) %>%
  filter(Depth == "9") %>%  #rename(absorbance_raw = absorbance 
  arrange(DateTime) %>% mutate(Data = "raw")
plot_df = union(plot_df,mux_corrected_long)
plot_df = union(plot_df,mux_air_long)
#plot_df = full_join(plot_df,mux_corrected_long, by = c("DateTime","Depth","wavelength", "type","absorbance"))

# Plot time series for a single wavelength to visualize corrections 
png("MUX20_rawFP_air_correct_022222.png",width = 13, height = 6, units = 'in', res = 300)
ggplot() +
  geom_path(data = plot_df, aes(x=DateTime,y=absorbance,colour=Data),size = 1.3) +
  facet_wrap(facets = vars(wavelength), ncol = 2, scales = "free_y") +
  theme(legend.position = "bottom") +
  ggtitle("Absorbance at Every 100nm; Depth = 9m; Corrected = Raw - Air")
dev.off()

# Plot wavelength vs. absorption for a single time point
ggplot() +
  geom_point(data = filter(mux_only_long, Depth == "9" & DateTime > as.POSIXct("2021-06-11 11:00")), 
             aes(x=wavelength,y=absorbance), colour='blue') +
  geom_point(data = filter(mux_corrected_long, Depth == "9" & DateTime > as.POSIXct("2021-06-11 11:00")), 
             aes(x=wavelength,y=absorbance), colour='darkgreen')


ggplot() +
  geom_point(data = mux_only_long, 
             aes(x=wavelength,y=absorbance), colour='blue') +
  geom_point(data = mux_corrected_long, 
             aes(x=wavelength,y=absorbance), colour='darkgreen')




#### Create new df with final dataset ####
MUX = mux_only

#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")
pathWQ = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/"
WQ = "Metals_2021.xlsx"
dataWQ<-read_xlsx(paste(pathWQ,WQ,sep="")) #Import data as .csv file

# Filter dataWQ to just the necessary data
dataWQ = dataWQ %>% filter(Reservoir=="FCR") %>%
  filter(Site=="50") %>%
  select(1:8) %>%
  mutate(DateTime = ymd_hms(DateTime, tz = "America/New_York"))

#### Match WQ times with 1.6m SCAN times to find the reading closest to the sampling time ####
   # ! Need to edit this to mirror the code below for the MUX ! #

WQtimes <- dataWQ %>% filter(Depth_m==1.6) %>%
  select(DateTime)

df.final<-SSCAN %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1])))) #Create a new dataframe of just the first sample 
for (i in 2:length(WQtimes$DateTime)){ #loop through all samples and add the closest values to the final dataframe
  SSCAN_atThisTime <- SSCAN %>% slice(which.min(abs(as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))))
  df.final <- rbind(df.final,SSCAN_atThisTime)
}

SSCAN_FP_Overlaps_2021 = df.final

#Plot to check
plot(WQtimes,SSCAN_FP_Overlaps_2021$DateTime, xlab = "Sampling Times",
     ylab = "SSCAN times")

#Write to csv
write.csv(SSCAN_FP_Overlaps_2021, file = "SSCAN_FP_Overlaps_2021.csv")

#### Match WQ times with MUX times to find the reading closest to the sampling time ####

WQtimes <- dataWQ %>% select(DateTime,Depth_m)
WQtimes$Depth_m <- as.character(WQtimes$Depth_m)
WQtimes$DateTime <- ymd_hms(WQtimes$DateTime, tz="America/New_York")

#Subset date range to Oxygen On Deployment
WQtimes <- WQtimes[WQtimes$DateTime>"2021-05-26 00:00:00",]
WQtimes <- WQtimes %>% mutate(date = date(DateTime))
# Remove samples from 06-02-2021 because the MUX was not deployed during that time
WQtimes <- WQtimes %>% filter(date(DateTime) != "2021-06-02")

#Add MUX cleaning times 
cleaning = tibble(clean_time =ymd_hms(c("2021-05-26 00:00:00", "2021-05-28 09:30:00", "2021-05-31 00:00:00", "2021-06-02 12:22:00", "2021-06-04 11:00:00",
                                        "2021-06-11 00:00:00", "2021-06-07 10:00:00","2021-06-10 16:55:00", "2021-06-15 17:25:00","2021-06-18 10:30:00"),
                                      "2021-06-21 00:00:00", tz="America/New_York"),
                  date = date(clean_time))
WQtimes <- WQtimes %>% left_join(cleaning, WQtimes, by="date")

df.final<-MUX %>% filter(Depth == WQtimes$Depth_m[1]) %>% 
  filter(DateTime > WQtimes$clean_time[1]) %>%   #slice_min(which(as.numeric(DateTime) - as.numeric(WQtimes[1]))>0) #Create a new dataframe of just the first sample
  mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1]))  %>% 
  slice_min(abs(time_diff))
for (i in 2:nrow(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- MUX %>% filter(Depth == WQtimes$Depth_m[i]) %>% 
    filter(DateTime > WQtimes$clean_time[i]) %>%   #slice_min(which(as.numeric(DateTime) - as.numeric(WQtimes[1]))>0) #Create a new dataframe of just the first sample
    mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))  %>% 
    slice_min(abs(time_diff))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2021 = df.final


#Plot to check
plot(WQtimes$DateTime[which(WQtimes$Depth_m=="9")],MUX_FP_Overlaps_2021$DateTime[which(MUX_FP_Overlaps_2021$Depth =="9")],
     xlab = "Sampling Times", ylab = "MUX times")
abline(a=0,b=1)

MUX_FP_Overlaps_2021 = MUX_FP_Overlaps_2021 %>% select(-c(time_diff))



#Write to csv
write.csv(MUX_FP_Overlaps_2021, file = "MUX_FP_Overlaps_2021.csv")


# Write full time series of SSCAN and MUX FP data to csv
write.csv(SSCAN, file = "SSCAN_FP_TS_2021.csv")
write.csv(MUX, file = "MUX_FP_TS_2021.csv")
