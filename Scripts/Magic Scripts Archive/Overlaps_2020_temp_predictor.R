# Script to load in FP data for both 1.6m SCAN and MUX, 
# then match WQ data to FP data 
# Adding water temp as an extra X variable
# Authors: Nick Hammond


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
header = c("Date.Time","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header

#plot some data from the combined file to check that all the data is there
plot(obs$Date.Time,obs$'725')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(obs)){
  time1 = obs$Date.Time[i-1]
  time2 = obs$Date.Time[i]
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

muxfiles<-list.files(path="./FP_2020", pattern = ".FP")

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5)))), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(NA,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="America/New_York")

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(paste0("./FP_2020/",muxfiles[i]))>4000){
    temp<-read.table(file=paste0("./FP_2020/",muxfiles[i]),skip=2,header=FALSE, row.names = NULL, sep = "\t")
    names(temp) <- mux_colnames
    temp$DateTime=ymd_hms(temp$DateTime, tz="America/New_York")
    obs2<-rbind(obs2,temp)
    #print(i)
  }
}


# Subset to date range after 2020-06-16 13:39 because data before that is messy
# May go back and clean this up later
mux_only=obs2[obs2$DateTime>c("2020-10-01 13:39"),] #change to not default to UTC, should be 14:15 in GMT 4
mux_only=mux_only[order(mux_only$DateTime),]

# Plot data
plot(mux_only$DateTime,mux_only$`255`)
ggplot(data = filter(mux_only,Valve==7)) +
  geom_point(aes(x = DateTime, y = `200`))

# Convert valve # to depth
Valves = as.data.frame(mux_only$Valve)
colnames(Valves)=c("Valve")
Valve_depth <- data.frame(
  Valve = c (1:12), 
  Depth_m = c("0.1","1.6","3.8","5","6.2", "8", "9", "NA", "acid_r", "air","NA", "air"),
  stringsAsFactors = FALSE
)
Valves = Valves %>% 
  left_join(Valve_depth, by="Valve") %>% 
  select(Depth_m)
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
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  filter(wavelength %in% c(seq(200,700,100)))
mux_corrected_long=mux_only_corrected%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  filter(wavelength %in% c(seq(200,700,100)))
mux_air_long=mux_only_air%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  filter(wavelength %in% c(seq(200,700,100)))

mux_corrected_long = mux_corrected_long %>% select(DateTime, Depth, wavelength, absorbance) %>% 
  arrange(DateTime) %>% filter(Depth == "9.0") %>%
  mutate(Data = "corrected") #%>% rename(absorbance_cor = absorbance)

mux_air_long = mux_air_long %>%  select(DateTime, Depth, wavelength, absorbance) %>% 
  arrange(DateTime) %>% mutate(Data = "air") 

plot_df = mux_only_long %>% select(DateTime, Depth, wavelength, absorbance) %>%
  filter(Depth == "9.0") %>%  #rename(absorbance_raw = absorbance 
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
  geom_point(data = filter(mux_only_long, Depth == "9.0" & DateTime > as.POSIXct("2020-10-28 12:00")), 
             aes(x=wavelength,y=absorbance), colour='blue') +
  geom_point(data = filter(mux_corrected_long, Depth == "9.0" & DateTime > as.POSIXct("2020-10-28 12:00")), 
             aes(x=wavelength,y=absorbance), colour='darkgreen')


#### Create new df with final dataset ####
MUX = mux_only


#### Add more predictor variables (e.g. water temp) ####

# Read in catwalk dataset
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")
catwalk = read.csv(paste0(getwd(),"/Data/Covariate data/Catwalk_EDI_2020.csv"))

# Select the variables we want
catwalk_exp = catwalk %>% select(Reservoir,Site,DateTime,RDO_mgL_5_adjusted,
                                 RDO_mgL_9_adjusted, EXODO_mgL_1,ThermistorTemp_C_surface,
                                 ThermistorTemp_C_1,ThermistorTemp_C_2, ThermistorTemp_C_3,
                                 ThermistorTemp_C_4, ThermistorTemp_C_8, ThermistorTemp_C_9,
                                 ThermistorTemp_C_5, ThermistorTemp_C_6, ThermistorTemp_C_7,
                                 EXOfDOM_QSU_1, EXOSpCond_uScm_1, EXOChla_ugL_1)

# Convert DateTime to PosixCT
catwalk_exp$DateTime = mdy_hm(catwalk_exp$DateTime, tz="America/New_York") 

# Select the reservoir, site, and date range we want
catwalk_exp = catwalk_exp %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-10-16 13:10:00") %>%
  filter(DateTime<"2020-11-11 00:00:00")

# convert temp to long format for plotting
therm_depths = data.frame(depth_m = c(0.1,1,1.6,3,3.8,5,6.2,7,8,9), #Changed the depths to match the closest MUX depth
                          depth = c("ThermistorTemp_C_surface",
                                                          paste0("ThermistorTemp_C_",rep(1:9))))
DO_depths = data.frame(depth_m = c(1.6,5,9), depth = c("EXODO_mgL_1","RDO_mgL_5_adjusted",
                                                       "RDO_mgL_9_adjusted"))
catwalk_exp_long = catwalk_exp %>% pivot_longer(cols=c(7:16),names_to = "depth", 
                                                values_to = "temperature") %>%
  left_join(therm_depths, by = "depth")

# filter temp by depth
#catwalk_exp_long = catwalk_exp_long %>% filter(depth_m %in% c(0.1,1,2,3,4,5,6,7,8,9))

# select just DateTime, depth, and temperature
temperature = catwalk_exp_long %>% select(DateTime,temperature, depth_m)
temperature$depth_m = as.character(temperature$depth_m)

# create new vector of FP times
MUX_time_depth = MUX %>% select(DateTime,Depth_m)

# Replace "air" depths with "0.1" so the matching code below will work (and those will be removed anyways)
MUX_time_depth = MUX_time_depth %>% mutate(Depth_m = if_else(Depth_m == "air","0.1",Depth_m))

# Loop through each MuX depth/time combo and find the closest temp value for that depth
temperature_matches <- temperature %>% filter(depth_m == MUX_time_depth$Depth_m[1]) %>% 
  mutate(time_diff = as.numeric(DateTime) - as.numeric(MUX_time_depth$DateTime[1]))  %>% 
  slice_min(abs(time_diff))

for (i in 2:nrow(MUX_time_depth)){
 #loop through all sample times and add the closest values to the final dataframe
  temp_temp <- temperature %>% filter(depth_m == MUX_time_depth$Depth_m[i]) %>% 
    mutate(time_diff = as.numeric(DateTime) - as.numeric(MUX_time_depth$DateTime[i]))  %>% 
    slice_min(abs(time_diff))
  temperature_matches <- rbind(temperature_matches,temp_temp)
}

temp_final = temperature_matches$temperature

MUX = MUX[-c(3615:3629),]

MUX = cbind(MUX,temp_final)

ggplot() +
geom_path(data = MUX, aes(x=DateTime,y=temp_final,color=Depth_m)) #+
  geom_point(data = catwalk_exp_long, aes(x=DateTime,y=temperature,color=as.character(depth_m)))

#write to csv
write.csv(MUX, file = "MUX_FP_TS_2020_w_temp.csv")

#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")
pathWQ = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/"
WQ = "Metals_2014_2021.csv"
dataWQ<-read_csv(paste(pathWQ,WQ,sep="")) #Import data as .csv file
dataWQ$DateTime <- ymd_hms(dataWQ$DateTime, tz="America/New_York")
dataWQ$Depth_m <- as.character(dataWQ$Depth_m)

dataWQ = dataWQ %>% filter(Reservoir == "FCR" & Site == 50) %>% 
  filter(DateTime > "2020-10-15 12:00:00") %>% 
  filter(DateTime < "2020-11-09 15:00:00") %>% 
  select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)

#### Match WQ times with 1.6m SCAN times to find the reading closest to the sampling time ####

WQtimes <- dataWQ[which(dataWQ$Depth_m==1.6),]
WQtimes <- WQtimes$Date
WQtimes <- ymd_hms(WQtimes, tz="America/New_York")

df.final<-SSCAN %>% slice(which.min(abs(as.numeric(Date.Time) - as.numeric(WQtimes[1])))) #Create a new dataframe of just the first sample 
for (i in 2:length(WQtimes)){ #loop through all samples and add the closest values to the final dataframe
  SSCAN_atThisTime <- SSCAN %>% slice(which.min(abs(as.numeric(Date.Time) - as.numeric(WQtimes[i]))))
  df.final <- rbind(df.final,SSCAN_atThisTime)
}

SSCAN_FP_Overlaps_2020 = df.final

#Plot to check
plot(WQtimes,SSCAN_FP_Overlaps_2020$Date.Time, xlab = "Sampling Times",
     ylab = "SSCAN times")

#Write to csv
write.csv(SSCAN_FP_Overlaps_2020, file = "SSCAN_FP_Overlaps_2020.csv")



#### Match WQ times with MUX times to find the reading closest to the sampling time ####
WQtimes <- dataWQ %>% select(DateTime,Depth_m)
WQtimes$Depth_m <- as.character(WQtimes$Depth_m)
WQtimes$DateTime <- ymd_hms(WQtimes$DateTime, tz="America/New_York")

#Subset date range to second deployment of MUX in Oct
WQtimes <- WQtimes[WQtimes$DateTime>"2020-10-16 12:00:00",]
WQtimes <- WQtimes %>% mutate(date = date(DateTime))

#Add MUX cleaning times 
cleaning = tibble(clean_time =ymd_hms(c("2020-10-16 13:00:00", "2020-10-17 14:00:00", # 2020-10-16 14:30 picked as arbitrary cutoff after first pump sequence
                                        "2020-10-19 11:26:00", "2020-10-21 11:27:00", 
                                        "2020-10-23 10:35:00", "2020-10-26 10:40:00",
                                        "2020-10-28 09:17:00", "2020-10-30 12:00:00",
                                        "2020-11-02 09:40:00", "2020-11-04 10:59:00", 
                                        "2020-11-09 12:00:00"), tz="America/New_York"),
                  date = date(clean_time))

WQtimes <- WQtimes %>% left_join(cleaning, WQtimes, by="date")

df.final<-MUX %>% filter(Depth_m == WQtimes$Depth_m[1]) %>% 
  mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[1]))  %>% 
  slice_min(abs(time_diff)) %>% 
  mutate(WQ_DateTime = WQtimes$DateTime[1])


for (i in 2:nrow(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- MUX %>% filter(Depth_m == WQtimes$Depth_m[i]) %>% 
    mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$DateTime[i]))  %>% 
    mutate(WQ_DateTime = WQtimes$DateTime[i]) %>%
    slice_min(abs(time_diff))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

df.final = df.final %>% arrange(-desc(DateTime))

colnames(dataWQ) = c("WQ_DateTime","Depth_m","TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL")
WQ_matches = df.final %>% select(WQ_DateTime,Depth_m) %>% left_join(dataWQ, by=c("WQ_DateTime","Depth_m"))
colnames(WQ_matches) = c("DateTime","Depth_m","TFe_mgL","TMn_mgL","SFe_mgL","SMn_mgL")
WQ_matches = WQ_matches %>% arrange(-desc(DateTime))

MUX_FP_Overlaps_2020 = df.final
MUX_FP_Overlaps_2020 = MUX_FP_Overlaps_2020 %>% select(-c(Depth_m,time_diff,WQ_DateTime))

#Plot to check #[which(WQ_matches$Depth_m=="9")] #[which(df.final$Depth_m =="9")]
plot(WQ_matches$DateTime,df.final$DateTime,
     xlab = "Sampling Times", ylab = "MUX times")
abline(a=0,b=1)

#Write to csv
write.csv(MUX_FP_Overlaps_2020, file = "MUX_FP_Overlaps_Oct_Nov_2020_w_temp.csv")
write.csv(WQ_matches, file = "MUX_WQ_Oct_Nov_2020_w_temp.csv")

# Write full time series of SSCAN and MUX FP data to csv
#write.csv(SSCAN, file = "SSCAN_FP_TS_2020.csv")
write.csv(MUX, file = "MUX_FP_TS_2020_w_temp.csv")
