
# Script to load in FP data for both 1.6m SCAN and MUX, 
# then match WQ data to FP data 
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
fp.files22<-list.files(path="./FP_2022", pattern = ".fp")
fp.files <- c(fp.files20,fp.files21,fp.files22)


### Read in first file
obs<-read.table(file=paste0("./FP_2020/",fp.files[1]),skip=1,header=TRUE, row.names = NULL,
                fill= TRUE, sep = "\t") #read in first file
obs$Date.Time=ymd_hms(obs$Date.Time, tz = "EST")

#reads in all files within folder in Github
for(i in 2:length(fp.files)){
  if(file.exists(paste0("./FP_2020/",fp.files[i])))
  { 
  temp<-read.table(file=paste0("./FP_2020/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                   sep = "\t", fill = TRUE)
  temp$Date.Time=ymd_hms(temp$Date.Time, tz = "EST")
  obs<-rbind(obs,temp)}
  if(file.exists(paste0("./FP_2021/",fp.files[i])))
  { 
    temp<-read.table(file=paste0("./FP_2021/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    temp$Date.Time=ymd_hms(temp$Date.Time, tz = "EST")
    obs<-rbind(obs,temp)}
  if(file.exists(paste0("./FP_2022/",fp.files[i])))
  { 
    temp<-read.table(file=paste0("./FP_2022/",fp.files[i]),skip=1,header=TRUE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    temp$Date.Time=ymd_hms(temp$Date.Time, tz = "EST")
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
  if(file.exists(paste0("./FP_2022/",fp.files[i]))){
    abc = read.table(file=paste0("./FP_2022/",fp.files[i]),nrows=1,header=FALSE, row.names = NULL,
                     sep = "\t", fill = TRUE)
    id = rbind(id, abc)
  }
}
print(id)

#rename variables for wavelength columns
header = c("DateTime","Status", seq(200, 750, 2.50))
colnames(obs) <- header

#plot some data from the combined file to check that all the data is there
plot(obs$DateTime,obs$'725')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's

missing = obs[is.na(obs$DateTime)==TRUE,]
#fill_031421 = seq.POSIXt(from = as_datetime("2021-03-14 02:01:34",tz="EST"), 
#                         to = as_datetime("2021-03-14 02:51:34",tz="EST"), by = 600)

for(i in 2:nrow(obs)){
  time1 = obs$DateTime[i-1]
  time2 = obs$DateTime[i]
  int = interval(time1,time2)
  if(int_length(int) > (24*60*60)){
    print(int)
  }
}

SSCAN = obs

#put the data in long format and filter to every 100nm 
SSCAN_long=SSCAN%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  filter(wavelength %in% c("200", "300", "400", "500", "600", "700"))

#Filter by time
#SSCAN_long = SSCAN_long %>% 
#  filter(DateTime > "2020-10-01 00:00") %>%
#  filter(DateTime < "2021-03-01 00:00")


#Create a plot of every 100nm wavelengths for a specific depth
png("SCAN_FP_raw_041122.png",width = 9, height = 4, units = 'in', res = 300) 
SCAN_plot = ggplot()+
  geom_point(data=SSCAN_long, aes(x=DateTime, y=absorbance, color=wavelength),size=0.5)
#geom_vline(data=cleaning, aes(xintercept=DateTime) , linetype="dashed", color="black", size=0.5)
SCAN_plot
dev.off()



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

#### Load in 2020 WQ data from local device storage (entire dataset in csv format) ####

#Change working directory to folder where WQ data is housed
setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")
pathWQ = "C:/Users/hammo/Documents/Magic Sensor PLSR/Data/"
WQ = "FCR_Jan_Nov_2020.xlsx"
dataWQ<-read_xlsx(paste(pathWQ,WQ,sep="")) #Import data as .csv file


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
WQtimes <- dataWQ %>% select(Date,Depth_m)
WQtimes$Depth_m <- as.character(WQtimes$Depth_m)
WQtimes$Date <- ymd_hms(WQtimes$Date, tz="America/New_York")

#Subset date range to second deployment of MUX in Oct
WQtimes <- WQtimes[WQtimes$Date>"2020-10-16 12:00:00",]
WQtimes <- WQtimes %>% mutate(date = date(Date))

#Add MUX cleaning times 
cleaning = tibble(clean_time =ymd_hms(c("2020-10-16 08:00:00", "2020-10-17 00:00:00", "2020-10-19 11:26:00", "2020-10-21 11:27:00", "2020-10-23 10:35:00",
                                  "2020-10-26 10:40:00","2020-10-28 09:17:00", "2020-10-30 08:00:00","2020-11-02 09:40:00",
                                  "2020-11-04 10:59:00", "2020-11-09 00:00:00"), tz="America/New_York"),
                  date = date(clean_time))
WQtimes <- WQtimes %>% left_join(cleaning, WQtimes, by="date")

df.final<-MUX %>% filter(Depth == WQtimes$Depth_m[1]) %>% 
  filter(DateTime > WQtimes$clean_time[1]) %>%   #slice_min(which(as.numeric(DateTime) - as.numeric(WQtimes[1]))>0) #Create a new dataframe of just the first sample
  mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$Date[1]))  %>% 
  slice_min(abs(time_diff))
for (i in 2:nrow(WQtimes)){ #loop through all sample times and add the closest values to the final dataframe
  MUX_atThisDepth <- MUX %>% filter(Depth == WQtimes$Depth_m[i]) %>% 
    filter(DateTime > WQtimes$clean_time[i]) %>%   #slice_min(which(as.numeric(DateTime) - as.numeric(WQtimes[1]))>0) #Create a new dataframe of just the first sample
    mutate(time_diff = as.numeric(DateTime) - as.numeric(WQtimes$Date[i]))  %>% 
    slice_min(abs(time_diff))
  df.final <- rbind(df.final,MUX_atThisDepth)
}

MUX_FP_Overlaps_2020 = df.final
MUX_FP_Overlaps_2020 = MUX_FP_Overlaps_2020 %>% select(-c(Depth,time_diff))

#Plot to check
plot(WQtimes$Date[which(WQtimes$Depth_m=="9")],MUX_FP_Overlaps_2020$DateTime[which(MUX_FP_Overlaps_2020$Depth =="9")],
     xlab = "Sampling Times", ylab = "MUX times")
abline(a=0,b=1)

#Write to csv
write.csv(MUX_FP_Overlaps_2020, file = "MUX_FP_Overlaps_Oct_Nov_2020.csv")


# Write full time series of SSCAN and MUX FP data to csv
write.csv(SSCAN, file = "SSCAN_FP_TS_2020.csv")
write.csv(MUX, file = "MUX_FP_TS_2020.csv")
