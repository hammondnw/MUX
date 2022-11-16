#Magic Mux 2020 Data Presentation
#Authors: Bethany Bookout, Nick Hammond, Adrienne Breef-Pilz, Rachel Corrigan

#MUX depth code for 2020, 1 = surface; 2 = 1.6m; 3= 3.8m; 4 = 5.0m; 5 = 6.2m; 6 = 8.0m; 7 = = 9.0m; 9 = acid rinse; 10 = air; 12 = water rinse (used surface and DI rinse at different points in 2020)

#### 1.6 m SCAN load ####
# important dates: April 7 15:44:54 1.6m scan deployed
# April 10 15:19:53 4.5m scan deployed; 
# April 24 9:49:52 4.5m pulled up, mux deployed again, 
# start all data on April 10 15:19 (or start scan 1.6 on April 7)
# scan_cleandates=c("2020-04-10","2020-04-20","2020-05-11","2020-05-25","2020-06-08","2020-06-29","2020-07-06","2020-07-10","2020-07-20","2020-07-31") #clean dates according to field sheets
# mux_cleandates=c("2020-05-04","2020-06-08","2020-06-25","2020-07-10","2020-07-31 12:00")  #clean dates according to field sheets

#packages need
library(lubridate)
library(tidyverse)
library(magrittr)
library(gganimate)
library(gifski)
require(transformr)
library(stringr)


#set working directory
setwd('./MagicData/FP_2020')

#create list with files from 1.6m SCAN (".fp" retrieves all files from 1.6m SCAN bc they all have a lower case fp)
fp.files<-list.files(path=".", pattern = ".fp")

### Read in first file
obs<-read.table(file=fp.files[1],skip=1,header=TRUE, row.names = NULL,
                fill= TRUE, sep = "\t") #read in first file
obs$Date.Time=ymd_hms(obs$Date.Time, tz = "Etc/GMT+4")

#reads in all files within folder in Github
for(i in 2:length(fp.files)){ 
  temp<-read.table(file=fp.files[i],skip=1,header=TRUE, row.names = NULL,
                   sep = "\t", fill = TRUE)
  temp$Date.Time=ymd_hms(temp$Date.Time, tz = "Etc/GMT+4")
  obs<-rbind(obs,temp)
}

#create list of the first line of each .fp file to make sure SCAN ID is consistent
id = list()
for(i in 2:length(fp.files)){
  abc = read.table(file=fp.files[i],nrows=1,header=FALSE, row.names = NULL,
                  sep = "\t", fill = TRUE)
  id = rbind(id, abc)
}

#plot some data from the combined file to check that all the data is there
header = c("Date.Time","Status_0", seq(200.00, 750.00, 2.50))
colnames(obs) <- header
plot(obs$Date.Time,obs$'725')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(obs)){
  time1 = obs$Date.Time[i-1]
  time2 = obs$Date.Time[i]
  int = interval(time1,time2)
  if(int_length(int) > (10*60)){
    print(int)
  }
}

##old code!
##Plot wavelength vs. absorbance for discrete time points
##First we have to rearrange the data so that each time point is a column and 
##the rows are wavelengths

#obs_trans = t(obs)
#obs_trans = cbind(rownames(obs_trans), data.frame(obs_trans, row.names=NULL))
#times = obs_trans[1,]
#colnames(obs_trans) = times
#colnames(obs_trans)[1] = "wavelength"
#obs_trans = obs_trans[-c(1,2),]

#plot(obs_trans$wavelength,obs_trans$`2020-04-01 11:56:59`, type='l', ylim=c(0,30))
#lines(obs_trans$wavelength,obs_trans$`2020-05-01 12:02:38`, col="red")
#lines(obs_trans$wavelength,obs_trans$`2020-06-01 11:56:50`, col="green")
#lines(obs_trans$wavelength,obs_trans$`2020-07-01 12:00:12`, col="yellow")

# Create animated GIF of wavelength vs. absorption over time
#To create animation- rearrange data so that there are three columns:
#Date.Time, Wavelength, Absorbance
obs_animate = pivot_longer(obs, cols=3:223, names_to = "wavelength", values_to = "absorbance")

#subset data to a smaller interval (one day)
sub= interval(start="2020-10-16 13:00:00", end="2020-10-17 14:00:00", tz="Etc/GMT+4")
obs_animate_sub = obs_animate[obs_animate$Date.Time %within% sub,]
obs_animate_sub$wavelength = as.numeric(obs_animate_sub$wavelength)

# Create animated GIF of wavelength vs. absorption over time
#install.packages('gganimate')

p <- ggplot(obs_animate_sub, aes(x = wavelength, y = absorbance)) +
  geom_line(aes(group=Date.Time))
a <- p + transition_time(Date.Time) +
  labs(title = "Date.Time: {frame_time}") +
  scale_x_continuous(breaks = c(seq(200,750,100)), limits = c(200,750)) +
  ease_aes('cubic-in-out')
animate(a, nframes=150, fps=12)
anim_save("1.6_Oct16_Oct17.gif", animation = last_animation())

#More plots (for powerpoint)
dev.off()
# All wavelengths
ggplot(obs_animate, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength))) +
  theme(legend.position='none')
# Select a few wavelengths
obs_animate2 = filter(obs_animate, wavelength==200|wavelength==300|wavelength==400
   |wavelength==500|wavelength==600|wavelength==700)

# Add vertical lines for cleaning dates
cleaning = as.POSIXct(c("2020-04-10 12:00","2020-04-20 12:40", "2020-05-11 12:30",
                         "2020-05-25 10:52","2020-06-08 13:30","2020-06-22 10:18",
                        "2020-06-29 12:40", "2020-07-06 12:48", "2020-07-13 11:24",
                        "2020-07-20 11:50"), tz="Etc/GMT+4")

png("1.6m_ts_2020.png",width = 7, height = 3, units = 'in', res = 300)
ggplot(obs_animate2, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))+
  geom_vline(xintercept = cleaning, linetype="dotted", 
             color = "black", size=0.6)
dev.off()
  #theme(legend.position='none')
# Plot a Subset of same time period as 4.5m Scan
# Cleaning times
cleaning_sub = as.POSIXct(c("2020-04-20 12:40"), tz="Etc/GMT+4")
obs_animate_sub2 = filter(obs_animate_sub, wavelength==200|wavelength==300|wavelength==400
                      |wavelength==500|wavelength==600|wavelength==700)
png("1.6m_ts_Oct_sub.png",width = 7, height = 3, units = 'in', res = 300)
ggplot(obs_animate_sub2, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))
  #geom_vline(xintercept = cleaning_sub, linetype="dotted", 
            # color = "black", size=0.6)
dev.off()





###### MUX Load ######

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

muxfiles<-list.files(path=".", pattern = ".FP")

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5))),"nm"), "Valve","Measurement time")
obs2 <- as.data.frame(matrix(,0,length(mux_colnames)))
names(obs2) <- mux_colnames
obs2$DateTime=ymd_hms(obs2$DateTime, tz="Etc/GMT+4")

for(i in 1:length(muxfiles)){ #reads in all files within folder in Github
  if(file.size(muxfiles[i])>4000){
  temp<-read.table(file=muxfiles[i],skip=2,header=FALSE, row.names = NULL, sep = "\t")
  names(temp) <- mux_colnames
  temp$DateTime=ymd_hms(temp$DateTime, tz="Etc/GMT+4")
  obs2<-rbind(obs2,temp)
  #print(i)
}
}

mux_only=obs2[obs2$DateTime>"2020-04-24 14:15:00",] #change to not default to UTC, should be 14:15 in GMT 4
mux_only=mux_only[order(mux_only$DateTime),]

###### Pump log load ######
setwd("..")
log_files=list.files(path = ".", pattern = glob2rx("20*MUX.TXT"))
logs<-read.table(file=log_files[1],header=T, row.names = NULL, sep = ",", fill = TRUE, stringsAsFactors =F) #read in first file

for(i in 2:length(log_files)){ #reads in all files within folder in Github
  temp<-read.table(file=log_files[i], header=T, row.names = NULL, sep = ",",fill = TRUE, stringsAsFactors =F)
  logs<-rbind(logs,temp)
  #print(i)
}

pumpCols <- c("Time", "Valve", "Dir", "PumpTime", "Measure","Purge", "Notes")
colnames(logs) = pumpCols

logs=na.omit(logs)

logs$Time=ymd_hms(logs$Time, tz="Etc/GMT+4")

#filter out unnecessary data
logs <- logs %>%
  filter(str_detect(Measure,"Manual", negate = TRUE)) %>%
  filter(str_detect(Dir,"Forward")) %>%
  filter(str_detect(Notes,"Manual", negate = TRUE))%>%
  filter(str_detect(Notes,"Manual - Start!", negate = TRUE))

#fix structure of data to numerical or date
logs$PumpTime <- seconds(logs$PumpTime)

#create measurement time column
logs$Time_p_Pump <- logs$Time+logs$PumpTime

##### Assign proper pump valve with fp data #####

#assign valve by closest time in pump log, steps 2 program results in warning, need to fix maybe, only effects valve 9 on steps 2, code always chooses first valve listed
for (k in 1:nrow(mux_only)) {
  temptime = interval(start = mux_only$DateTime[k]-minutes(2), end = mux_only$DateTime[k]+minutes(2) ) #trying something out with data
  mux_only$correctvalve_a[k]=logs$Valve[logs$Time %within% temptime]
  #mux_only$correctvalve_b[k]=logs$Valve[logs$Time_p_Pump %within% temptime]
  mux_only$logtime_a[k]=logs$Time[logs$Time %within% temptime]
  #mux_only$logtime_b[k]=logs$Time_p_Pump[logs$Time %within% temptime]
}

#mux_only2=mux_only[,c(1,224,226:229,2:223,225)]

#####graphs of absorbance and wavelength for 2020 for each depth######

#create a data frame of valve number and depth
valve_depth <- data.frame(
  correctvalve_a = c (1:12), 
  Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0", "NA", "acid_r", "air","NA", "water_r"),
  stringsAsFactors = FALSE
)

#put the data in long format and add valve depth

mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  left_join(valve_depth, by="correctvalve_a")%>%
  filter(wavelength %in% c("200nm", "300nm", "400nm", "500nm", "600nm", "700nm"))%>%
  filter(Depth %in% c('0.1','1.6','3.8','5.0','6.2','8.0','9.0'))

#mux cleaning times
# mux_cleandates=c("2020-05-04","2020-06-08","2020-06-25","2020-07-10","2020-07-31")  
#clean dates according to field sheets
mux_cleaning = as.POSIXct(c("2020-05-04 15:00",
  "2020-07-06 12:48", "2020-07-13 11:24", "2020-06-08 11:50",
  "2020-06-25 12:00", "2020-07-31 12:00"), tz="Etc/GMT+4")

#subset for most recent time period
mux_lastweek=mux_only_long[mux_only_long$DateTime>"2020-08-06 00:00:00" & mux_only_long$DateTime<"2020-08-08 00:00:00",]

#create  a multipanel plot of absorbance over time separated by depth 
png("mux_cleaning_raw_by_depth.png",width = 9, height = 4, units = 'in', res = 300)
ggplot(mux_lastweek, aes(x=DateTime, y=absorbance, color=wavelength)) + 
 geom_line() +
  geom_vline(xintercept = mux_cleaning, linetype="dotted", 
             color = "black", size=0.6)+
   facet_grid(rows = vars(Depth))
dev.off()
  
####compare 1.6m mux data with 1.6m scan####
mux_16 = filter(mux_only_long,Depth=='1.6')
mux_16$wavelength2=gsub('nm','',mux_16$wavelength)

png("mux16_2020_raw_by_depth.png",width = 8, height = 2, units = 'in', res = 300)
ggplot(mux_16, aes(x=DateTime, y=absorbance, color=wavelength)) + 
  geom_line() +
  geom_vline(xintercept = mux_cleaning, linetype="dotted", 
             color = "black", size=0.6)#+
  #facet_grid(rows = vars(Depth))
dev.off()

png("scan16_2020_muxmatch.png",width = 9, height = 2, units = 'in', res = 300)

ggplot(obs_animate2, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))+
  geom_vline(xintercept = cleaning, linetype="dotted", 
             color = "black", size=0.6)

dev.off()

# for (l in 1:nrow(mux_v2)) { #struggling with this for loop, trying to get closest measurements together..
#   temptime = interval(start = mux_16$DateTime[l]-minutes(4), end = mux_16$DateTime[l]+minutes(4) ) #trying something out with data
#   mux_16$scan_abs[l]=obs_animate2$absorbance[obs_animate2$wavelength[obs_animate2$Date.Time %within% temptime]==mux_16$wavelength2[l]]
#   mux_16$scantime[l]=obs_animate2$Date.Time[obs_animate2$Date.Time %within% temptime && obs_animate2$wavelength==mux_16$wavelength2[l]]
# }

##### 4.5 m scan #####
deploy_time = interval(start = "2020-04-10 15:15:00", end = "2020-04-24 10:00:00", tz="Etc/GMT+4" )
scan_45=obs2[obs2$DateTime %within% deploy_time,]

#plot some data to check that it's all there
plot(scan_45$DateTime,scan_45$'252.5nm')

#check for data gaps - this just prints the intervals that are > 10 min. 
#Can add more code later to fill gaps with 0's or NA's
for(i in 2:nrow(scan_45)){
  time1 = scan_45$DateTime[i-1]
  time2 = scan_45$DateTime[i]
  int = interval(time1,time2)
  if(int_length(int) > (10*60)){
    print(int)
  }
}

##old code!
##Plot wavelength vs. absorbance for discrete time points
##First we have to rearrange the data so that each time point is a column and 
##the rows are wavelengths
#scan_45_trans = t(scan_45)
#scan_45_trans = cbind(rownames(scan_45_trans), data.frame(scan_45_trans, row.names=NULL))
#times = scan_45_trans[1,]
#colnames(scan_45_trans) = times
#colnames(scan_45_trans)[1] = "wavelength"
#scan_45_trans = scan_45_trans[-c(1,2),]

#plot(scan_45_trans$wavelength,scan_45_trans$`2020-04-10 15:27:45`, type='l', ylim=c(0,30))
#lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-13 15:19:59`, col="red")
#lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-17 15:20:01`, col="green")
#lines(scan_45_trans$wavelength,scan_45_trans$`2020-04-23 15:20:01`, col="yellow")

# Create animated GIF of wavelength vs. absorption over time
#To create animation- rearrange data so that there are three columns:
#Date.Time, Wavelength, Absorbance
scan_45_animate = pivot_longer(scan_45, cols=3:223, names_to = "wavelength", values_to = "absorbance")
wvlng = str_split_fixed(scan_45_animate$wavelength,pattern = "(nm)$", n=2)
wvlng = wvlng[,1]
scan_45_animate$wavelength = wvlng
scan_45_animate$wavelength = as.numeric(scan_45_animate$wavelength)

#subset data to a smaller interval (one day)
sub_45 = interval(start="2020-04-10 15:19:53", end="2020-04-24 9:49:52", tz="Etc/GMT+4")
scan_45_animate_sub = scan_45_animate[scan_45_animate$DateTime %within% sub_45,]


# Create animated GIF of wavelength vs. absorption over time
#install.packages('gganimate')

p <- ggplot(scan_45_animate_sub, aes(x = wavelength, y = absorbance)) +
  geom_line(aes(group=DateTime))
a <- p + transition_time(DateTime) +
  labs(title = "DateTime: {frame_time}") +
  scale_x_continuous(breaks = c(seq(200,750,100)), limits = c(200,750)) +
  ease_aes('cubic-in-out')
animate(a, nframes=150, fps=6)
anim_save("4.5_Apr10_Apr24.gif", animation = last_animation())

# More plots!
scan_45_animate2 = filter(scan_45_animate, wavelength==200|wavelength==300|wavelength==400
                      |wavelength==500|wavelength==600|wavelength==700)
#Cleaning times  
cleaning_scan45 = as.POSIXct(c("2020-04-20 12:40"), tz="Etc/GMT+4")

png("4.5m_ts_Apr10_24.png",width = 7, height = 3, units = 'in', res = 300)
ggplot(scan_45_animate2, aes(x=DateTime,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))+
  geom_vline(xintercept = cleaning_scan45, linetype="dotted", 
             color = "black", size=0.6)
dev.off()

####Catwalk Data####

catheader<-read.csv("https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
catdata<-read.csv("https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting

sub_cat= interval(start="2020-04-10 15:20:00", end="2020-08-03 09:40:00", tz="Etc/GMT+4")
catdata$TIMESTAMP=ymd_hms(catdata$TIMESTAMP, tz="Etc/GMT+4")
catdata_muxmatch = catdata[catdata$TIMESTAMP %within% sub_cat,]

for(j in 5:39){
  catdata_muxmatch[,j]<-as.numeric(levels(catdata_muxmatch[,j]))[catdata_muxmatch[,j]]#need to set all columns to numeric values
}


png("catwalk_2020_muxmatch.png",width = 8, height = 8, units = 'in', res = 300)
par(mfrow=c(3,1))
plot(catdata_muxmatch$TIMESTAMP,catdata_muxmatch$doobs_9, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,13))
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$doobs_5, col="black", type='l', lwd=1.5)
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$doobs_1, col="magenta", type='l', lwd=1.5)
legend("topleft", c("1.6m", "5m", "9m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)

plot(catdata_muxmatch$TIMESTAMP,catdata_muxmatch$Cond_1, main="Cond, SpCond, TDS @ 1.6m", xlab="Time", ylab="uS/cm or mg/L", type='l', col="red", lwd=1.5, ylim=c(-0.5,60))
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$SpCond_1, col="black", type='l', lwd=1.5)
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$TDS_1, col="orange", type='l', lwd=1.5)
legend("topleft", c("TDS", "SpCond", "Cond"), text.col=c("orange", "black","red"), x.intersp=0.001)

#wavelength 435-445
plot(catdata_muxmatch$TIMESTAMP,catdata_muxmatch$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='l', col="green", lwd=1.5, ylim=c(-0.5,35))
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$BGAPC_1, col="blue", type='l', lwd=1.5)
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$fDOM_QSU_1, col="firebrick4", type='l', lwd=1.5)
legend("topleft", c("Chla", "Phyco", "fDOM"), text.col=c("green", "blue", "firebrick4"), x.intersp=0.001)

dev.off()

png("mux16_2020_muxmatch2.png",width = 9, height = 4, units = 'in', res = 300)

ggplot(obs_animate2, aes(x=Date.Time,y=absorbance))+
  geom_line(aes(colour=factor(wavelength)))+
  geom_vline(xintercept = cleaning, linetype="dotted", 
             color = "black", size=0.6)

dev.off()

png("EXObio_2020_muxmatch.png",width = 9, height = 4, units = 'in', res = 300)

#wavelength 435-445
plot(catdata_muxmatch$TIMESTAMP,catdata_muxmatch$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='l', col="green", lwd=1.5, ylim=c(-0.5,35))
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$BGAPC_1, col="blue", type='l', lwd=1.5)
points(catdata_muxmatch$TIMESTAMP, catdata_muxmatch$fDOM_QSU_1, col="firebrick4", type='l', lwd=1.5)
legend("topleft", c("Chla", "Phyco", "fDOM"), text.col=c("green", "blue", "firebrick4"), x.intersp=0.001)

dev.off()

