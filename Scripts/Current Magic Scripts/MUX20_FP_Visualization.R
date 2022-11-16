# MUX FP visualization

#packages
library(lubridate)
library(tidyverse)

#set working directory
setwd('C:/Users/hammo/Documents/Magic Sensor PLSR')
folder = 'C:/Users/hammo/Documents/Magic Sensor PLSR/Data/'

MUX = read_csv(paste0(folder,"MUX_FP_TS_2020.csv"))
MUX = MUX %>% select(-c(`...1`))

mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5)))), "Valve","Measurement time")
names(MUX) <- mux_colnames
MUX$DateTime=ymd_hms(MUX$DateTime, tz="America/New_York")


mux_only=MUX
mux_only=mux_only[order(mux_only$DateTime),]

mux_only = mux_only %>% filter(DateTime > ymd_hms("2020-10-15 12:00:00",tz="America/New_York"))

#create a data frame of valve number and depth
valve_depth <- data.frame(
  Valve = c (1:12), 
  Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0", "NA", "acid_r", "air","NA", "air"),
  stringsAsFactors = FALSE
)

#put the data in long format and add valve depth

mux_only_long=mux_only%>%
  pivot_longer(cols=3:223, names_to = "wavelength", values_to = "absorbance")%>%
  left_join(valve_depth, by="Valve")%>%
  filter(wavelength %in% c("200", "300", "400", "500", "600", "700"))%>%
  filter(Depth %in% c('0.1','1.6','3.8','5.0','6.2','8.0','9.0','air','water_r', "acid_r"))

# Vector of cleaning times
cleaning = tibble(ymd_hms(c("2020-10-17 13:00:00", "2020-10-19 11:26:00", "2020-10-21 11:27:00", "2020-10-23 10:35:00",
                                   "2020-10-26 10:40:00","2020-10-28 09:17:00", "2020-10-30 12:00:00","2020-11-02 09:40:00",
                                   "2020-11-04 10:59:00"), tz="America/New_York"))
colnames(cleaning)<- "DateTime"


#### Create  a multipanel plot of absorbance over time separated by depth 
png("Turnover_20_fullTS_022522.png",width = 13, height = 6, units = 'in', res = 300)
ggplot(mux_only_long, aes(x=DateTime, y=absorbance, color=wavelength)) + 
  geom_point() +
  geom_vline(xintercept = cleaning$DateTime, linetype="dotted", 
             color = "black", size=0.6)+
  facet_wrap(facets = vars(Depth), ncol = 2, scales = "free_y")
dev.off()



# For single-depth plots
DI = mux_only_long%>%
  filter(Depth %in% 'water_r')
air = mux_only_long%>%
  filter(Depth %in% 'air')
surface = mux_only_long%>%
  filter(Depth %in% '0.1')
one_m = mux_only_long%>%
  filter(Depth %in% '1.6')
three_m = mux_only_long%>%
  filter(Depth %in% '3.8')
five_m = mux_only_long%>%
  filter(Depth %in% c('5.0'))
six_m = mux_only_long%>%
  filter(Depth %in% '6.2')
eight_m = mux_only_long%>%
  filter(Depth %in% '8.0')
nine_m = mux_only_long%>%
  filter(Depth %in% '9.0')


#Create a plot of every 100nm wavelengths for a specific depth
png("mux_cleaning_raw_DI.png",width = 9, height = 4, units = 'in', res = 300) 
ggplot(one_m, aes(x=DateTime, y=absorbance, color=wavelength))+
  geom_point()
#geom_vline(data=cleaning, aes(xintercept=DateTime) , linetype="dashed", color="black", size=0.5)
dev.off()






###### Pump log load ######
log_files=list.files(path = ".", pattern = glob2rx("201*MUX.TXT"))
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