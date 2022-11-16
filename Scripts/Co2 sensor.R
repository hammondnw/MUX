#Co2 Vaisala Sensor Time Series
#ABP
#16 NOV 20


library(lubridate)
library(tidyverse)
library(ggplot2)

#load in data from the Vaisala Sensor

mydir = "FCR_CO2sensors/Vaisala Sensor.backup"
myfiles = list.files(path=mydir, pattern="", full.names=TRUE)

#combine the files
#create an out.file for the combined data
out.file<-""
#for loop to combine the files
for(i in 1:length(myfiles)){
  file <- read.csv(myfiles[i], header=F, skip=4)
  out.file <- rbind(out.file, file)
}


#Naming the header because they have to be eliminated above to combine the files
colnames(out.file)=c("TIMESTAMP","RECORD","batt_volt_Min","PTemp","CO2_1_Avg","CO2_2_Avg")
#take out duplicates
out.file=distinct(out.file)
#make the dates cooperate
out.file$TIMESTAMP <- as.POSIXct(out.file$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+4")
#add a month column to make sorting easier
out.file$Month=months(out.file$TIMESTAMP)
#change from character to Numeric
out.file$CO2_2_Avg=as.numeric(out.file$CO2_2_Avg)

#create the graph
axs=ggplot(out.file, aes(x=TIMESTAMP, y=CO2_2_Avg, col=CO2_2_Avg)) +
  geom_point()
  
axs 

axs_2=out.file%>%
  filter(TIMESTAMP>"2022-01-10 00:00")%>%
  ggplot(., aes(x=TIMESTAMP, y=CO2_2_Avg, col=CO2_2_Avg)) +
  geom_point()

axs_2

