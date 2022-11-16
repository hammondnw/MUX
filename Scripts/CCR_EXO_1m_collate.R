#combinig EXO data which can then be used to add to the ccre-waterquality.csv file

library(lubridate)
library(tidyverse)
library(ggplot2)


#load in data from the EXO Sensor

#read in the current file
current=read.csv("CCR_manual_downloads/CCR_1_5_EXO_downloads/Collated_CCR_1_5_EXO.csv")

#All files including the one from 08 Nov 21(last reading actually happened on 16:10EST ) is EDT 
#so need to change to EST

mydir = "CCR_manual_downloads/CCR_1_5_EXO_downloads"
myfiles = list.files(path=mydir, pattern="CCR_EXO_1_5_20*", full.names=TRUE)

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#combine the files
#create an out.file for the combined data
out.file<-""
#for loop to combine the files
for(i in 1:length(myfiles)){
  file <- read.csv(myfiles[i], header=T, skip=17, skipNul = TRUE)
    if (length(names(file))<23){
      file$Wiper.Position.volt<-"NA"#remove extra column
    file=file%>%relocate(Wiper.Position.volt, .after=TDS.mg.L)
    }

  out.file <- rbind(out.file,file)
}


#clean up the created file
CCR_1_5_EXO=out.file%>%
  filter(Site.Name!="")%>%
  select(-c(Site.Name,Time..Fract..Sec.,ODO...local,Sal.psu,))%>%
  unite(., col="TIMESTAMP", c("Date..MM.DD.YYYY.","Time..HH.mm.ss."), sep=" ")
  
#Convert TIMESTAMP
CCR_1_5_EXO$TIMESTAMP <- as.POSIXct(strptime(CCR_1_5_EXO$TIMESTAMP, format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+4"))
CCR_1_5_EXO$TIMESTAMP=as.character(CCR_1_5_EXO$TIMESTAMP)



#change TIMESTAMP to EST and change format. Most likely will only have to be until 08 Nov 21 download 11/8/2021 17:10:00
#CCR_timechange=max(which(CCR_1_5_EXO$TIMESTAMP=="2021-11-08 17:10:00")) #shows time point when met station was switched from GMT -4 to GMT -5
#CCR_1_5_EXO$TIMESTAMP <- as.POSIXct(strptime(CCR_1_5_EXO$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+4")) #get dates aligned
#CCR_1_5_EXO$TIMESTAMP[c(1:CCR_timechange-1)]<-with_tz(force_tz(CCR_1_5_EXO$TIMESTAMP[c(1:CCR_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

#CCR_1_5_EXO$TIMESTAMP <- as.POSIXct(strptime(CCR_1_5_EXO$TIMESTAMP, format = "%m/%d/%Y %H:%M:%S", tz = "Etc/GMT+5"))#format
#CCR_1_5_EXO$TIMESTAMP<-with_tz(force_tz(CCR_1_5_EXO$TIMESTAMP,"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

#Naming the header to match what is on the data logger
colnames(CCR_1_5_EXO)=c("TIMESTAMP","Chla_RFU_1","Cond_1","EXO_depth_1","fDOM_QSU_1","fDOM_RFU_1","nLF.Cond.uS.cm",
"dosat_1","doobs_1","EXO_pressure_1","SpCond_1","BGAPC_RFU_1","TDS_1","EXO_wiper_1",
"EXO_wtr_1","Vertical.Position.m","EXO_battery_1","EXO_cablepower_1")

#bind the current file to new data
now=rbind(current, CCR_1_5_EXO)
         

#change everything else to numeric
#can change later
#CCR_1_5_EXO[2:23]=as

write.csv(now, "CCR_manual_downloads/CCR_1_5_EXO_downloads/Collated_CCR_1_5_EXO.csv",na="NAN", row.names = FALSE)
