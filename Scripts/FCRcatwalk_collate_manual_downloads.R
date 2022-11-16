#script written to combine manual downlaods to fill in gaps of missing data from the gateway
#written by CCC, BB, Vahid-Dan, ABP
#09 JUN 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)

#read in the collated manual file that we are adding to
FCRman_data=read.csv("CR6_Files/FCRcatwalk_manual_2021.csv")


#time to now play with FCR data!
#Gateway has missing data sections so combine manual data for EDI

#put manual data from FCR platform into a file 
mydir = "CR6_Files/Catwalk"
myfiles = list.files(path=mydir, pattern=".dat", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
myfilesFCR <- myfiles[ !grepl("CR6_FCRcatwalk_Catwalk_20220505.dat*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
fcrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfilesFCR)){
  fcrheader2<-read.csv(myfilesFCR[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  fcrdata2<-read.csv(myfilesFCR[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(fcrdata2)<-names(fcrheader2) #combine the names to deal with Campbell logger formatting
  fcrdata3=rbind(fcrdata2, fcrdata3)
}

#Merge the large data set FCRman
fcr=rbind(FCRman_data,fcrdata3)

#add the turbidity column
fcr$Turbidity_FNU_1=NA

#reorder the turbidity column

fcrre=fcr%>%
  select("TIMESTAMP","RECORD","BattV","PTemp_C","wtr_surface","wtr_1","wtr_2","wtr_3","wtr_4","wtr_5","wtr_6","wtr_7","wtr_8",
         "wtr_9","doobs_5","dosat_5","dotemp_5","doobs_9","dosat_9","dotemp_9","EXO_Date","EXO_Time","EXO_wtr_1","Cond_1","SpCond_1","TDS_1","dosat_1","doobs_1","Chla_RFU_1","Chla_1","BGAPC_RFU_1","BGAPC_1","fDOM_RFU_1","fDOM_QSU_1","Turbidity_FNU_1","EXO_pressure","EXO_depth","EXO_battery","EXO_cablepower","EXO_wiper","Lvl_psi","wtr_pt_9")

#add in the file with the turbidity sensor
mayheader2<-read.csv("CR6_Files/Catwalk/CR6_FCRcatwalk_Catwalk_20220505.dat", skip=1, as.is=T) #get header minus wonky Campbell rows
maydata2<-read.csv("CR6_Files/Catwalk/CR6_FCRcatwalk_Catwalk_20220505.dat", skip=4, header=F) #get data minus wonky Campbell rows
names(maydata2)<-names(mayheader2)

#combine from above
f=rbind(fcrre, maydata2)

write.csv(f, "CR6_Files/FCRcatwalk_manual_2022.csv", row.names=FALSE)


#put manual data from FCR pressure sensor into a file 
mydir2 = "CR6_Files/Pressure_Sensor"
myfiles2 = list.files(path=mydir2, pattern="", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
presdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(j in 1:length(myfiles2)){
  presheader2<-read.csv(myfiles2[j], skip=1, as.is=T) #get header minus wonky Campbell rows
  presdata2<-read.csv(myfiles2[j], skip=4, header=F) #get data minus wonky Campbell rows
  names(presdata2)<-names(presheader2) #combine the names to deal with Campbell logger formatting
  presdata3=rbind(presdata2, presdata3)
}

#get rid of duplicates
presdata=presdata3[!duplicated(presdata3$TIMESTAMP), ]

#merge the catwalk files and the pressure sensor

FCR=merge(fcrdata,presdata, all.x=T)

FCR=rbind(FCRman_data,fcrdata3)

FCR2=FCR%>%
  filter(TIMESTAMP!="")

#create CSV of manual downloads which can be combined with Github files to fill in missing gaps

write.csv(FCR2, "CR6_Files/FCRcatwalk_manual_2021.csv", row.names=FALSE)
