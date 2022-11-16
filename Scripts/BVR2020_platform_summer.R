#script written to create daily figures for BVR
#written by CCC, BB, Vahid-Dan, ABP
#10 SEP 2020

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
library(plyr)
library(readr)
library(dplyr)

#time to now play with BVR data!
#Gateway has missing data sections so combine manual data for EDI

#put manual data from BVR platform into a file 
mydir = "BVRPlatform"
myfiles = list.files(path=mydir, pattern="CR6*", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
bvrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfilesBVR)){
  bvrheader2<-read.csv(myfilesBVR[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  bvrdata2<-read.csv(myfilesBVR[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting
  bvrdata3=rbind(bvrdata2, bvrdata3)
}

#get rid of duplicates
bvrdata=bvrdata3[!duplicated(bvrdata3$TIMESTAMP), ]

#create CSV of manual downloads which can be combined with Github files to fill in missing gaps

write.csv(bvrdata, "BVRplatform_manual_2020.csv")

#upload the current BVR data from GitHub
download.file('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv', "BVRplatform.csv") 

bvrheader1<-read.csv("BVRplatform.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata1<-read.csv("BVRplatform.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata1)<-names(bvrheader1) #combine the names to deal with Campbell logger formatting



#put manual data from BVR platform into a file 
mydir = "BVRPlatform"
myfiles = list.files(path=mydir, pattern="CR6*", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
myfilesBVR <- myfiles[ !grepl("CR6_BVR_TempTest*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
bvrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfilesBVR)){
  bvrheader2<-read.csv(myfilesBVR[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  bvrdata2<-read.csv(myfilesBVR[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting
  bvrdata3=rbind(bvrdata2, bvrdata3)
}

#combine manual and most recent files
bvrdata=rbind(bvrdata3, bvrdata1)

#taking out the duplicate values  
obs1=bvrdata[!duplicated(bvrdata$TIMESTAMP), ]


#change the date from character to unknown making it easier to graph
obs1$TIMESTAMP <- as.POSIXct(obs1$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+4") 


#trying to do it in ggplot
#ggplot(obs1, aes(x=TIMESTAMP)) + 
  #geom_line(aes(y = wtr_1), color = "firebrick4") +
  #geom_line(aes(y = wtr_2), color="firebrick1") + 
  #geom_line(aes(y = wtr_3), color="DarkOrange1") + 
  #geom_line(aes(y = wtr_4), color="gold") + 
  #geom_line(aes(y = wtr_5), color="greenyellow") + 
  #geom_line(aes(y = wtr_6), color="medium sea green") + 
  #geom_line(aes(y = wtr_7), color="sea green") + 
  #geom_line(aes(y = wtr_8), color="DeepSkyBlue4") + 
  #geom_line(aes(y = wtr_9), color="blue2") + 
  #geom_line(aes(y = wtr_10), color="blue4") + 
  #geom_line(aes(y = wtr_11), color="navy") + 
  #geom_line(aes(y = wtr_12), color="magenta2") + 
  #geom_line(aes(y = wtr_13), color="darkmagenta") + 
  #labs(y = "degrees C", title="BVR Water Temp", x="") +
  #scale_colour_manual(values=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                    #"DeepSkyBlue4", "blue2", "blue4", "navy", "magenta2", "darkmagenta")) +
  #scale_colour_manual(values=c("1m"="firebrick4", "2m"="firebrick1", "3m"="DarkOrange1", "4m"="gold", "5m"="greenyellow", "6m"="medium sea green", "7m"="sea green",
                               #"8m"="DeepSkyBlue4", "9m"="blue2", "10m"="blue4", "11m"="navy", "12m"="magenta2", "13m"="darkmagenta")) 
  

#making a figure the same as the daily email. This is from the BVR data from the summer 2020
pdf(paste0("BVRDataFiguresSummer_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
par(mfrow=c(3,2))

plot(obs1$TIMESTAMP,obs1$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='p')
plot(obs1$TIMESTAMP,obs1$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='p')
if(min(tail(na.omit(obs1$BattV)))<11.5){
  mtext("Battery Charge Low", side = 3, col="red")
}
plot(obs1$TIMESTAMP,obs1$EXO_battery, main="EXO Battery", xlab="Time", ylab="Volts", type='p')
plot(obs1$TIMESTAMP,obs1$EXO_cablepower, main="EXO Cable Power", xlab="Time", ylab="Volts", type='p')
plot(obs1$TIMESTAMP,obs1$EXO_depth, main="EXO Depth", xlab="Time", ylab="Meters", type='p')

plot(obs1$TIMESTAMP,obs1$EXO_pressure, main="Sonde Pressure", xlab="Time", ylab="psi", type='p', ylim=c(-1,25))
points(obs1$TIMESTAMP, obs1$Lvl_psi, col="blue4", type='p')
legend("topleft", c("1.5m EXO", "13m PT"), text.col=c("black", "blue4"), x.intersp=0.001)

par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
plot(obs1$TIMESTAMP,obs1$dotemp_13, main="Water temp of sondes", xlab="Time", ylab="degrees C", type='p', col="medium sea green", lwd=1.5, ylim=c(0,45))
points(obs1$TIMESTAMP, obs1$dotemp_6, col="black", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$EXO_wtr_1, col="magenta", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_pt_13, col="blue4", type='p', lwd=1.5)
legend("topleft", c("1.5m EXO", "6m DO", "13m DO", "13m PT"), text.col=c("magenta", "black", "medium sea green", "blue4"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$doobs_13, main="DO", xlab="Time", ylab="mg/L", type='p', col="medium sea green", lwd=1.5, ylim=c(-0.5,18))
points(obs1$TIMESTAMP, obs1$doobs_6, col="black", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$doobs_1, col="magenta", type='p', lwd=1.5)
legend("topleft", c("1m", "6m", "13m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$dosat_13, main="DO % saturation", xlab="Time", ylab="% saturation", type='p', col="medium sea green", lwd=1.5, ylim=c(-0.5,200))
points(obs1$TIMESTAMP, obs1$dosat_6, col="black", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$dosat_1, col="magenta", type='p', lwd=1.5)
legend("topleft", c("1m", "6m", "13m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$Cond_1, main="Cond, SpCond, TDS @ 1m", xlab="Time", ylab="uS/cm or mg/L", type='p', col="red", lwd=1.5, ylim=c(-0.5,60))
points(obs1$TIMESTAMP, obs1$SpCond_1, col="black", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$TDS_1, col="orange", type='p', lwd=1.5)
legend("topleft", c("TDS", "SpCond", "Cond"), text.col=c("orange", "black","red"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='p', col="green", lwd=1.5, ylim=c(-0.5,35))
points(obs1$TIMESTAMP, obs1$BGAPC_1, col="blue", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$fDOM_QSU_1, col="firebrick4", type='p', lwd=1.5)
legend("topleft", c("Chla", "Phyco", "fDOM"), text.col=c("green", "blue", "firebrick4"), x.intersp=0.001)

par(mfrow=c(1,1))
par(oma=c(1,1,1,4))
plot(obs1$TIMESTAMP,obs1$wtr_1, main="Water Temp", xlab="Time", ylab="degrees C", type='p', col="firebrick4", lwd=1.5, ylim=c(0,40))
points(obs1$TIMESTAMP, obs1$wtr_2, col="firebrick1", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_3, col="DarkOrange1", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_4, col="gold", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_5, col="greenyellow", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_6, col="medium sea green", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_7, col="sea green", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_8, col="DeepSkyBlue4", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_9, col="blue2", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_10, col="blue4", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_11, col="navy", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_12, col="magenta2", type='p', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_13, col="darkmagenta", type='p', lwd=1.5)

par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right",c("1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m", "10m", "11m", "12m", "13m"),
       text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                  "DeepSkyBlue4", "blue2", "blue4", "navy", "magenta2", "darkmagenta"),
       cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')

dev.off() #file made!

#Create Air temp and Do for the summer
#change to line and not point
#get rid of air temp in graph

#messy way of getting rid of broken temp string data
obs1[obs1==0]<- NA
obs1[obs1 == 10.86135] <- NA  
obs1[obs1 == 10.44107] <- NA 
obs1[obs1 == 10.44106] <- NA
obs1[obs1 == 10.28613] <- NA  
obs1[obs1 == 10.19531] <- NA  
obs1[obs1 == 10.04641] <- NA  
obs1[obs1 == 9.759333] <- NA 
obs1[obs1 == 30.77133] <- NA  
obs1[obs1 == 32.61395] <- NA  
obs1[obs1 == 33.0209] <- NA 



#Water temp
par(mfrow=c(1,1))
par(oma=c(1,1,1,4))
#plot(obs1$TIMESTAMP,obs1$wtr_1, main="Water Temp", xlab="Time", ylab="degrees C", type='p', col="firebrick4", lwd=1.5, ylim=c(0,40))
#points(obs1$TIMESTAMP, obs1$wtr_2, col="firebrick1", type='p', lwd=1.5)
plot(obs1$TIMESTAMP, obs1$wtr_3, main="Water Temp", xlab="Time", ylab="degrees C",type='l', col="DarkOrange1",  lwd=1.5, ylim=c(0,40))
points(obs1$TIMESTAMP, obs1$wtr_4, col="gold", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_5, col="greenyellow", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_6, col="medium sea green", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_7, col="sea green", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_8, col="DeepSkyBlue4", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_9, col="blue2", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_10, col="blue4", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_11, col="navy", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_12, col="magenta2", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_13, col="darkmagenta", type='l', lwd=1.5)

par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right",c("1m", "2m", "3m", "4m", "5m", "6m","7m", "8m", "9m", "10m", "11m"),
       text.col=c("DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                  "DeepSkyBlue4", "blue2", "blue4", "navy", "magenta2", "darkmagenta"),
       cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
dev.off()

#DO
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
plot(obs1$TIMESTAMP,obs1$doobs_13, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,18))
points(obs1$TIMESTAMP, obs1$doobs_6, col="black", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$doobs_1, col="magenta", type='l', lwd=1.5)
legend("topleft", c("1.5m", "4m", "11m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)

#DOsat 
plot(obs1$TIMESTAMP,obs1$dosat_13, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,200))
points(obs1$TIMESTAMP, obs1$dosat_6, col="black", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$dosat_1, col="magenta", type='l', lwd=1.5)
legend("topleft", c("1m", "6m", "13m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)
dev.off()
