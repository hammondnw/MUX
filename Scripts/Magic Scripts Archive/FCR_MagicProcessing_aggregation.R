#Script to work with Magic data from Fall 2018

#Let's see if anything is worth anything

#Tasks:
#1. Load in data in legit way
#2. Post acid cleaning (October 10th ish), plot pre and post cleaning air valve (what is the best absorbance col to use?)
#3. Plot DI valve
#4. Plot Surface valve
#5. Compare magic data to DO + temp data at 1, 5, and 9m

#packages need
library(lubridate)

#loading in data
setwd('./MagicData')

#create list with files after cleaning protocol
magicfiles<-list.files(path=".", pattern = "810*")
#this isn't working how I want. Will fix by limiting time for now
fp.files <- magicfiles[grep(".FP", magicfiles, fixed=T)] #takes above list and keeps only FP file

#alternative loading. loads in all FP files. still limit by time after agg
#magicFPfiles<-list.files(magicfiles, pattern = ".FP")

###putting in met merge script as place holder
obs<-read.table(file=fp.files[1],skip=1,header=TRUE, row.names = NULL, sep = "\t") #read in first file
obs$Date.Time=ymd_hms(obs$Date.Time, tz = "Etc/GMT+4")

for(i in 2:length(fp.files)){ #reads in all files within folder in Github
  temp<-read.table(file=fp.files[i],skip=1,header=TRUE, row.names = NULL, sep = "\t")
  temp$Date.Time=ymd_hms(temp$Date.Time, tz = "Etc/GMT+4")
  obs<-rbind(obs,temp)
  #print(i)
}

for(i in 2:length(obs$Date.Time)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
  if(obs$Date.Time[i]-obs$Date.Time[i-1]>4.5){
    print(c(obs$Date.Time[i-1],obs$Date.Time[i]))
  }
}
#suspicious, lot of data separated by large time chunks. ~20-30 min


#limit data to after Oct 19 16:30
#make sure all pumping was the same?
Fall_magic=obs[obs$Date.Time> "2018-10-19 16:30:00",]
Fall_magic=Fall_magic[order(Fall_magic$Date.Time),] #orders by time
Fall_magic=unique(Fall_magic)

#read in log files
log_files=list.files(path = ".", pattern = "*LOG.TXT")
logs<-read.table(file=log_files[1],header=F, row.names = NULL, sep = ",") #read in first file
logs$Date.Time=ymd_hms(obs$Date.Time, tz = "Etc/GMT+4")

for(i in 2:length(fp.files)){ #reads in all files within folder in Github
  temp<-read.table(file=fp.files[i],skip=1,header=TRUE, row.names = NULL, sep = "\t")
  temp$Date.Time=ymd_hms(temp$Date.Time, tz = "Etc/GMT+4")
  obs<-rbind(obs,temp)
  #print(i)
}

setwd("..") #goes up one directory so that metadata file is not written to /MetStationData



#write.csv(obs,"AllRawMetData.csv", row.names = FALSE) #create csv with all raw appended data
#this is the LEVEL_ZERO dataset: i.e., no QA/QC applied, complete dataset but has errors
