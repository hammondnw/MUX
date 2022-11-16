##BVR temp string collation script
#Author: Bethany Bookout
#Created: 12 FEB 2020
#Last Edited: 12 FEB 2020

library(plyr)
library(readr)
library(lubridate)
library(dplyr)
library(magrittr)
library(driftR) #package to read in EXO and hobo data frames

#load in all hobo temp string data
#how do I want to do this?? by depth? then merge by time?
mydir = "BVR_SensorString"
myfiles = list.files(path=mydir, pattern="Hobo*", full.names=TRUE)
myfiles

dat_csv = ldply(myfiles, read_csv)
dat_csv

#load minidot DO data
DO_5m=read.table("BVR_SensorString/BVR_DO_5m_20181206_20191206.TXT", sep=",/", skip = 6, header = T, stringsAsFactors=F)
DO_5m=DO_5m[-1,]
names(DO_5m)=c("DateTime_Unix","DateTime_UTC","DateTime_EST", "Battery_V", "wtr_C", "doobs_5m", "dosat_5", "Q")
str(DO_5m)

DO_5m$DateTime_UTC=ymd_hms(DO_5m$DateTime_UTC)
DO_5m$DateTime_EST=ymd_hms(DO_5m$DateTime_EST)
DO_5m %>% mutate_if(is.factor,as.numeric) %>% str()
DO_5m$DateTime_Unix_compare=as_datetime(DO_5m$DateTime_Unix,origin = lubridate::origin, tz = "UTC")

#load EXO data
EXO_BVR=read.table("BVR_SensorString/BVR_1_5m_20191007_20191212.csv", sep= ",", skip = 8, header = T, blank.lines.skip = T)
#what is wrong with this loading?????
EXO_BVR=dr_read("BVR_SensorString/BVR_1_5m_20191007_20191212.csv", instrument = "EXO")
EXO_FCR=read.csv("EXOData/EXO_FCR_preCR6datacompilation_20180828.csv", skip=5, header = F)
EXO_BVR=read.csv("BVR_SensorString/BVR_1_5m_20191007_20191212.csv", skip=5, header = F)  
  
