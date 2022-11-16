#### Script for calculating thermocline depth, etc. for MUX Turnover Deployment ####
### Author: Nick Hammond
### Last Edited: 01/31/2022

library(tidyverse)
library(lubridate)
library(rLakeAnalyzer)

setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")


# Read in catwalk data
catwalk_full = read.csv(paste0(getwd(),"/Data/Covariate data/Catwalk_EDI_2020.csv"))


# Select the variables we want
catwalk = catwalk_full %>% select(Reservoir,Site,DateTime,ThermistorTemp_C_surface,
                                 ThermistorTemp_C_1,ThermistorTemp_C_2, ThermistorTemp_C_3,
                                 ThermistorTemp_C_4,ThermistorTemp_C_5, ThermistorTemp_C_6,
                                 ThermistorTemp_C_7, ThermistorTemp_C_8, ThermistorTemp_C_9)

# Convert DateTime to PosixCT
catwalk$DateTime = mdy_hm(catwalk$DateTime, tz="Etc/GMT+5") # GMT+5 because the sensors are on EST?


# Select the reservoir, site, and date range we want
catwalk = catwalk %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2020-10-16 13:10:00") %>% #"2020-10-16 13:10:00"
  filter(DateTime<"2020-11-09 14:00:00")

# Get rid of 'reservoir' and 'site' columns
catwalk = catwalk %>% select(!c("Reservoir","Site"))

# Rename column headers to match LakeAnalyzer format
colnames(catwalk) = c("datetime","wtr_0.1","wtr_1.0","wtr_2.0","wtr_3.0","wtr_4.0","wtr_5.0","wtr_6.0",
                      "wtr_7.0","wtr_8.0","wtr_9.0")
# Calculate Thermocline depth
t.d = ts.thermo.depth(catwalk,na.rm = T)
plot(t.d$datetime, t.d$thermo.depth, type='l', ylab='Thermocline Depth (m)', xlab='Date')

# Calculate metalimnion depth
m.d = ts.meta.depths(catwalk, na.rm = T)
plot(m.d$datetime, m.d$top, type='l', ylab='Meta Depths (m)', xlab='Date', col='blue')
lines(m.d$datetime, m.d$bottom, col='red')

m.d = m.d %>% mutate(diff = bottom - top)
plot(m.d$datetime, m.d$diff, type='l', ylab='Meta size (m)', xlab='Date', col='blue')

#write to csv

write.csv(t.d,'FCR_ThermoDepth_MUX20_013122.csv', row.names = F)


