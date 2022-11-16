#### Script for calculating thermocline depth, etc. for MUX Turnover Deployment ####
### Author: Nick Hammond
### Last Edited: 01/31/2022

library(tidyverse)
library(lubridate)
library(rLakeAnalyzer)

setwd("C:/Users/hammo/Documents/Magic Sensor PLSR/")


# Read in catwalk data
catwalk_full = read.csv(paste0(getwd(),"/Data/Covariate data/FCR_Catwalk_2018_2021.csv"))


# Select the variables we want
catwalk = catwalk_full %>% select(Reservoir,Site,DateTime,ThermistorTemp_C_surface,
                                  ThermistorTemp_C_1,ThermistorTemp_C_2, ThermistorTemp_C_3,
                                  ThermistorTemp_C_4,ThermistorTemp_C_5, ThermistorTemp_C_6,
                                  ThermistorTemp_C_7, ThermistorTemp_C_8, ThermistorTemp_C_9)

# Convert DateTime to PosixCT
catwalk$DateTime = ymd_hms(catwalk$DateTime, tz="Etc/GMT+5") # GMT+5 because the sensors are on EST?


# Select the reservoir, site, and date range we want
catwalk = catwalk %>% filter(Reservoir=="FCR" & Site==50) %>%
  filter(DateTime>"2021-05-26 00:00:00") %>% 
  filter(DateTime<"2021-06-21 24:00:00")

# Get rid of 'reservoir' and 'site' columns
catwalk = catwalk %>% select(!c("Reservoir","Site"))

# Rename column headers to match LakeAnalyzer format
colnames(catwalk) = c("datetime","wtr_0.1","wtr_1.0","wtr_2.0","wtr_3.0","wtr_4.0","wtr_5.0","wtr_6.0",
                      "wtr_7.0","wtr_8.0","wtr_9.0")

# Calculate Schmidt stability
#Load bathymetry
bathy = read.csv("C:/Users/hammo/Documents/Magic Sensor PLSR/Data/Covariate data/Bathymetry_comb.csv")%>%
  filter(Reservoir == "FCR") %>% select(Depth_m,SA_m2) %>% 
  rename(depths = Depth_m, areas = SA_m2)

schmidt = ts.schmidt.stability(catwalk, bathy,
                               na.rm = T)


###
# PLOT RESULTS
###
schmidt%>%
  ggplot(aes(x = datetime, y = schmidt.stability))+
  geom_path()


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
write.csv(schmidt,'FCR_SchmidtStability_MUX21_082322.csv', row.names = F)

write.csv(t.d,'FCR_ThermoDepth_MUX20_013122.csv', row.names = F)



#### Abby's code for calculating schmidt stability ####

###
#LOAD DATA
###

ctd <- read.csv("../CTD_season_csvs/CTD_FCR_2022.csv")%>% #load CTD data
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%SZ"),
         Date = as.Date(Date))

#Load bathymetry
bathy = read.csv("../2022_BVR_drawdown_figs/Bathymetry_comb.csv")%>%
  filter(Reservoir == "FCR")



###
# FORMAT DATA
###

#For some reason schmidt doesn't run unless we filter to 1m intervals. The whole mess of code below is just to do this
depths <- seq(0,10, by = 1) #Set up the depths that we want
newDepths <- depths
df.final.1m<- ctd %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[1]))) #Create a new dataframe
df.final.1m$Depth_m <- newDepths[1]
for (i in 2:length(depths)){ #loop through all depths and add the closest values to the final dataframe
  ctd_atThisDepth <- ctd %>% group_by(Date, Reservoir) %>% slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
  ctd_atThisDepth = ctd_atThisDepth%>%
    filter(abs(Depth_m-newDepths[i])<0.1) #only include if the measured depth is within 0.1 of the depth we are labeling it
  ctd_atThisDepth$Depth_m <- newDepths[i]
  df.final.1m <- rbind(df.final.1m,ctd_atThisDepth)
}



###
# CALCULATE SCHMIDT
###

#Once you have a filtered CTD dataset and bathymetry loaded, calculating Schmidt Stability is easy!
schmidt = df.final.1m%>%
  filter(Site == 50)%>% #Make sure we are only looking at Site 50 data
  group_by(Date)%>%
  summarize(schmidt = schmidt.stability(wtr = Temp_C, 
                                        depths = Depth_m,
                                        bthA = bathy$SA_m2, 
                                        bthD = bathy$Depth_m,
                                        sal = rep(0,length(Temp_C))#Salinity is approximately 0 (freshwater)
  )
  )


###
# PLOT RESULTS
###
schmidt%>%
  ggplot(aes(x = Date, y = schmidt))+
  geom_point()
