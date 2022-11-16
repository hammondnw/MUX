####################################################
# Objective: Fix timestamp issues with the FP file from June 10th, 2021 
# when the MUX did not reset properly
# Affected File: 106104B8.FP
#####################################################

# Load Packages
library(tidyverse)
library(lubridate)

# Set working directory
setwd('./MagicData')

# Read in an example file "10610000.FP" to demonstrate that the Date.Time and Measurement time are always
# a consistent duration apart
mux_colnames = c("DateTime", "Status", paste0(as.character(c(seq(200,750, by = 2.5))),"nm"), "Valve","MeasurementTime")
example<-read.table(file=paste0("./FP_2021/","10610000.FP"),skip=2,header=FALSE, row.names = NULL, sep = "\t")
names(example) <- mux_colnames
example$DateTime=ymd_hms(example$DateTime, tz="Etc/GMT+4")
example$MeasurementTime =ymd_hms(example$MeasurementTime, tz="Etc/GMT+4")

example = example %>% mutate(time_diff = MeasurementTime - DateTime)

# It appears that the difference between MeasurementTime and DateTime drifts by ~ 3 sec over the course of the day,
# but this is negligible so I think it is safe to use this approach.

# Read in the affected file
old_fp <-read.table(file=paste0("./FP_2021/Bad FP Files/","106104B8_bad.FP"),skip=2,header=FALSE, row.names = NULL, sep = "\t")
names(old_fp) <- mux_colnames
old_fp$DateTime=ymd_hms(old_fp$DateTime, tz="Etc/GMT+4")
old_fp$MeasurementTime =ymd_hms(old_fp$MeasurementTime, tz="Etc/GMT+4")

# Mutate the Measurement time column to replace the Date.Time column and subtract 9.5 minutes (570 sec)
# 9.5 minutes was the average time diff btwn the DateTime and MeasurementTime
new_fp = old_fp %>% mutate(DateTime = MeasurementTime - 570)
# Add the ID row back in (to make it match format of other MUX files)
new_fp = new_fp %>% add_row(Status = "18200006_350_0x0101_spectro::lyser_RIV1000FV2",.before = 1)

# Flag the original file as "bad" and the new file as "edited"
write.table(new_fp, file = "./FP_2021/106104B8_edited.FP", quote = FALSE, row.names = FALSE, sep = "\t")

