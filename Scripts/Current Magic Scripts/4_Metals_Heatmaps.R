
#*****************************************************************
#* TITLE:  MUx Manuscript Heat Maps Script
#*                     
#* AUTHORS: Nick Hammond                                         
#* LAST UPDATED: 18 Oct 2022
#*                                    
#* NOTES:  This script utilizes the EDI metals dataset to produce heat maps of:
#*         - Total Fe from 2020-2021
#*         - Total Mn from 2020-2021
#*         Figure 2 in the manuscript
#*                                  
#*        
#*****************************************************************

# load libraries
library(akima)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(grid)
library(colorRamps)
library(RColorBrewer)
library(rLakeAnalyzer)
library(readxl)
library(lubridate)
library(zoo)
library(patchwork)


# Set the WD 
setwd("./")

# Specify directory and file name for data file
pathWQ = "./MagicData/MUX/Figures Files/"
WQ = "Metals_2014_2021.csv"
#Download EDI metals dataset
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/455/6/57912981e3e857c85924484806492446" 
infile1 <- c(paste0(pathWQ,WQ,sep=""))
download.file(inUrl1,infile1,method="curl")

# load in metal data from FCR
FCR <- read_csv(paste(pathWQ,"Metals_2014_2021.csv",sep="")) %>% na.omit(FCR)

# Filter to include desired reservoir (FCR), site (50), and year (2020-2021)
FCR$DateTime <- ymd_hms(FCR$DateTime,tz="America/New_York")
FCR <- FCR %>% filter(Reservoir=="FCR") %>%
  filter(Site==50) %>% 
  filter(DateTime > "2020-01-01 00:00:00") 
#filter(DateTime < "2021-01-01 00:00:00")


# Select and make each variable a separate dataframe
# I have done this for the heatmap plotting purposes. 
TFe <- select(FCR, DateTime, Depth_m, TFe_mgL) 
TMn <- select(FCR, DateTime, Depth_m, TMn_mgL)
SFe <- select(FCR, DateTime, Depth_m, SFe_mgL)
SMn <- select(FCR, DateTime, Depth_m, SMn_mgL)
DateTime_u <- as.data.frame(unique(date(ymd_hms(FCR$DateTime))))
colnames(DateTime_u) <- "DateTime"
DateTime_x <- date(ymd_hms(FCR$DateTime))       

# Complete data interpolation for the heatmaps
# interative processes here

#Total Fe
interp_TFe <- interp(x=DateTime_x, y = TFe$Depth_m, z = TFe$TFe_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, 9, by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_TFe <- interp2xyz(interp_TFe, data.frame=T)
i_DateTime_TFe <- as.Date(interp_TFe$x)
interp_TFe_f <- interp_TFe %>% mutate(DateTime_i=i_DateTime_TFe)

#Total Mn
interp_TMn <- interp(x=DateTime_x, y = TMn$Depth_m, z = TMn$TMn_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, 9, by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_TMn <- interp2xyz(interp_TMn, data.frame=T)
i_DateTime_TMn <- as.Date(interp_TMn$x)
interp_TMn_f <- interp_TMn %>% mutate(DateTime_i=i_DateTime_TMn)

#Soluble Iron
interp_SFe <- interp(x=DateTime_x, y = SFe$Depth_m, z = SFe$SFe_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, 9, by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_SFe <- interp2xyz(interp_SFe, data.frame=T)
i_DateTime_SFe <- as.Date(interp_SFe$x)
interp_SFe_f <- interp_SFe %>% mutate(DateTime_i=i_DateTime_SFe)

#Soluble Mn
interp_SMn <- interp(x=DateTime_x, y = SMn$Depth_m, z = SMn$SMn_mgL,
                     xo = seq(min(DateTime_x), max(DateTime_x), by = 0.5), 
                     yo = seq(0.1, 9, by = 0.1),
                     extrap = F, linear = T, duplicate = "strip")
interp_SMn <- interp2xyz(interp_SMn, data.frame=T)
i_DateTime_SMn <- as.Date(interp_SMn$x)
interp_SMn_f <- interp_SMn %>% mutate(DateTime_i=i_DateTime_SMn)

# Plotting #

# This a theme I have adapted from 
#https://gist.github.com/jslefche/eff85ef06b4705e6efbc
# I LIKE IT!
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_line(size = 1, colour = NA),  
      axis.text.x = element_text(size = base_size*0.9, color = "black", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.9, color = "black", lineheight = 0.9),  
      axis.ticks = element_line(color = "black", size  =  0.4),  
      axis.title.x = element_text(size = base_size*1, color = "black", margin = margin(2, 0, 0, 0)),  
      axis.title.y = element_text(size = base_size*1, color = "black", angle = 90, margin = margin(0, 3, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "white"),  
      legend.key = element_rect(color = "black",  fill = "white"),  
      legend.key.size = unit(2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.9, color = "black"),  
      legend.title = element_text(size = base_size*1, face = "bold", hjust = 0, color = "black"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = NA, color  =  NA),  
      panel.border = element_rect(fill = NA, color = NA),  
      panel.grid.major = element_line(color = NA),  
      panel.grid.minor = element_line(color = NA),  
      panel.spacing = unit(0, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*1.2, color = "black"),  
      strip.text.y = element_text(size = base_size*1.2, color = "black",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "white", fill = "white"),  
      plot.title = element_text(size = base_size*1.6, color = "black",margin = margin(0,0,3,0)),  
      plot.margin = unit(rep(0, 4), "lines")
      
    )
  
}

#Vectors of dates for Hox#
ON = as.data.frame(ymd(c("2020-06-29", "2021-06-11")))
colnames(ON) = "Date"
OFF = as.data.frame(ymd(c("2020-12-02", "2021-12-06")))
colnames(OFF) = "Date"

# Specify plot limits
Begin_time = as.Date("2020-01-31")
End_time = as.Date("2021-12-31")

# For MUX Deployments

rectangle_TO <- data.frame(xmin = as.Date(c("2020-10-16")),
                           xmax = as.Date(c("2020-11-09")),
                           ymin = 0.05, ymax = 9.05)

rectangle_OO <- data.frame(xmin = as.Date(c("2021-05-26")),
                           xmax = as.Date(c("2021-06-21")),
                           ymin = 0.05, ymax = 9.05)


#Total Iron
p1 <- ggplot()+
  geom_raster(data=interp_TFe_f,aes(x=DateTime_i, y=y,fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=-0.1, z=NULL), pch = 25, size = 2, color = "black", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(10), values = c(0,0.02,0.05,0.1,0.15,0.25,0.4,0.6,0.8,1), na.value="gray")+
  labs(x=NULL, y = "Depth (m)",fill=expression('Tot. Fe (mg/L)'))+
  scale_x_date(date_breaks = "3 months", date_labels = ("%b %Y"), limits = c(Begin_time,End_time))+
  coord_cartesian(expand = FALSE) +
  geom_vline(data=ON, aes(xintercept=Date), linetype="solid", color="black", size=1)+
  geom_vline(data=OFF, aes(xintercept=Date), linetype="dashed", color="black", size=1)+
  theme_black()
  #geom_rect(data = rectangle_TO, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #          fill = "gray", alpha = 0.8) + 
  #geom_rect(data = rectangle_OO, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #          fill = "gray", alpha = 0.8)

#Total Mn
p2 <- ggplot()+
  geom_raster(data=interp_TMn_f, aes(x=DateTime_i,y=y,fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=-0.1, z=NULL), pch = 25, size = 2, color = "black", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(10), values = c(0,0.02,0.05,0.1,0.15,0.25,0.4,0.6,0.8,1), na.value="gray")+
  labs(x=NULL, y = "Depth (m)",fill=expression('Tot. Mn (mg/L)'))+
  scale_x_date(date_breaks = "3 months", date_labels = ("%b %Y"), limits = c(Begin_time,End_time))+
  geom_vline(data=ON, aes(xintercept=Date), linetype="solid", size=1)+
  geom_vline(data=OFF, aes(xintercept=Date), linetype="dashed", size=1)+
  coord_cartesian(expand = FALSE) +
  theme_black()
  #geom_rect(data = rectangle_TO, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #          fill = "gray", alpha = 0.7) + 
  #geom_rect(data = rectangle_OO, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #          fill = "gray", alpha = 0.7)



# Figure 2
jpeg('MUX_TFe_TMn_Heatmap_110922.jpeg', width = 190, height =160, units = 'mm', res = 600)

p1 / p2 + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(size = 14, hjust = 0, vjust = 0))

dev.off()



#### Code for plotting solubles heat maps

#Soluble Fe
png('FCR SFe Heatmap 2020.png', width = 15, height = 12, units = 'in', res = 300)
p3 <- ggplot(interp_SFe_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR Soluble Iron (mg/L) 2020",fill=expression('(mg/L)'))+
  theme_black()
print(p3)
dev.off()

#Soluble Mn
png('FCR SMn Heatmap 2020.png', width = 15, height = 12, units = 'in', res = 300)
p4 <- ggplot(interp_SMn_f, aes(x=DateTime_i, y=y))+
  geom_raster(aes(fill=z), interpolate = TRUE)+
  scale_y_reverse()+
  geom_point(data = DateTime_u, aes(x=DateTime, y=0, z=NULL), pch = 25, size = 1.5, color = "white", fill = "black")+
  scale_fill_gradientn(colours = blue2green2red(60), na.value="gray")+
  labs(x = "Date", y = "Depth (m)", title = "FCR Soluble Manganese (mg/L) 2020",fill=expression('(mg/L)'))+
  theme_black()
print(p4)
dev.off()

