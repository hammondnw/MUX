###########################
# Plotting met data to compare to 1.6m scan 
# Rachel Corrigan
# Sept 2020
##########################

metvars <- read_csv("/Users/rachelcorrigan/Dropbox/SCCdata/carina-data/FCRmet.csv", skip=1)
metvars$TIMESTAMP <- as.POSIXct(metvars$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
metvars <- metvars[-c(1:2),]
overlap_metvars <- metvars %>%
  filter(TIMESTAMP >= "2020-04-24")
plot(overlap_metvars$TIMESTAMP, overlap_metvars$Rain_mm_Tot, type="l")


rainplot <- ggplot() +
  geom_point(data=overlap_metvars, aes(x=TIMESTAMP,y=Rain_mm_Tot), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("Rain mm")), title = "Rain") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
rainplot

windplot <- ggplot() +
  geom_point(data=overlap_metvars, aes(x=TIMESTAMP,y=WS_ms_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("Wind m/s")), title = "Wind") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
windplot
overlap_metvars$WS_ms_Avg <- as.numeric(overlap_metvars$WS_ms_Avg)
overlap_metvars$Rain_mm_Tot <- as.numeric(overlap_metvars$Rain_mm_Tot)
overlap_metvars$SR01Up_Avg <- as.numeric(overlap_metvars$SR01Up_Avg)
overlap_metvars$PAR_Den_Avg <- as.numeric(overlap_metvars$PAR_Den_Avg)
plot(overlap_metvars$TIMESTAMP, overlap_metvars$WS_ms_Avg, type="l")
plot(overlap_metvars$TIMESTAMP, overlap_metvars$PAR_Den_Avg, type="l")

class(overlap_metvars$WS_ms_Avg)

require(dplyr)
fifteen <- overlap_metvars %>%
  group_by(TIMESTAMP = cut(TIMESTAMP, breaks="15 min")) %>%
  summarize(Rain_mm_Tot = mean(Rain_mm_Tot), WS_ms_Avg = mean(WS_ms_Avg), 
            SR01Up_Avg = mean(SR01Up_Avg), PAR_Den_Avg = mean(PAR_Den_Avg)) 
fifteen$TIMESTAMP <- as.POSIXct(fifteen$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

windplot <- ggplot() +
  geom_line(data=fifteen, aes(x=TIMESTAMP,y=WS_ms_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("Wind m/s")), title = "Wind") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
windplot

rainplot <- ggplot() +
  geom_line(data=fifteen, aes(x=TIMESTAMP,y=Rain_mm_Tot), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("Rain mm")), title = "Rain") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
rainplot

PARplot <- ggplot() +
  geom_line(data=fifteen, aes(x=TIMESTAMP,y=PAR_Den_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("PAR umol/s/m2")), title = "PAR average") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
PARplot

swplot <- ggplot() +
  geom_line(data=fifteen, aes(x=TIMESTAMP,y=SR01Up_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("SW W/m2")), title = "Shortwave Up") +
  scale_x_datetime(labels = date_format("%Y-%m-%d"))+
  theme(legend.position="none")
swplot

vars_plot <- ggarrange(rainplot, windplot,PARplot,swplot, ncol=2, nrow=2)
vars_plot

oneweek_metvars <- fifteen %>%
  filter(TIMESTAMP >= "2020-05-01" & TIMESTAMP <= "2020-05-07")

windwk <- ggplot() +
  geom_line(data=oneweek_metvars, aes(x=TIMESTAMP,y=WS_ms_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("Wind m/s")), title = "Wind") +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))+
  theme(legend.position="none")


rainwk <- ggplot() +
  geom_line(data=oneweek_metvars, aes(x=TIMESTAMP,y=Rain_mm_Tot), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("Rain mm")), title = "Rain") +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))+
  theme(legend.position="none")


PARwk <- ggplot() +
  geom_line(data=oneweek_metvars, aes(x=TIMESTAMP,y=PAR_Den_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("PAR umol/s/m2")), title = "PAR average") +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))+
  theme(legend.position="none")


swwk <- ggplot() +
  geom_line(data=oneweek_metvars, aes(x=TIMESTAMP,y=SR01Up_Avg), size=0.5) +
  #geom_ribbon(data=TS_conc, aes(ymin=uncerDN_min, ymax=uncerDN_max, x=DateTime, fill = "band"), alpha = 0.2)+
  #geom_point(data=dataWQ, aes(x=DateTime, y=DN_mgL), colour="blue") +
  #ylim(-5, 50)+
  labs(x="Date", y = expression(paste("SW W/m2")), title = "Shortwave Up (facing sky)") +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))+
  theme(legend.position="none")


varswk_plot <- ggarrange(rainwk, windwk, PARwk, swwk, ncol=2, nrow=2)
varswk_plot