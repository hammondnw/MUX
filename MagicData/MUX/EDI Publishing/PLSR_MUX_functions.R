
data_prep_20 = function(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam){
  
  #### Read in FCR WQ data ####
  
  #Download EDI metals dataset
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/455/6/57912981e3e857c85924484806492446" 
  infile1 <- c(paste0(pathD,WQ_name,sep=""))
  download.file(inUrl1,infile1,method="curl")  
  
  dataWQ <- read_csv(paste(pathD,WQ_name,sep=""))
  
  dataWQ = dataWQ %>% filter(Reservoir == "FCR" & Site == 50) %>% 
    select(DateTime,Depth_m,TFe_mgL,TMn_mgL,SFe_mgL,SMn_mgL)
  
  #Subset to just include desired depths
  dataWQ <- dataWQ %>%
    filter(Depth_m %in% as.numeric(Depths))
  
  #Subset to desired date range
  dataWQ$DateTime = ymd_hms(dataWQ$DateTime,tz="America/New_York")
  dataWQ = dataWQ[dataWQ$DateTime>Begin_time,]
  dataWQ = dataWQ[dataWQ$DateTime<End_time,]
  
  
  #### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
  # This step reads in the file of overlapping MUX/SCAN data and field data. 
  dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
  colnames(dataCalFP)<-c("ID","DateTime","status",seq(200,732.5,2.5),"Valve", "Measurement Time") #Add column names
  
  # Filter out the air valve measurements (valve #10, which was wrongly recorded as valve 12 in 2020)
  dataCalFP <- dataCalFP %>%
    filter(Valve!=12)
  
  # Subset to just include desired depths
  # First we need to convert valve # to depth
  Valves = as.data.frame(dataCalFP$Valve)
  colnames(Valves)=c("valve")
  valve_depth <- data.frame(
    valve = c (1:7), 
    Depth_m= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
    stringsAsFactors = FALSE
  )
  Valves = Valves %>% 
    left_join(valve_depth, by="valve") %>% 
    select(Depth_m)
  dataCalFP = cbind(dataCalFP,Valves)
  # Now we can subset based on depth
  dataCalFP <- dataCalFP %>%
    filter(as.numeric(Depth_m) %in% as.numeric(Depths))
  #remove 'Depth' column
  dataCalFP <- dataCalFP %>% 
    select(!c(Depth_m))
  
  #Subset to desired date range
  dataCalFP = dataCalFP[dataCalFP$DateTime>Begin_time,]
  dataCalFP = dataCalFP[dataCalFP$DateTime<End_time,]
  
  #Create vector of times and ID from the FP file
  timesCalFP<-dataCalFP %>% select(ID,DateTime)
  
  dataCalFP <- dataCalFP %>% select(!c(ID,DateTime,status)) #Remove ID, DateTime, and status columns
  dataCalFP<- dataCalFP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
  dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix
  
  #This replaces the ID and Date from the original dataWQ with the exact values
  #from the SCAN so that manual values can be plotted later on in the TS plots
  
  dataWQ$ID<-timesCalFP[,1]
  dataWQ$DateTime<- ymd_hms(timesCalFP[,2], tz="America/New_York")
  
  
  #### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) ####
  # This is the 2020 SCAN data 
  TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",",header = T)  #Import Time Series data as .csv file
  colnames(TS_FP)<-c("DateTime","Valve","Depth_m","wavelength","absorbance") #Add column names
  
  # Filter out the air valve measurements (valve #7)
  TS_FP <- TS_FP %>%
    filter(Valve!=12)
  
  # Subset to just include desired depths
  
  # subset based on depth
  TS_FP <- TS_FP %>%
    filter(as.numeric(Depth_m) %in% as.numeric(Depths))
  #remove 'Depth' column
  TS_FP <- TS_FP %>% 
    select(!c(Depth_m))
  
  #Subset to desired date range
  TS_FP = TS_FP[TS_FP$DateTime>Begin_time,]
  TS_FP = TS_FP[TS_FP$DateTime<End_time,]
  
  # pivot wider
  TS_FP = TS_FP %>% pivot_wider(names_from = wavelength, values_from = absorbance)
  
  TS_FP$DateTime = as.POSIXct(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S")
  Dat<-strptime(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time
  
  #### Create a dataframe to store outputs (TS_conc) ####
  # Convert Valve number to Depth for the TS_conc matrix
  Valves = as.data.frame(TS_FP$Valve)
  colnames(Valves)=c("valve")
  valve_depth <- data.frame(
    valve = c (1:7), 
    Depth_m= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
    stringsAsFactors = FALSE
  )
  Valves= Valves %>% 
    left_join(valve_depth, by="valve")
  
  Depth_lab = Valves %>% 
    select(Depth_m)
  
  #Create matrix to store calculated concentrationss:TS_conc 
  TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],6))  #Create data frame for date/time and predicted values
  TS_conc[,1]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
  TS_conc[,2]<- Depth_lab
  colnames(TS_conc)<-c("DateTime", "Depth_m",WQparam) #Add column names
  
  # Finish cleaning up time series dataframe
  TS_FP<- TS_FP %>% select(!c(DateTime,Valve)) #remove 'Date' and 'status' columns
  TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix
  
  assign("dataCalFP",dataCalFP,env=.GlobalEnv)
  assign("dataWQ",dataWQ,env=.GlobalEnv)
  assign("TS_FP",TS_FP,env=.GlobalEnv)
  assign("TS_conc",TS_conc,env=.GlobalEnv)
  
}




data_prep_21 = function(WQ_name,FPcaldata_name,TimeSeriesFP_name,Depths,Begin_time,End_time,WQparam){
  
  #### Read in FCR WQ data ####
  
  #Download EDI metals dataset
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/455/6/57912981e3e857c85924484806492446" 
  infile1 <- c(paste0(pathD,WQ_name,sep=""))
  download.file(inUrl1,infile1,method="curl")  
  
  dataWQ <- read_csv(paste(pathD,WQ_name,sep=""))
  
  dataWQ$DateTime = ymd_hms(dataWQ$DateTime,tz="America/New_York")
  
  #Subset to just include desired depths, Reservoir, and Site
  # Remove 'Flag' columns
  dataWQ <- dataWQ %>%
    filter(Depth_m %in% as.numeric(Depths)) %>%
    filter(Reservoir == "FCR") %>%
    filter(Site == 50) %>%
    select(-c("Reservoir","Site","Flag_DateTime","Flag_TFe","Flag_TMn","Flag_SFe","Flag_SMn"))
  
  #Subset to desired date range
  dataWQ = dataWQ[dataWQ$DateTime>Begin_time,]
  dataWQ = dataWQ[dataWQ$DateTime<End_time,]
  
  # Remove samples from 06-02-2021 because the MUX was not deployed during that time
  dataWQ = dataWQ %>% filter(date(DateTime) != "2021-06-02")
  
  #### Reading of  FingerPrint (FP) file corresponding to lab concentrations for calibration ####
  # This step reads in the file of overlapping MUX/SCAN data and field data. 
  dataCalFP<-read.delim(file=paste(pathD,FPcaldata_name,sep=""),sep=",")  #Import data as .csv file
  colnames(dataCalFP)<-c("ID","DateTime","status",seq(200,732.5,2.5),"Valve", "Measurement Time") #Add column names
  
  
  # Filter out the air valve measurements (valve #7)
  dataCalFP <- dataCalFP %>%
    filter(Valve!=9 | Valve!=10)
  
  # Subset to just include desired depths
  # First we need to convert valve # to depth
  Valves = as.data.frame(dataCalFP$Valve)
  colnames(Valves)=c("valve")
  valve_depth <- data.frame(
    valve = c (1:7), 
    Depth= c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
    stringsAsFactors = FALSE
  )
  Valves = Valves %>% 
    left_join(valve_depth, by="valve") %>% 
    select(Depth)
  dataCalFP = cbind(dataCalFP,Valves)
  # Now we can subset based on depth
  dataCalFP <- dataCalFP %>%
    filter(as.numeric(Depth) %in% as.numeric(Depths))
  #remove 'Depth' column
  dataCalFP <- dataCalFP %>% 
    select(!c(Depth))
  
  #Subset to desired date range
  dataCalFP = dataCalFP[dataCalFP$DateTime>Begin_time,]
  dataCalFP = dataCalFP[dataCalFP$DateTime<End_time,]
  
  #Create vector of times and ID from the FP file
  timesCalFP<-dataCalFP %>% select(ID,`DateTime`)
  
  dataCalFP <- dataCalFP %>% select(!c(ID,`DateTime`,status)) #Remove ID, DateTime, and status columns
  dataCalFP<- dataCalFP %>% select(!c(Valve,`Measurement Time`)) #remove 'valve' and 'measurement time' columns at the end
  dataCalFP<-data.matrix(dataCalFP) #Convert to data matrix
  
  #This replaces the ID and Date from the original dataWQ with the exact values
  #from the SCAN so that manual values can be plotted later on in the TS plots
  
  dataWQ$ID<-timesCalFP[,1]
  dataWQ$DateTime<- ymd_hms(timesCalFP[,2], tz="America/New_York")
  
  
  #### Reading of  FingerPrint (FP) file corresponding to the entire time series (TS) ####
  # This is the 2021 MUX data 
  TS_FP<-read.table(file=paste(pathD,TimeSeriesFP_name,sep=""),sep=",",header = T)  #Import Time Series data as .csv file
  colnames(TS_FP)<-c("DateTime","Valve","Depth_m","wavelength","absorbance") #Add column names
  
  # Filter out the air valve measurements (valve #7)
  TS_FP <- TS_FP %>%
    filter(Valve!=9 | Valve!=10)
  
  # Now we can subset based on depth
  TS_FP <- TS_FP %>%
    filter(as.numeric(Depth_m) %in% as.numeric(Depths))
  #remove 'Depth' column
  TS_FP <- TS_FP %>% 
    select(!c(Depth_m))
  
  #Subset to desired date range
  TS_FP = TS_FP[TS_FP$DateTime>Begin_time,]
  TS_FP = TS_FP[TS_FP$DateTime<End_time,]
  
  # pivot wider
  TS_FP = TS_FP %>% pivot_wider(names_from = wavelength, values_from = absorbance)
  
  TS_FP$DateTime = as.POSIXct(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S")
  Dat<-strptime(TS_FP$DateTime, format = "%Y-%m-%d %H:%M:%S") #Create record of date and time
  
  #### Create a dataframe to store outputs (TS_conc) ####
  # Convert Valve number to Depth for the TS_conc matrix
  Valves = as.data.frame(TS_FP$Valve)
  colnames(Valves)=c("valve")
  valve_depth <- data.frame(
    valve = c (1:7), 
    Depth_m = c("0.1","1.6","3.8","5.0","6.2", "8.0", "9.0"),
    stringsAsFactors = FALSE
  )
  Valves= Valves %>% 
    left_join(valve_depth, by="valve")
  
  Depth_lab = Valves %>% 
    select(Depth_m)
  
  #Create matrix to store calculated concentrationss:TS_conc 
  TS_conc<-as.data.frame(matrix(0,dim(TS_FP)[1],6))  #Create data frame for DateTime and predicted values
  TS_conc[,1]<- Depth_lab
  TS_conc[,2]<-as.character(Dat, "%Y-%m-%d %H:%M:%S")
  colnames(TS_conc)<-c("Depth_m", "DateTime",WQparam) #Add column names
  
  # Finish cleaning up time series dataframe
  TS_FP<- TS_FP %>% select(!c(DateTime,Valve)) #remove 'Date' and 'status' columns
  TS_FP<-data.matrix(TS_FP) #Convert spectrometer output to matrix
  
  assign("dataCalFP",dataCalFP,env=.GlobalEnv)
  assign("dataWQ",dataWQ,env=.GlobalEnv)
  assign("TS_FP",TS_FP,env=.GlobalEnv)
  assign("TS_conc",TS_conc,env=.GlobalEnv)
  
}



PLSR_enpls<-function(param,dataCalFP,dataWQ,TS_FP,maxcomp, reptimes){
  
  # These five lines of code are technically obsolete with the prior data prep (but leaving them in for now) #
  WQ<-data.matrix(subset(dataWQ,select=param)) #Make matrix of the param values
  temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
  temp<-temp[complete.cases(temp),] #removes the rows containing NAs
  WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
  dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
  
  
  fit<-enpls.fit(data.matrix(dataFP), WQ, maxcomp=maxcomp, reptimes = reptimes)  #PLSR model to predict param with cross validation
  cv.fit <- cv.enpls(data.matrix(dataFP), WQ, maxcomp = maxcomp,
                     reptimes = reptimes, verbose = TRUE)
  print(cv.fit)
  Pfit<-predict(fit,newx=dataFP) #Predict concentrations based on PLSR model
  df <- data.frame(WQ, Pfit)
  ggplot(df, aes_string(x = "TFe_mgL", y = "Pfit")) +
    geom_abline(slope = 1, intercept = 0, colour = "darkgrey") +
    geom_point(size = 3, shape = 1, alpha = 0.8) +
    coord_fixed(ratio = 1) +
    xlab("Observed Response") +
    ylab("Predicted Response")
  
  fs <- enpls.fs(data.matrix(dataFP), WQ, maxcomp = maxcomp, reptimes = reptimes)
  print(fs, nvar = 10)
  
  od <- enpls.od(data.matrix(dataFP), as.vector(WQ), maxcomp = maxcomp, reptimes = reptimes)
  plot(od, prob = 0.05)
  plot(od, sdtimes = 3)
  
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  
}




PLSR_SCAN<-function(param,dataCalFP,dataWQ,TS_FP,ncomp,yesplot=FALSE){
  
  # These five lines of code are technically obsolete with the prior data prep (but leaving them in for now) #
  WQ<-data.matrix(subset(dataWQ,select=param)) #Make matrix of the param values
  temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
  temp<-temp[complete.cases(temp),] #removes the rows containing NAs
  WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
  dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
  
  
  fit<-plsr(WQ~data.matrix(dataFP),ncomp=ncomp,validation="CV")  #PLSR model to predict param with cross validation
  summary(fit)  #See summary of PLSR model to choose number of components
  Pfit<-predict(fit,dataFP,ncomp=ncomp,type=c("response")) #Predict concentrations based on PLSR model
  #x11()
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  Pfit_TS<-predict(fit,TS_FP,ncomp=ncomp,type=c("response"))
  WQP_TS<-as.data.frame(matrix(0,1,dim(TS_FP)[1])) #Create data frame for predicted Time series values
  WQP_TS<-as.data.frame(Pfit_TS[1:length(Pfit_TS)])  #Insert predicted param values into data frame
  
  if (yesplot==TRUE){
    plot(WQ,as.matrix(WQP), asp = 1,
         xlab=paste("measured",param,"?g/L",sep=" "),
         ylab=c("PLSR_predicted")) #Compare predicted and lab values of param
    
    fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
    abline(fit2)
    summary(fit2)
    tp_resid <- resid(fit2)
  }
  
  assign("WQP_TS",WQP_TS,env=.GlobalEnv)
  assign("WQ",WQ,env=.GlobalEnv)
  assign("WQP",WQP,env=.GlobalEnv)
  assign("fit",fit,env=.GlobalEnv)
  assign("fit2",fit2,env=.GlobalEnv)
}




PLSR_SCAN_boot<-function(param,dataCalFP,dataWQ,TS_FP,ncomp,yesplot=FALSE){
  
  # These five lines of code are technically obsolete with the prior data prep (but leaving them in for now) #
  WQ<-data.matrix(subset(dataWQ,select=param)) #Make matrix of the param values
  temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
  temp<-temp[complete.cases(temp),] #removes the rows containing NAs
  WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
  dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
  
  
  fit<-plsr(WQ~data.matrix(dataFP),ncomp=ncomp,validation="CV")  #PLSR model to predict param with cross validation
  summary(fit)  #See summary of PLSR model to choose number of components
  Pfit<-predict(fit,dataFP,ncomp=ncomp,type=c("response")) #Predict concentrations based on PLSR model
  #x11()
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  Pfit_TS<-predict(fit,TS_FP,ncomp=ncomp,type=c("response"))
  WQP_TS<-as.data.frame(matrix(0,1,dim(TS_FP)[1])) #Create data frame for predicted Time series values
  WQP_TS<-as.data.frame(Pfit_TS[1:length(Pfit_TS)])  #Insert predicted param values into data frame
  
  #### Bootstrapping ####
  
  B = 1000 # number of bootstrapped samples
  G = matrix(NA,nrow(WQP_TS),B) # set up matrix to store prediction error values
  
  for(i in 1:B){
    # Sample the residuals of the original fitted model to construct a set of new
    # Y values (WQP_star)
    WQP_star = WQP + sample(resid(fit)[,,ncomp],nrow(WQP),replace = TRUE)
    WQP_star = data.matrix(WQP_star[1])
    
    # Sample the residuals of the original fitted model to construct a set of new 
    # Predicted values
    WQP_TS_star = WQP_TS + sample(resid(fit)[,,ncomp], nrow(WQP_TS), replace = TRUE)
    WQP_TS_star = data.matrix(WQP_TS_star[1])
    
    # Fit a new PLS regression using the new Y-values (WQP_star)
    fit_star = plsr(WQP_star~data.matrix(dataFP),ncomp=ncomp,validation="CV")
    
    # Predict new values based on new fit
    Pfit_TS_star<-predict(fit_star,TS_FP,ncomp=ncomp,type=c("response"))
    WQP_TS_hat_star<-matrix(Pfit_TS_star[1:length(Pfit_TS_star)])  #Insert predicted param values into data frame
    
    # Calculate prediction error for bootstrap sample
    pred_error = WQP_TS_star - WQP_TS_hat_star
    
    G[,i] = pred_error
  }
  
  # Sample from G to estimate the 90% predictive interval
  pred_int = apply(G, 1, quantile, c(0.05,0.95))
  
  if (yesplot==TRUE){
    plot(WQ,as.matrix(WQP), asp = 1,
         xlab=paste("measured",param,"?g/L",sep=" "),
         ylab=c("PLSR_predicted")) #Compare predicted and lab values of param
    
    fit2<-lm(WQ~as.matrix(WQP)) #Linear regression of predicted and lab NO3-N values
    abline(fit2)
    summary(fit2)
    tp_resid <- resid(fit2)
  }
  
  assign("WQP_TS",WQP_TS,env=.GlobalEnv)
  assign("WQ",WQ,env=.GlobalEnv)
  assign("WQP",WQP,env=.GlobalEnv)
  assign("fit",fit,env=.GlobalEnv)
  assign("pred_int",pred_int,env=.GlobalEnv)
}

