#### function which does the calibration and then calculates conc from the TS ####
# for a given chemical parameter (param).  It does the calibration for a given 
#number of components (ncomp)

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
