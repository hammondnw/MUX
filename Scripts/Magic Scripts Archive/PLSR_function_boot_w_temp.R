#### function which does the calibration and then calculates conc from the TS ####
# for a given chemical parameter (param).  It does the calibration for a given 
#number of components (ncomp)

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
  pred_int = apply(G, 1, quantile, c(0.05,0.95),na.rm=T)
  
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
