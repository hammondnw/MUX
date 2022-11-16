##################
# Rachel Corrigan
# June 2020
#
# Using the PLSR model to identify the correct number of components for each nutrient based on the RMSE
# Using code addapted from CCC original PLSR script
##################


ncomps = c(1:10)
param="DN_mgL" #the nutrient you want to find the number of comps for 
WQ<-data.matrix(subset(dataWQ,select=param)) #matrix of the param values
temp<-cbind(dataCalFP,WQ) #combines FP and WQ columns to remove the rows containing NAs 
temp<-temp[complete.cases(temp),] #removes the rows containing NAs
WQ<-data.matrix(subset(temp,select=param)) # recreate a data matrix from the WQ vector minus the NAs
dataFP<-temp[,-dim(temp)[2]]  # redefines the FP matrix rid off the NA values of missing WQ
RMSE <- numeric(length(ncomps))


############
# Loop 1:10 components through the PLSR model and plot the RMSE of thepredictions
############

for (i in 1:length(ncomps)) {  
  fit<-plsr(WQ~data.matrix(dataFP),ncomps[i],validation="CV")  #PLSR model to predict param with cross validation
  summary(fit)  #See summary of PLSR model to choose number of components
  Pfit<-predict(fit,dataFP,ncomps[i],type=c("response")) #Predict NO3-N concentrations based on PLSR model
  #x11()
  WQP<-as.data.frame(matrix(0,1,dim(dataFP)[1])) #Create data frame for predicted param values
  WQP<-as.data.frame(Pfit[1:length(Pfit)])  #Insert predicted param values into data frame
  
  Pfit_TS<-predict(fit,TS_FP, ncomps[i],type=c("response"))
  WQP_TS<-as.data.frame(matrix(0,1,dim(TS_FP)[1])) #Create data frame for predicted Time series values
  WQP_TS<-as.data.frame(Pfit_TS[1:length(Pfit_TS)])  #Insert predicted param values into data frame
  RMSE[i]<-sqrt(mean((WQ-WQP$`Pfit[1:length(Pfit)]` )^2)) #write out rmse values for each # of components

}
plot(RMSE) #plot RMSE curve

#############
#Choose the number that is at the bottom of the curve, plus 1.  
############
