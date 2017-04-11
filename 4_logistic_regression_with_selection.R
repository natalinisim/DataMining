# Initialization 
rm(list = ls()) # remove all variables from global environment
cat("\014") # clear the screen
file_path = file.path(getwd(),"store_data.csv")
training = read.csv(file_path, na.strings="") # read data
set.seed(2017) # define the pseudorandomness
library("randomForest") #loading required package: randomForest
library("ROCR") #loading required package: ROCR

# Data preparation
training$BUYER_FLAG[training$BUYER_FLAG==1]="YES"
training$BUYER_FLAG[training$BUYER_FLAG==0]="NO"
training$BUYER_FLAG <- as.factor(training$BUYER_FLAG)
mainresults=data.frame()
# Feature selection - omit a different feature every iteration.
for(a in 1:(ncol(training)-1)){
  variables2include = (colnames(training) != "ID" & colnames(training)!=colnames(training)[a])
  K = 5 # set K
  g = sample(K, nrow(training), replace=T)
  
  # Variables for the next phases
  results = data.frame()
  
  MaxExpectedValue = 0
  RecallMaxExpected = 0
  PercisionMaxExpected = 0
  CutOffMaxExpected = 0
  CutOffZIndex = 0
  # Using cross validation method
  for(k in 1:K){
    # Split to test and train sets
    test_set_indicator = (g==k)
    train_set = training[!test_set_indicator,]
    test_set = training[test_set_indicator,]
    
    MaxExpectedValue = 0
    
    # Define model
    mdl = glm(BUYER_FLAG ~ .,family="binomial" , data=train_set[,variables2include])
    
    # Prediction
    y_hat = predict(mdl, test_set, type="response")
    pred=prediction(y_hat, test_set$BUYER_FLAG)
    perf=performance(pred,"prec","rec") 
    
    x=perf@x.values[[1]] # recall
    y=perf@y.values[[1]] # precision
    z=perf@alpha.values[[1]] #cutoff
    
    # Find the max expected value.
    for(t in 1:length(z))
    {
      y_hat_temp = (y_hat > z[t])
      true_state = test_set$BUYER_FLAG == "YES"
      tp = sum(((y_hat_temp==TRUE) & (true_state==TRUE)))   # true positive
      fp = sum(((y_hat_temp==TRUE) & (true_state==FALSE)))  # false positive
      fn = sum(((y_hat_temp==FALSE) & (true_state==TRUE)))  # false negative
      tn = sum(((y_hat_temp==FALSE) & (true_state==FALSE))) # true negative  
      sumP = tp + fn # all positive
      sumN = tn + fp # all negative
      sumAll = sumP + sumN # all observations
      pRate = sumP/sumAll # positive rate
      nRate = sumN/sumAll # negatice rate
      tpRate = tp/sumP # true positive rate
      fpRate = fp/sumN # true negative rate
      
      # Calculate the expected value
      ExpectedValue = pRate*(tpRate*35.4)+nRate*(fpRate*-6.5)
      
      # If there weren't better values until now. 
      if(ExpectedValue > MaxExpectedValue)
      {
        MaxExpectedValue = ExpectedValue
        CutOffMaxExpected = z[t]
        RecallMaxExpected = x[t]
        PercisionMaxExpected = y[t]
        CutOffZIndex = t
      }
    }
    # Set max expected value and cutoff
    results[k,"MaxExpectedValue"] = MaxExpectedValue
    results[k,"Cutoff"]= CutOffMaxExpected
    results[k,"Precision"] = PercisionMaxExpected
    results[k,"Recall"] = RecallMaxExpected
  }
  results
  colMeans(results,na.rm=FALSE,dims=1)
  results["Cutoff"]
  # The MaxExpectedValue for every feature omited.
  mainresults[a,"MaxExpectedValue"]=colMeans(subset(results, select=c(MaxExpectedValue)),na.rm=TRUE)
  mainresults[a,"Cutoff"]=colMeans(subset(results, select=c(Cutoff)),na.rm=TRUE)
  mainresults[a,"Precision"]=colMeans(subset(results, select=c(Precision)),na.rm=TRUE)
  mainresults[a,"Recall"]=colMeans(subset(results, select=c(Recall)),na.rm=TRUE)
}
mainresults
