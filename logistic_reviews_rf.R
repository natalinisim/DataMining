# Initialization 
rm(list = ls()) # remove all variables from global environment
cat("\014") # clear the screen
file_path = file.path(getwd(),"store_data.csv")
reviews_path = file.path(getwd(), "reviews_data.csv")
training = read.csv(file_path, na.strings="") # read data
reviews = read.csv(reviews_path, na.strings="")
set.seed(2017) # define the pseudorandomness
library("randomForest") #loading required package: randomForest
library("ROCR") #loading required package: ROCR
K = 5 # set K

# Data preparation
training$BUYER_FLAG[training$BUYER_FLAG==1]="YES"
training$BUYER_FLAG[training$BUYER_FLAG==0]="NO"
training$BUYER_FLAG <- as.factor(training$BUYER_FLAG)
res = training[training$ID %in% reviews$ID, c("ID", "BUYER_FLAG")]
reviews = merge(reviews, res, by="ID")
#library(FSelector)
#weights <- information.gain(BUYER_FLAG~.-ID, reviews)
#subset <- cutoff.k(weights, 200)
#if(! ("ID" %in% subset)){
#  subset = c(subset, "ID")
#}
#subset = c(subset,  "BUYER_FLAG")
#reviews = reviews[,subset]
g = sample(K, nrow(training), replace=T)
variables2include = (colnames(training) != "ID")

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
  
  rev_train = reviews[reviews$ID %in% train_set$ID,]
  rev_test = reviews[reviews$ID %in% test_set$ID,]
  
  # Define model
  mdl = glm(BUYER_FLAG ~ .,family="binomial" , data=train_set[,variables2include])
  
  rev_mdl = randomForest(BUYER_FLAG ~ .-ID,data=rev_train,ntree=100)
  MaxExpectedValue=0
  
  # Prediction
  y_hat = predict(mdl, test_set, type="response")
  log_pred_result = y_hat > 0.16
  rev_y_hat = predict(rev_mdl, rev_test, type="prob")
  
  pred=prediction(rev_y_hat[,2], rev_test$BUYER_FLAG)
  perf=performance(pred,"prec","rec") 
  
  x=perf@x.values[[1]] # recall
  y=perf@y.values[[1]] # precision
  z=perf@alpha.values[[1]] #cutoff
  
  # Find the max expected value.
  for(t in 1:length(z))
  {
    y_hat_rev = (rev_y_hat[,"YES"] > z[t])
    y_hat_temp = log_pred_result
    y_hat_temp[test_set$ID %in% rev_test$ID] =  y_hat_temp[test_set$ID %in% rev_test$ID] | y_hat_rev
    
    
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
  
}

results
colMeans(results,na.rm=FALSE,dims=1)




