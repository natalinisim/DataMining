# Initialization 
rm(list = ls()) # remove all variables from global environment
cat("\014") # clear the screen
file_path = file.path(getwd(),"store_data.csv")
training = read.csv(file_path, na.strings="") # read data
set.seed(2017) # define the pseudorandomness
library("class") #loading required package: class 
library(ROCR)

# Data preparation
training$BUYER_FLAG[training$BUYER_FLAG==1]="YES"
training$BUYER_FLAG[training$BUYER_FLAG==0]="NO"
training$BUYER_FLAG <- as.factor(training$BUYER_FLAG)
levels(training$GENDER) <- c(levels(training$GENDER), "1")
levels(training$GENDER) <- c(levels(training$GENDER), "0")
training$GENDER[training$GENDER=="F"]="1"
training$GENDER[training$GENDER=="M"]="0"
training$GENDER <- factor(training$GENDER)
training$GENDER=as.numeric(levels(training$GENDER))[training$GENDER]

library("class") #loading required package: class 

K = 5 # set K
g = sample(K, nrow(training), replace=T)


variables2include = (colnames(training) != "ID" & colnames(training) != "BUYER_FLAG")

# Normalizing the data
for (col in colnames(training)){
  if(col != 'ID' & col != "BUYER_FLAG"){
    maxValue = max(training[,col])
    minValue = min(training[,col])
    diff = maxValue-minValue
    training[,col] = (training[,col]-minValue)/diff
  }
}

head(training,15)
head(training$Money,150)

results = data.frame()
resultsCutoff = data.frame()
resultsPrecision = data.frame()
resultsRecall = data.frame()

# Variables for the next phases
RecallMaxExpected = 0
PercisionMaxExpected = 0
CutOffMaxExpected = 0
CutOffZIndex = 0

# Using cross validation method
for(fold in 1:5){
  # Split to test and train sets
  test_set_indicator = (g==fold)
  train_set = training[!test_set_indicator,]
  test_set = training[test_set_indicator,]

  #searching for the best k.
  for(kn in 1:50)
  {
    
    # Make the knn model.
    mdl = knn(train = train_set[,variables2include],test = test_set[,variables2include],cl=train_set$BUYER_FLAG,k=kn, prob=TRUE)
    pd=attr(mdl,"prob")
    pred = numeric(length(mdl))
    pred[mdl == "YES"] = pd[mdl=="YES"]
    pred[mdl == "NO"] = 1-pd[mdl=="NO"]
    prob = pred
    pred=prediction(pred, test_set[,21])
    perf=performance(pred,"prec","rec") 
    
    x=perf@x.values[[1]] # recall
    y=perf@y.values[[1]] # precision
    z=perf@alpha.values[[1]] #cutoff
    MaxExpectedValue = 0
    for(t in 1:length(z))
    {
      y_hat_temp = (prob > z[t])
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
      if(is.nan(tpRate)){
        tpRate = 0
      }
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
    
    # The best values for this iteration
    results[fold,kn] = MaxExpectedValue
    resultsCutoff[fold,kn] = CutOffMaxExpected
    resultsRecall[fold, kn] = RecallMaxExpected
    resultsPrecision[fold, kn] = PercisionMaxExpected
  }
  
}

#plot(colMeans(resultsaccuracy,na.rm = FALSE,dims=1L))
plot(colMeans(results,na.rm = FALSE,dims=1L))

results[which.max(colMeans(results))]
resultsPrecision[which.max(colMeans(results))]
resultsRecall[which.max(colMeans(results))]