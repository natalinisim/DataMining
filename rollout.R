# Initialization 
rm(list = ls()) # remove all variables from global environment
cat("\014") # clear the screen
file_path = file.path(getwd(),"store_data.csv")
reviews_path = file.path(getwd(), "reviews_data.csv")
training = read.csv(file_path, na.strings="") # read data
reviews = read.csv(reviews_path, na.strings="")
set.seed(2017) # define the pseudorandomness
library("randomForest") #loading required package: randomForest

training$BUYER_FLAG[training$BUYER_FLAG==1]="YES"
training$BUYER_FLAG[training$BUYER_FLAG==0]="NO"
training$BUYER_FLAG <- as.factor(training$BUYER_FLAG)
res = training[training$ID %in% reviews$ID, c("ID", "BUYER_FLAG")]
reviews = merge(reviews, res, by="ID")
variables2include = (colnames(training) != "ID")

mdl = glm(BUYER_FLAG ~ .,family="binomial" , data=training[,variables2include])

rev_mdl = randomForest(BUYER_FLAG ~ .-ID,data=reviews,ntree=100)

roll_file_path = file.path(getwd(),"store_rollout.csv")
roll_reviews_path = file.path(getwd(), "reviews_rollout.csv")
roll_store = read.csv(roll_file_path, na.strings="") # read data
roll_reviews = read.csv(roll_reviews_path, na.strings="")

y_hat = predict(mdl, roll_store, type="response")
store_hat = y_hat > 0.16

rev_y_hat = predict(rev_mdl, roll_reviews, type="prob")
rev_hat = rev_y_hat[,2] > 0.212

store_hat[roll_store$ID %in% roll_reviews$ID] =  store_hat[roll_store$ID %in% roll_reviews$ID] | rev_hat
rollout_ID = 30001:40000
rollout_BF = as.numeric(store_hat)
write.csv(	cbind(ID=rollout_ID, BUYER_FLAG=rollout_BF),
           "recommendations2.csv",
           row.names = FALSE)		
