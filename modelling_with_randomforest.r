# split 70 percent of the data into the training dataset and 30 percent of the data
# into the testing dataset:
set.seed(123)
ind = sample(2, nrow(hr.emp.data.df), replace = TRUE, prob=c(0.7,0.3))
train.df = hr.emp.data.df[ind == 1,]
test.df = hr.emp.data.df[ind == 2,]
dim(train.df)
dim(test.df)

prop.table(table(train.df$Attrition)) 

prop.table(table(test.df$Attrition)) 
# we see almost equal representation in both training and testing set for the dependent or response variable

## few false positives  and high accuracy in the model

library(randomForest)
library(ROCR)

#make the formula
varNames <- names(train.df)

#exclude the response variable and unimportant variables
varNames<- varNames[!varNames %in% c("Attrition","EmployeeCount","EmployeeNumber","Over18","StandardHours")]

#add response variable and convert to a formula object
rf.form <- as.formula(paste("Attrition",paste(varNames,collapse=' + '),sep=' ~ '))

# all the variables considered and 500 trees
rf <- randomForest(rf.form, data = train.df, ntree=500, importance =T)
rf #28% false positive cases
plot(rf) 

AttritionTrainPrediction = predict(rf, train.df[,-2])
Attritionprediction = predict(rf, test.df[,-2])

table(AttritionTrainPrediction, train.df$Attrition)
table(Attritionprediction, test.df$Attrition)

library(caret)
##confusion matrix
confusionMatrix(AttritionTrainPrediction,train.df$Attrition,positive = "Yes")
confusionMatrix(Attritionprediction,test.df$Attrition,positive = "Yes")
# Accuracy : 0.9513 ; Sensitivity : 0.7233  (True Positive Rate)

varImpPlot(rf)

minnooftree<-which.min(rf$err.rate[,1])
minnooftree

# all the variables and minimum tree
rf2 = randomForest(rf.form, data = train.df, ntree=minnooftree)
print(rf2) #29% false positive cases

rf2.train = predict(rf2, newdata=train.df[,-2], type="response")
table(rf2.train, train.df$Attrition)

rf2.test = predict(rf2, newdata=test.df[,-2], type="response")
table(rf2.test, test.df$Attrition)

##confusion matrix
confusionMatrix(rf2.train,train.df$Attrition,positive = "Yes")
confusionMatrix(rf2.test,test.df$Attrition,positive = "Yes")
# Accuracy : 0.9513 ; Sensitivity : 0.7233  (TPR)

## variable importance plot
varImpPlot(rf2)

#list of variables in the decreasing order of MeanDecreaseGini
varImp <- randomForest::importance(rf2) 

#selecting the top 25 variables to build the model
selVars <- names(sort(varImp[,1], decreasing=T))[1:25]

#add response variable and convert to a formula object
rf3.form <- as.formula(paste("Attrition",paste(selVars,collapse=' + '),sep=' ~ '))

#build 500 trees with the top 25 variables as the independent or predictor variables
rf3 <- randomForest(rf3.form, data = train.df, ntree=500, importance =T)
rf3 #29% false classification rate
plot(rf3)

minnooftree<-which.min(rf3$err.rate[,1])
minnooftree

AttritionTrainPrediction = predict(rf3, train.df[,-2])
Attritionprediction = predict(rf3, test.df[,-2])

table(AttritionTrainPrediction, train.df$Attrition)
table(Attritionprediction, test.df$Attrition)

##confusion matrix
confusionMatrix(AttritionTrainPrediction,train.df$Attrition,positive = "Yes")
confusionMatrix(Attritionprediction,test.df$Attrition,positive = "Yes")
# Accuracy : 0.9513 ; Sensitivity : 0.7233  (TPR)

#building the model with the top 25 variables and minimum tree
rf4 = randomForest(rf3.form, data = train.df, ntree=minnooftree)
print(rf4) #30% false positive

rf4.train = predict(rf4, newdata=train.df[,-2], type="response")
table(rf4.train, train.df$Attrition)

rf4.test = predict(rf4, newdata=test.df[,-2], type="response")
table(rf4.test, test.df$Attrition)

##confusion matrix
confusionMatrix(rf4.train,train.df$Attrition,positive = "Yes")
confusionMatrix(rf4.test,test.df$Attrition,positive = "Yes")
#Accuracy : 0.958  ; Sensitivity : 0.7610 (TPR)


## metric evaluation

## scoring step for the train data
rf4.train = predict(rf4, newdata=train.df[,-2], type="response")
table(rf4.train, train.df$Attrition)

train.df$predict.class <- predict(rf4, train.df, type="class")
train.df$predict.score <- predict(rf4, train.df, type="prob")
table(train.df$predict.class, train.df$Attrition)
View(train.df)

## deciling code 
decile <- function(x)
{
  deciles <- vector(length = 10)
  for (i in seq(0.1, 1, .1))
  {
    deciles[i * 10] <- quantile(x, i, na.rm = T)
  }
  return ( ifelse(x < deciles[1], 1, 
                  ifelse(x < deciles[2], 2, 
                         ifelse(x < deciles[3], 3, 
                                ifelse(x < deciles[4], 4,
                                       ifelse(x < deciles[5], 5, 
                                              ifelse(x < deciles[6], 6, 
                                                     ifelse(x < deciles[7], 7, 
                                                            ifelse(x < deciles[8], 8,
                                                                   ifelse(x < deciles[9], 9, 10)))))))))) 
}

## deciling on the basis of the predict score
train.df$deciles <- decile(train.df$predict.score[,2])

train.df$AttritionFlag<-ifelse(train.df$Attrition=="Yes",1,0)
## Ranking code
library(data.table)
tmp_DT = data.table(train.df)
rank <-
  tmp_DT[, list(
    cnt = length(AttritionFlag),
    cnt_resp = sum(AttritionFlag),
    cnt_non_resp = sum(AttritionFlag == 0)
  ) , by = deciles][order(deciles)]
rank$rrate <- rank$cnt_resp * 100 / rank$cnt

rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)

rank$cum_rel_non_resp <-
  rank$cum_non_resp / sum(rank$cnt_non_resp)

rank$ks <- abs(rank$cum_rel_resp-rank$cum_rel_non_resp)

rank ## why only 3 deciles, because one node is having 66% of the data hence explaining almost 70% of the entire data points

##Plotting ROC Curve and AUC 
library(ROCR) 
pred <- prediction(train.df$predict.score[,2], train.df$Attrition) 
perf <- performance(pred, "tpr", "fpr") 
plot(perf) 
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]]) 
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

## Computing Gini Index 
#install.packages("ineq")
library(ineq) 
gini <- ineq(train.df$predict.score[,2], type="Gini")

## Output all the values 
with(train.df, table(Attrition, predict.class)) 
auc 
KS 
gini


## scoring step for the test data
rf4.test = predict(rf4, newdata=test.df[,-2], type="response")
table(rf4.test, test.df$Attrition)

test.df$predict.class <- predict(rf4, test.df, type="class")
test.df$predict.score <- predict(rf4, test.df, type="prob")
table(test.df$predict.class, test.df$Attrition)
View(test.df)

## deciling code 
decile <- function(x)
{
  deciles <- vector(length = 10)
  for (i in seq(0.1, 1, .1))
  {
    deciles[i * 10] <- quantile(x, i, na.rm = T)
  }
  return ( ifelse(x < deciles[1], 1, 
                  ifelse(x < deciles[2], 2, 
                         ifelse(x < deciles[3], 3, 
                                ifelse(x < deciles[4], 4,
                                       ifelse(x < deciles[5], 5, 
                                              ifelse(x < deciles[6], 6, 
                                                     ifelse(x < deciles[7], 7, 
                                                            ifelse(x < deciles[8], 8,
                                                                   ifelse(x < deciles[9], 9, 10)))))))))) 
}

## deciling on the basis of the predict score
test.df$deciles <- decile(test.df$predict.score[,2])

test.df$AttritionFlag<-ifelse(test.df$Attrition=="Yes",1,0)
## Ranking code
library(data.table)
tmp_DT = data.table(test.df)
rank <-
  tmp_DT[, list(
    cnt = length(AttritionFlag),
    cnt_resp = sum(AttritionFlag),
    cnt_non_resp = sum(AttritionFlag == 0)
  ) , by = deciles][order(deciles)]
rank$rrate <- rank$cnt_resp * 100 / rank$cnt

rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)

rank$cum_rel_non_resp <-
  rank$cum_non_resp / sum(rank$cnt_non_resp)

rank$ks <- abs(rank$cum_rel_resp-rank$cum_rel_non_resp)

rank ## why only 3 deciles, because one node is having 66% of the data hence explaining almost 70% of the entire data points

##Plotting ROC Curve and AUC 
library(ROCR) 
pred <- prediction(test.df$predict.score[,2], test.df$Attrition) 
perf <- performance(pred, "tpr", "fpr") 
plot(perf) 
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]]) 
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

## Computing Gini Index 
#install.packages("ineq")
library(ineq) 
gini <- ineq(test.df$predict.score[,2], type="Gini")

## Output all the values 
with(test.df, table(Attrition, predict.class)) 
auc 
KS 
gini
