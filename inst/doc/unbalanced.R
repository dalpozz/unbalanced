## ----load_library, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE----
library(knitr)

options(width=65)
# Make output look like output, input like input
opts_chunk$set(include=TRUE, tidy=FALSE, results='markup', message=FALSE, warning=FALSE, error=FALSE, out.width="\\textwidth", out.height="!")

## ----overIono, echo=TRUE, results='markup', cache=TRUE---------
library(unbalanced)
data(ubIonosphere)
n <- ncol(ubIonosphere)
output <- ubIonosphere[ ,n]
input <- ubIonosphere[ ,-n]

set.seed(1234)
#apply oversampling
data <- ubBalance(X=input, Y=output, type="ubOver", k=0)
#oversampled dataset
overData <- data.frame(data$X, Class=data$Y)
#check the frequency of the target variable after oversampling
summary(overData$Class)

## ----underIono, echo=TRUE, results='markup', cache=TRUE--------
#apply undersampling
data <- ubBalance(X=input, Y=output, type="ubUnder", perc=50,  method="percPos")
#undersampled dataset
underData <- data.frame(data$X, Class=data$Y)
#check the frequency of the target variable after oversampling
summary(underData$Class)

## ----rf, echo=TRUE, cache=TRUE---------------------------------
set.seed(1234)

#keep half for training and half for testing
N <- nrow(ubIonosphere)
N.tr <- floor(0.5*N)
id.tr <- sample(1:N, N.tr)
id.ts <- setdiff(1:N, id.tr)
X.tr  <- input[id.tr, ]
Y.tr <- output[id.tr]
X.ts <- input[id.ts, ] 
Y.ts <- output[id.ts]

unbalTrain <- data.frame(X.tr, Class=Y.tr)
summary(unbalTrain$Class)

library(randomForest)
#use the original unbalanced training set to build a model
model1 <- randomForest(Class ~ ., unbalTrain)
#predict on the testing set
preds <- predict(model1, X.ts, type="class")
confusionMatrix1 <- table(prediction=preds, actual=Y.ts)
print(confusionMatrix1)

#rebalance the training set before building a model
balanced <- ubBalance(X=X.tr, Y=Y.tr, type="ubSMOTE", percOver=200, percUnder=150)
balTrain <- data.frame(balanced$X, Class=balanced$Y)
summary(balTrain$Class)

#use the balanced training set
model2 <- randomForest(Class ~ ., balTrain)
#predict on the testing set
preds <- predict(model2, X.ts, type="class")
confusionMatrix2 <- table(prediction=preds, actual=Y.ts)
print(confusionMatrix2)
#we can now correctly classify more minority class instances

## ----raceFraud, echo=TRUE, cache=TRUE--------------------------

set.seed(1234)

# load the dataset
load(url("http://www.ulb.ac.be/di/map/adalpozz/data/creditcard.Rdata"))

#configuration of the sampling method used in the race
ubConf <- list(percOver=200, percUnder=200, 
               k=2, perc=50, method="percPos", w=NULL)

# Race with 10 trees in the Random Forest to speed up results
results <- ubRacing(Class ~., creditcard, "randomForest", positive=1, 
                    metric="auc", ubConf=ubConf, ntree=10)

# Race using 4 cores and 500 trees (default number of trees in randomForest)
# results <- ubRacing(Class ~., creditcard, "randomForest", positive=1, 
#                     metric="auc", ubConf=ubConf, ncore=4)


# Let's try with a different algorithm (see mlr package for supported packages)
# library(e1071)
# results <- ubRacing(Class ~., creditcard, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., creditcard, "rpart", positive=1, ubConf=ubConf)

