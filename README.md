# unbalanced

State-of-the-art classification algorithms suffer when the data is skewed towards one class. 
This led to the development of a number of techniques to cope with unbalanced data. 
However, no technique appears to work consistently better in all conditions.
The R package **unbalanced** implements some well-known techniques for unbalanced classification tasks and 
provides a racing strategy to adaptively select the best methods for a given dataset, 
classification algorithms and accuracy measure adopted.

## Installation

You can install the stable version on
[CRAN](http://cran.r-project.org/package=unbalanced):

```{r}
install.packages('unbalanced', dependencies = TRUE)
```

or the one available in github using:

```{r}
library(devtools)
devtools::install_github("dalpozz/unbalanced")
```




## Methods for unbalanced classification

The **unbalanced** package implements some of the most well-known sampling and distance-based methods for unbalanced classification task. Within the family of sampling methods, we have functions for random undersampling (**ubUnder**) and oversampling (**ubOver**). The former remove observations from the majority class while the latter replicate minority class instances.
The package contains also a function called **ubSMOTE** that implements SMOTE, which oversamples the minority class by generating synthetic minority examples in the neighborhood of observed ones.
Other distance-based methods available in **unbalanced** are: **ubCNN**, **ubTomek**, **ubOSS**, **ubENN**, **ubNCL**.

Condensed Nearest Neighbor (CNN) (**ubCNN**) is used to select a subset of instances from the original unbalanced set which is consistent in the sense that it is correctly classified with the one-nearest neighbor rule.
Tomek Link (**ubTomek**) removes observations from the negative class that are close to the positive region in order to return a dataset that presents a better separation between the two classes.
One-sided Selection (OSS) (**ubOSS**) is an undersampling method resulting from the application of Tomek links followed by the application of CNN.
Edited Nearest Neighbor (ENN) (**ubENN**) removes any example whose class label differs from the class of at least two of its three nearest neighbors. In this way majority examples that fall in the minority region and isolated minority examples are removed. 
Neighborhood Cleaning Rule (NCL) (**ubNCL**) modifies the ENN method by increasing the role of data cleaning. Firstly, NCL removes negatives examples which are misclassified by their 3-nearest neighbors. Secondly, the neighbors of each positive examples are found and the ones belonging to the majority class are removed.


All these methods can be called by a wrapper function **ubBalance** that allows testing all these strategies by simpling changing the argument **type**.

The package includes the **ubIonosphere** datasets, which is a modification of the Ionosphere dataset contained in **mlbench** package. 
It has only numerical input variables, i.e. the first two variables are removed. 
The *Class* variable, originally taking values *bad* and *good*, has been transformed into a factor where 1 denotes the minority (bad) and 0 the majority class (good). This variable is our target and it is in the last column of the dataset.
In the following we will also called the minority class as positive and the majority as negative.

For example, let's apply oversampling to the Ionosphere dataset to have a balanced dataset.

```{r} 
set.seed(1234)
library(unbalanced)
data(ubIonosphere)
n <- ncol(ubIonosphere)
output <- ubIonosphere[ ,n]
input <- ubIonosphere[ ,-n]
#apply oversampling
data <- ubBalance(X=input, Y=output, type="ubOver", k=0)
#oversampled dataset
overData <- data.frame(data$X, Class=data$Y)
#check the frequency of the target variable after oversampling
summary(overData$Class)
```

In this case we replicate the minority class until we have as many positive as negative instances.
Alternativelly, we can balance the dataset using undersampling (i.e. removing observations from the majority class):

```s 
#apply undersampling
data <- ubBalance(X=input, Y=output, type="ubUnder", perc=50,  method="percPos")
#undersampled dataset
underData <- data.frame(data$X, Class=data$Y)
#check the frequency of the target variable after oversampling
summary(underData$Class)
```

Another well-know method for unbalanced distribution is SMOTE, which oversample the minority class by creating new synthetic observations.
Let's compare the performances of two **randomForest** classifiers, one trained on the original unbalanced dataset and a second trained on a dataset obtained after applying SMOTE.

```s 
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
#confusion matrix
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
```

Using SMOTE we alter the original class distribution and we are able to increase the number of minority instances correctly classified.
After smoting the dataset we have fewer false negatives, but a larger number of false positives.
In unbalanced classification, it often desired to correctly classify all minority instances (reducing the number of false negatives), because the cost of missing a positive instances (a false negative) is much higher than the cost of missing a negative instance (a false positive).


## Selecting the best methods

The variety of approaches available in the **unbalanced** package allows the user to test multiple unbalanced methods.
In a real situation where we have no prior information about the data distribution, it is difficult to decide which unbalanced strategy to use. 
In this case testing all alternatives is not an option either because of the associated computational cost.


A possible solution comes from the adoption of the Racing approach to perform efficiently model selection in a learning task. 
The principle of Racing consists in testing in parallel a set of alternatives and using a statistical test to determine if an alternative is significantly worse than the others. 
In that case such alternative is discarded from the competition, and the computational effort is devoted to differentiate the remaining ones. 
The *F-race* version combines the *Friedman test* with *Hoeffding Races* to eliminate inferior candidates as soon as enough statistical evidence arises against them. In F-race, the Friedman test is used to check whether there is evidence that at least one of the candidates is significantly different from others and post-tests are applied to eliminate those candidates that are significantly worse than the best one.

Here we adopt F-Race to search efficiently for the best strategy for unbalanced data. 
The candidates are assessed on different subsets of data and, each time a new assessment is made, the Friedman test is used to dismiss significantly inferior candidates.
We used a 10 fold cross validation to provide the assessment measure to the race. If a candidate is significantly better than all the others than the race is terminated without the need of using the whole dataset. In case there is not evidence of worse/better methods, the race terminates when the entire dataset is explored and the best candidate is the one with the best average result.
F-Race is available in **unbalanced** with the **ubRacing** function and its implementation is a modification of the **race** function available in the **race** package. 
The function **ubRacing** compares the 8 unbalanced methods (**ubUnder**, **ubOver**, **ubSMOTE**, **ubOSS**, **ubCNN**, **ubENN**, **ubNCL**, **ubTomek**) against the unbalanced distribution, so we have 9 candidates starting the race.

```s 
set.seed(1234)
#configuration of the sampling method used in the race
ubConf <- list(percOver=250, percUnder=150, k=3, perc=50, method="percPos", w=NULL)

# Race with 10 trees in the Random Forest to speed up results
results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, 
                    metric="auc", ubConf=ubConf, ntree=10)

# Race using 4 cores and 500 trees (default number of trees in randomForest)
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, 
#                     metric="auc", ubConf=ubConf, ncore=4)

# Let's try with a different algorithm (see mlr package for supported packages)
# library(e1071)
# results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)
```

The race terminates with 5 candidates and the best method is oversampling.
Please note that it is possible to change the type of statistical test used to remove candidates in the race with the argument **stat.test**.
When we set **stat.test = "no"**, no statistical test is performed and the race terminates when all the folds of the cross validation are explored.

## Summary

With the **unbalanced** package we have made available some of the most well-known methods for unbalanced distribution. All these methods can be called from **ubBalance** that is a wrapper to the method-specific functions.
Depending on the type of dataset, classification algorithm and accuracy measure adopted, we may have different strategies that return the best accuracy.

This consideration has lead us to adopt the F-race strategy where different candidates (unbalanced methods) are tested simultaneously. This algorithm is implemented in the **ubRacing** function which selects the best candidate without having to explore the whole dataset.

