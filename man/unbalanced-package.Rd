\name{unbalanced-package}
\alias{unbalanced-package}
\alias{unbalanced}
\docType{package}

\title{Racing for Unbalanced Methods Selection}

\description{A dataset is said to be unbalanced when the class of interest (minority class) is much rarer than normal behaviour (majority class). The cost of missing a minority class is typically much higher that missing a majority class. Most learning systems are not prepared to cope with unbalanced data and several techniques have been proposed to rebalance the classes. This package implements some of most well-known techniques and propose a racing algorithm [2] to select adaptively the most appropriate strategy for a given unbalanced task [1].
}

\details{
\tabular{ll}{
Package: \tab unbalanced\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2015-06-17\cr
License: \tab GPL (>= 3)\cr
}

}

\author{
Andrea Dal Pozzolo \email{adalpozz@ulb.ac.be}, Olivier Caelen \email{olivier.caelen@worldline.com} and Gianluca Bontempi \email{gbonte@ulb.ac.be}

Maintainer: Andrea Dal Pozzolo

Andrea Dal Pozzolo and Gianluca Bontempi are with the \href{http://mlg.ulb.ac.be}{Machine Learning Group}, Computer Science Department, Faculty of Sciences ULB, Universite Libre de Bruxelles, Brussels, Belgium.\cr
Olivier Caelen is with the Fraud Risk Management Analytics, Worldline, Belgium.\cr
The work of Andrea Dal Pozzolo is supported by the Doctiris scholarship of Innoviris, Belgium.
}

\references{
1. Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.\cr 
2. Birattari, Mauro, et al. "A Racing Algorithm for Configuring Metaheuristics."GECCO. Vol. 2. 2002.
}

\keyword{unbalanced datasets, imbalanced learning}


\seealso{
\code{\link{ubBalance}}, \code{\link{ubRacing}}
}


\examples{
#use Racing to select the best technique for an unbalanced dataset
library(unbalanced)
data(ubIonosphere)

#configure sampling parameters
ubConf <- list(percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)

#load the classification algorithm that you intend to use inside the Race
#see 'mlr' package for supported algorithms
library(randomForest)
#use only 5 trees
results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=5)

# try with 500 trees
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=500)
# let's try with a different algorithm
# library(e1071)
# results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)
}
