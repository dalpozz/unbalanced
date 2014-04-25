\name{unbalanced-package}
\alias{unbalanced-package}
\alias{unbalanced}
\docType{package}

\title{Techniques for unbalanced dataset}

\description{A dataset is said to be unbalanced when the class of interest (minority class) is much rarer than normal behaviour (majority class). The cost of missing a minority class is typically much higher that missing a majority class. Most learning systems are not prepared to cope with unbalanced data.\cr
The proposed strategies essentially belong to the following categories: sampling and distance-based. 
Sampling techniques up-sample or down-sample a class of observations. SMOTE generates synthetic minority examples. Distance based techniques use distances between input points to under-sample or to remove noisy and borderline examples.
}

\details{
\tabular{ll}{
Package: \tab unbalanced\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2014-01-27\cr
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
Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.
}

\keyword{unbalanced datasets, imbalanced learning}


\seealso{
\code{\link{ubBalance}}
}


\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

#balance the dataset
data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balancedData<-cbind(data$X,data$Y)
}
