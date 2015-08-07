\name{ubUnder}
\alias{ubUnder}

\title{Under-sampling}

\description{The function removes randomly some instances from the majority (negative) class and keeps all instances in the minority (positive) class in order to obtain a more balanced dataset.
It allows two ways to perform undersampling: 
i) by setting the percentage of positives wanted after undersampling (percPos method),
ii) by setting the sampling rate on the negatives, (percUnder method).
For percPos, "perc"has to be (N.1/N * 100) <=  perc <= 50, where N.1 is the number of positive and N the total number of instances. 
For percUnder, "perc"has to be (N.1/N.0 * 100) <=  perc <= 100, where N.1 is the number of positive and N.0 the number of negative instances. 
}

\usage{ubUnder(X, Y, perc = 50, method = "percPos", w = NULL)}


\arguments{
  \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
          It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{perc}{percentage of sampling.}
  \item{method}{method to perform under sampling ("percPos", "percUnder").}
  \item{w}{weights used for sampling the majority class, if NULL all majority instances are sampled with equal weights}
}


\value{
The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variable}
  \item{id.rm}{index of instances removed}
}


\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubUnder(X=input, Y= output, perc = 40,  method = "percPos")
newData<-cbind(data$X, data$Y)
}