\name{ubOver}
\alias{ubOver}

\title{Over-sampling}

\description{
The function replicates randomly some instances from the minority class in order to obtain a final dataset with the same number of instances from the two classes.}

\usage{ubOver(X, Y, k = 0, verbose=TRUE)}

\arguments{
  \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
          It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{k}{defines the sampling method.}
  \item{verbose}{print extra information (TRUE/FALSE)}
}


\details{
If K=0: sample with replacement from the minority class until we have the same number of instances in each class.
If K>0: sample with replacement from the minority class until we have k-times the orginal number of minority instances.
}

\value{
  The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variable}
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

data<-ubOver(X=input, Y= output)
newData<-cbind(data$X, data$Y)
}