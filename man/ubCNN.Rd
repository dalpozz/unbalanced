\name{ubCNN}
\alias{ubCNN}

\title{Condensed Nearest Neighbor}

\description{Condensed Nearest Neighbor selects the subset of instances that are able to correctly classifing the original datasets using a one-nearest neighbor rule.}

\usage{ubCNN(X, Y, k = 1, verbose = T)}

\arguments{
  \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
  It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{k}{the number of neighbours to use}
  \item{verbose}{print extra information (TRUE/FALSE)}
}

\details{In order to compute nearest neighbors, only numeric features are allowed.}

\value{
  The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variable}
}
\references{
P. E. Hart. The condensed nearest neighbor rule. IEEE Transactions on Informa- tion Theory, 1968.
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

data<-ubCNN(X=input, Y= output)
newData<-cbind(data$X, data$Y)

}