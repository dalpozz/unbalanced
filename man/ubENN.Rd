\name{ubENN}
\alias{ubENN}

\title{Edited Nearest Neighbor}

\description{Edited Nearest Neighbor removes any example whose class label differs from the class of at least two of its three nearest neighbors.}

\usage{ubENN(X, Y, k = 3, verbose = TRUE)}

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

\references{D. Wilson. Asymptotic properties of nearest neighbor rules using edited data. Systems, Man and Cybernetics, IEEE Transactions on, 408-421, 1972.}


\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubENN(X=input, Y= output)
newData<-cbind(data$X, data$Y)
}
