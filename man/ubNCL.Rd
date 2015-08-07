\name{ubNCL}
\alias{ubNCL}

\title{Neighborhood Cleaning Rule}
\description{
Neighborhood Cleaning Rule modifies the Edited Nearest Neighbor method by increasing the role of data cleaning.
Firstly, NCL removes negatives examples which are misclassified by their 3-nearest neighbors. 
Secondly, the neighbors of each positive examples are found and the ones belonging to the majority class are removed.
}

\usage{ubNCL(X, Y, k = 3, verbose = TRUE)}

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

\references{J. Laurikkala. Improving identification of difficult small classes by balancing class distribution. Artificial Intelligence in Medicine, pages 63-66, 2001.}


\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubNCL(X=input, Y= output)
newData<-cbind(data$X, data$Y)
}