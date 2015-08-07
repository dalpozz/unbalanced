\name{ubTomek}
\alias{ubTomek}

\title{Tomek Link}

\description{The function finds the points in the dataset that are tomek link using 1-NN and then removes only majority class instances that are tomek links.}

\usage{ubTomek(X, Y, verbose = TRUE)}


\arguments{
\item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
          It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{verbose}{print extra information (TRUE/FALSE)}
}


\details{In order to compute nearest neighbors, only numeric features are allowed.}

\value{
  The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variable}
  \item{id.rm}{index of instances removed}
}

\references{I. Tomek. Two modifications of cnn. IEEE Trans. Syst. Man Cybern., 6:769-772, 1976.}

\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubTomek(X=input, Y= output)
newData<-cbind(data$X, data$Y)
}