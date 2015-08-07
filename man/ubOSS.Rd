\name{ubOSS}
\alias{ubOSS}

\title{One Side Selection}

\description{One Side Selection is an undersampling method resulting from the application of Tomek links followed by the application of Condensed Nearest Neighbor.}

\usage{ubOSS(X, Y, verbose = TRUE)}

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
}

\references{M. Kubat, S. Matwin, et al. Addressing the curse of imbalanced training sets: one-sided selection. In MACHINE LEARNING-INTERNATIONAL WORKSHOP THEN CONFERENCE-, pages 179-186. MORGAN KAUFMANN PUBLISHERS, INC., 1997.}


\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubOSS(X=input, Y= output)
newData<-cbind(data$X, data$Y)
}

