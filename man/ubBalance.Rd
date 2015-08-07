\name{ubBalance}
\alias{ubBalance}

\title{Balance wrapper}

\description{The function implements several techniques to re-balance or remove noisy instances in unbalanced datasets.}

\usage{ubBalance(X, Y, type="ubSMOTE", positive=1, percOver=200, percUnder=200, 
          k=5, perc=50, method="percPos", w=NULL, verbose=FALSE)}

\arguments{
  \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset.}
  \item{type}{the balancing technique to use (ubOver, ubUnder, ubSMOTE, ubOSS, ubCNN, ubENN, ubNCL, ubTomek).}
  \item{positive}{the majority class of the response variable.}
  \item{percOver}{parameter used in ubSMOTE}
  \item{percUnder}{parameter used in ubSMOTE}
  \item{k}{parameter used in ubOver, ubSMOTE, ubCNN, ubENN, ubNCL}
  \item{perc}{parameter used in ubUnder}
  \item{method}{parameter used in ubUnder}
  \item{w}{parameter used in ubUnder}
  \item{verbose}{print extra information (TRUE/FALSE)}
}


\details{The argument type can take the following values: "ubOver" (over-sampling), "ubUnder" (under-sampling), "ubSMOTE" (SMOTE), "ubOSS" (One Side Selection), "ubCNN" (Condensed Nearest Neighbor), "ubENN" (Edited Nearest Neighbor), "ubNCL" (Neighborhood Cleaning Rule), "ubTomek" (Tomek Link).}

\value{
  The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variable}
  \item{id.rm}{index of instances removed if availble in the technique selected}
}
\references{Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.}


\seealso{
\code{\link{ubRacing}}, \code{\link{ubOver}}, \code{\link{ubUnder}}, \code{\link{ubSMOTE}}, \code{\link{ubOSS}}, \code{\link{ubCNN}}, \code{\link{ubENN}}, \code{\link{ubNCL}}, \code{\link{ubTomek}}
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