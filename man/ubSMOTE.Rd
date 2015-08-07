\name{ubSMOTE}
\alias{ubSMOTE}

\title{SMOTE}

\description{Function that implements SMOTE (synthetic minority over-sampling technique)}

\usage{ubSMOTE(X, Y, perc.over = 200, k = 5, perc.under = 200, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
          It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{perc.over}{per.over/100 is the number of new instances generated for each rare instance. 
      If perc.over < 100 a single instance is generated.}
  \item{k}{the number of neighbours to consider as the pool from where the new examples are generated}
  \item{perc.under}{perc.under/100 is the number of "normal" (majority class) instances that are randomly selected for each smoted observation.}
  \item{verbose}{print extra information (TRUE/FALSE)}
}

\details{Y must be a factor.}

\value{
  The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variable}
}

\references{Chawla, Nitesh V., et al. "SMOTE: synthetic minority over-sampling technique." arXiv preprint arXiv:1106.1813 (2011).}

\note{Original code from DMwR package}

\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubSMOTE(X=input, Y= output)
newData<-cbind(data$X, data$Y)
}