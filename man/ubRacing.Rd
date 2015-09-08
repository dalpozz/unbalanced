\name{ubRacing}
\alias{ubRacing}

\title{Racing}

\description{The function implementes the Racing algorithm [2] for selecting the best technique to re-balance or remove noisy instances in unbalanced datasets [1].}

\usage{
ubRacing(formula, data, algo, positive=1, ncore=1, nFold=10, maxFold=10, maxExp=100, 
          stat.test="friedman", metric="f1", ubConf, verbose=FALSE, ...)
}

\arguments{
  \item{formula}{formula describing the model to be fitted.}
  \item{data}{the unbalanced dataset}
  \item{algo}{the classification algorithm to use with the mlr package.}
  \item{positive}{label of the positive (minority) class.}
  \item{ncore}{the number of core to use in the Race. Race is performed with parallel exectuion when ncore > 1.}
  \item{nFold}{number of folds in the cross-validation that provides the subset of data to the Race}
  \item{maxFold}{maximum number of folds to use in the Race}
  \item{maxExp}{maximum number of experiments to use in the Race}
  \item{stat.test}{statistical test to use to remove candidates which perform significantly worse than the best.}
  \item{metric}{metric used to asses the classification (f1, auc or gmean).}
  \item{ubConf}{configuration of the balancing techniques used in the Race.}
  \item{verbose}{print extra information (TRUE/FALSE)}
  \item{\dots}{additional arguments pass to train function in mlr package.}
}

\details{The argument metric can take the following values: "gmean", "f1" (F-score or F-measure), "auc" (Area Under ROC curve).  Argument stat.test defines the statistical test used to remove candidates during the race. It can take the following values: "friedman" (Friedman test), "t.bonferroni" (t-test with bonferroni correction), "t.holm" (t-test with holm correction), "t.none" (t-test without correction), "no" (no test, the Race continues until new subsets of data are provided by the cross validation). Argument ubConf is a list passed to function ubBalance that is used for configuration.}

\value{
 The function returns a list: 
  \item{Race}{matrix containing accuracy results for each technique in the Race.}              
  \item{best}{best technique selected in the Race.}
  \item{avg}{average of the metric used in the Race for the technique selected.}
  \item{sd}{standard deviation of the metric used in the Race for the technique selected.}
  \item{N.test}{number of experiments used in the Race.}
  \item{Gain}{\% of computational gain with resepct to the maximum number of experiments given by the cross validation.}
}
\references{1. Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.\cr 
2. Birattari, Mauro, et al. "A Racing Algorithm for Configuring Metaheuristics."GECCO. Vol. 2. 2002.}

\note{The function ubRacing is a modified version of the race function availble in the race package: \url{http://cran.r-project.org/package=race}.}

\seealso{
\code{\link{ubBalance}}, \code{\link{ubOver}}, \code{\link{ubUnder}}, \code{\link{ubSMOTE}}, \code{\link{ubOSS}}, \code{\link{ubCNN}}, \code{\link{ubENN}}, \code{\link{ubNCL}}, \code{\link{ubTomek}}
}

\examples{
#use Racing to select the best technique for an unbalanced dataset
library(unbalanced)
data(ubIonosphere)

#configure sampling parameters
ubConf <- list(percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)

#load the classification algorithm that you intend to use inside the Race
#see 'mlr' package for supported algorithms
library(randomForest)
#use only 5 trees
results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=5)

# try with 500 trees
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=500)
# let's try with a different algorithm
# library(e1071)
# results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# library(rpart)
# results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
