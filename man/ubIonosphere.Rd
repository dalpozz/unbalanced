\name{ubIonosphere}
\alias{ubIonosphere}
\docType{data}

\title{Ionosphere dataset}

\description{
The datasets is a modifcation of Ionosphere dataset cotained in "mlbench" package.
It contains only numerical input variables, i.e. the first two variables are removed.
The Class variable orginally taking values bad and good has been transformed into a factor where 1 denotes bad and 0 good.
}

\usage{data(ubIonosphere)}
\format{
  A data frame with 351 observations on 33 independent variables (all numerical) and one last defining the class (1 or 0).
}

\source{\url{http://cran.r-project.org/package=mlbench}}

\examples{
data(ubIonosphere)
summary(ubIonosphere)
}
\keyword{datasets}
