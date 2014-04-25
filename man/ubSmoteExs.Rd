\name{ubSmoteExs}
\alias{ubSmoteExs}

\title{ubSmoteExs}

\description{Function used in SMOTE to generate new minority examples.}

\usage{ubSmoteExs(data, tgt, N = 200, k = 5)}

\arguments{
\item{data}{the data.frame}
  \item{tgt}{the index of the target/response variables}
  \item{N}{N/100 is the number of new instances generated for each rare instance. 
      If N < 100 a single instance is generated}
  \item{k}{the number of neighbours to consider as the pool from where the new examples are generated}
}

\details{This function does not handle vectors}

\value{
  newCases
}

\seealso{
\code{\link{ubSMOTE}}
}

