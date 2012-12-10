\name{ timer }
\alias{timer}
\title{ A Timing Function for SPMD Routines }
\description{
  A timing function for use with parallel codes executed in the batch
  SPMD style.
}
\usage{
  timer(timed)
}
\arguments{
  \item{timed}{expression to be timed.}
}
\details{
  Finds the min, mean, and max execution time across all independent 
  processes executing the operation \code{timed}.
}
\keyword{Timing}
