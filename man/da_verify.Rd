\name{ LinAlgVerify }
\alias{verify.svd}
\alias{verify.chol}
\alias{verify.inverse}
\alias{verify.solve}
\title{At-Scale Verification Routines for Distributed Linear Algebra}
\description{
  At-scale verification routines for distributed linear algebra.
}
\usage{
  verify.svd(nrows=1e3, ncols=1e3, mean=0, sd=1, bldim=8, tol=1e-7, 
             ICTXT = .DEMO.CT$ictxt)
  verify.chol(nrows=1e3, mean=0, sd=1, bldim=8, tol=1e-7,
             ICTXT = .DEMO.CT$ictxt)
  verify.inverse(nrows=1e3, mean=0, sd=1, bldim=8, tol=1e-7, 
             ICTXT = .DEMO.CT$ictxt)
  verify.solve(nrows=1e3, mean=0, sd=1, const=1, bldim=8, tol=1e-7, 
             ICTXT = .DEMO.CT$ictxt)
}
\arguments{
  \item{nrows, ncols}{global dimension.}
  \item{bldim}{blocking dimension.}
  \item{mean, sd}{mean and standard deviation when sampling from a 
  normal distribution.}
  \item{const}{numerical value for generating a constant \code{ddmatrix}.}
  \item{tol}{numeric tolerance for testing equality. Differences 
  smaller than \code{tol} are considered equal.}
  \item{ICTXT}{BLACS context}
}
\details{
  Finds the min, mean, and max execution time across all independent 
  processes executing the operation \code{timed}.
}
\keyword{Timing}
