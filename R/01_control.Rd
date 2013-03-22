\name{DEMO Control}
\alias{.DEMO.CT}
\docType{data}
\title{A set of controls in pbdDEMO}
\description{
  This set of controls is used to provide default values in this package.
}
\format{
  Objects contain several default parameters for BLACS.
}
\details{
  The elements of \code{.DEMO.CT} are default values
\tabular{lcl}{
  Elements              \tab Default        \tab Usage \cr
  \code{spmd.major}     \tab 1L             \tab a default SPMD row-major \cr
  \code{ictxt}          \tab 0L             \tab a default BLACS context \cr
  \code{bldim}          \tab c(2L,2L)       \tab a default block dimension \cr
  \code{divide.method}  \tab "block.cyclic" \tab a default balance method \cr
}
}
\references{
  Programming with Big Data in R Website:
  \url{http://r-pbd.org/}
}
\author{
  Drew Schmidt \email{schmidt AT math.utk.edu}, Wei-Chen Chen,
  George Ostrouchov, and Pragneshkumar Patel.
}
\keyword{global variables}
