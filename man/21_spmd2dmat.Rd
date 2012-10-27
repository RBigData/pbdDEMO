\name{spmd2dmat}
\alias{spmd2dmat}
\title{SPMD Matrix to Distributed Dense Matrix}
\description{
  This function convert a SPMD matrix to a distributed dense matrix.
}
\usage{
  spmd2dmat(X.spmd, comm = .SPMD.CT$comm, bldim = .DEMO.CT$bldim,
            ICTXT = .DEMO.CT$ictxt)
}
\arguments{
  \item{X.spmd}{a SPMD matrix.}
  \item{comm}{a communicator number.}
  \item{bldim}{the blocking dimension for block-cyclically distributing
               the matrix across the process grid. }
  \item{ICTXT}{BLACS context number for return.}
}
\details{
  \code{X.spmd} is a matrix with dimension \code{N.spmd * p} and
  exists on all processors. \code{N.spmd} may be vary across processors.

  For demonstration purpose, these objects should not contains weird values
  such \code{NA}.
}
\value{
  \code{spmd2dmat} returns a ddmatrix object.
}
\references{
  Programming with Big Data in R Website:
  \url{http://r-pbd.org/}
}
\author{
  Drew Schmidt, Wei-Chen Chen \email{wccsnow@gmail.com},
  George Ostrouchov, Pragneshkumar Patel.
}
\examples{
\dontrun{
### Under command mode, run the demo with 2 processors by
### (Use Rscript.exe for windows system)
mpiexec -np 2 Rscript -e "demo(spmd2dmat,'pbdDEMO',ask=F,echo=F)"
}
}
\keyword{programming}

