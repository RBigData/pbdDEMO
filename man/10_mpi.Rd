\name{mpi.demo}
\alias{mpi.stat}
\alias{mpi.bin}
\alias{mpi.quantile}
\alias{mpi.ols}
\title{MPI Demonstration}
\description{
  These functions are examples of simple statistics via MPI calls.
}
\usage{
  mpi.stat(x.spmd)
  mpi.bin(x.spmd)
  mpi.quantile(x.spmd, prob = 0.5)
  mpi.ols(y.spmd, X.spmd)
}
\arguments{
  \item{x.spmd}{a SPMD vector.}
  \item{y.spmd}{a SPMD vector.}
  \item{X.spmd}{a SPMD matrix.}
}
\details{
  \code{x.spmd} and \code{y.spmd} are vectors with length \code{N.spmd}.
  \code{X.spmd} is a matrix with dimension \code{N.spmd * p} and
  exists on all processors. \code{N.spmd} may be vary across processors.

  For demonstration purpose, these objects should not contains weird values
  such \code{NA}.
}
\value{
  \code{mpi.stat} returns sample mean and sample variance.

  \code{mpi.bin} returns binning counts for the given breaks.

  \code{mpi.quantile} returns a quantile.

  \code{mpi.ols} returns ordinary least square estimates (beta_hat).
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
mpiexec -np 2 Rscript -e "demo(mpi_stat,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 2 Rscript -e "demo(mpi_bin,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 2 Rscript -e "demo(mpi_quantile,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 2 Rscript -e "demo(mpi_ols,'pbdDEMO',ask=F,echo=F)"
}
}
\keyword{programming}

