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
  mpi.stat(x.gbd)
  mpi.bin(x.gbd, breaks = pi/3 * (-3:3))
  mpi.quantile(x.gbd, prob = 0.5)
  mpi.ols(y.gbd, X.gbd)
}
\arguments{
  \item{x.gbd}{a GBD vector.}
  \item{breaks}{a set to break data in groups.}
  \item{prob}{a desired probability for quantile.}
  \item{y.gbd}{a GBD vector.}
  \item{X.gbd}{a GBD matrix.}
}
\details{
  \code{x.gbd} and \code{y.gbd} are vectors with length \code{N.gbd}.
  \code{X.gbd} is a matrix with dimension \code{N.gbd * p} and
  exists on all processors. \code{N.gbd} may be vary across processors.

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
  Drew Schmidt \email{schmidt AT math.utk.edu}, Wei-Chen Chen,
  George Ostrouchov, and Pragneshkumar Patel.
}
\examples{
\dontrun{
### Under command mode, run the demo with 4 processors by
### (Use Rscript.exe for windows system)
mpiexec -np 4 Rscript -e "demo(sample_stat,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 4 Rscript -e "demo(binning,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 4 Rscript -e "demo(quantile,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 4 Rscript -e "demo(ols,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 4 Rscript -e "demo(gbd2dmat,'pbdDEMO',ask=F,echo=F)"
mpiexec -np 4 Rscript -e "demo(balance,'pbdDEMO',ask=F,echo=F)"
}
}
\keyword{programming}

