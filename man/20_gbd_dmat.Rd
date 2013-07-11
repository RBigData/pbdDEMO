\name{gbd_dmat}
\alias{gbd2dmat}
\alias{dmat2gbd}
\title{GBD Matrix to Distributed Dense Matrix and vice versa}
\description{
  This function convert a GBD matrix and a distributed dense matrix.
}
\usage{
  gbd2dmat(X.gbd, skip.balance = FALSE, comm = .SPMD.CT$comm,
            gbd.major = .DEMO.CT$gbd.major, bldim = .DEMO.CT$bldim, 
            ICTXT = .DEMO.CT$ictxt)
  dmat2gbd(X.dmat, bal.info = NULL, comm = .SPMD.CT$comm, 
            gbd.major = .DEMO.CT$gbd.major)
}
\arguments{
  \item{X.gbd}{a GBD matrix.}
  \item{skip.balance}{if \code{load.balance} were skipped.}
  \item{comm}{a communicator number.}
  \item{bldim}{the blocking dimension for block-cyclically distributing
               the matrix across the process grid. }
  \item{gbd.major}{1 for row-major storage, 2 for column-major.}
  \item{ICTXT}{BLACS context number for return.}

  \item{X.dmat}{a ddmatrix matrix.}
  \item{bal.info}{a returned object from \code{balance.info}.}
}
\details{
  \code{X.gbd} is a matrix with dimension \code{N.gbd * p} and
  exists on all processors. \code{N.gbd} may be vary across processors.

  If \code{skip.balance = TRUE}, then \code{load.balance} will not be called 
  and \code{X.gbd} is preassumed to be balanced.

  For demonstration purpose, these objects should not contains weird values
  such as \code{NA}.

  \code{dmat2gbd} is supposed returned a balanced gbd matrix if
  \code{bal.info} is not supplied.
}
\value{
  \code{gbd2dmat} returns a ddmatrix object.
  \code{dmat2gbd} returns a (balanced) gbd matrix.
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
mpiexec -np 4 Rscript -e "demo(gbd_dmat,'pbdDEMO',ask=F,echo=F)"
}
}
\keyword{programming}

