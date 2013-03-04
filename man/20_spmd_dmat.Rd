\name{spmd_dmat}
\alias{spmd2dmat}
\alias{dmat2spmd}
\title{SPMD Matrix to Distributed Dense Matrix and vice versa}
\description{
  This function convert a SPMD matrix and a distributed dense matrix.
}
\usage{
  spmd2dmat(X.spmd, skip.balance = FALSE, comm = .SPMD.CT$comm,
            spmd.major = .DEMO.CT$spmd.major, bldim = .DEMO.CT$bldim, 
            ICTXT = .DEMO.CT$ictxt)
  dmat2spmd(X.dmat, bal.info = NULL, comm = .SPMD.CT$comm, 
            spmd.major = .DEMO.CT$spmd.major)
}
\arguments{
  \item{X.spmd}{a SPMD matrix.}
  \item{skip.balance}{if \code{load.balance} were skipped.}
  \item{comm}{a communicator number.}
  \item{bldim}{the blocking dimension for block-cyclically distributing
               the matrix across the process grid. }
  \item{spmd.major}{l}
  \item{ICTXT}{BLACS context number for return.}

  \item{X.dmat}{a ddmatrix matrix.}
  \item{bal.info}{a returned object from \code{balance.info}.}
}
\details{
  \code{X.spmd} is a matrix with dimension \code{N.spmd * p} and
  exists on all processors. \code{N.spmd} may be vary across processors.

  If \code{skip.balance = TRUE}, then \code{load.balance} will not be called 
  and \code{X.spmd} is preassumed to be balanced.

  For demonstration purpose, these objects should not contains weird values
  such as \code{NA}.

  \code{dmat2spmd} is supposed returned a balanced spmd matrix if
  \code{bal.info} is not supplied.
}
\value{
  \code{spmd2dmat} returns a ddmatrix object.
  \code{dmat2spmd} returns a (balanced) spmd matrix.
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
mpiexec -np 4 Rscript -e "demo(spmd_dmat,'pbdDEMO',ask=F,echo=F)"
}
}
\keyword{programming}

