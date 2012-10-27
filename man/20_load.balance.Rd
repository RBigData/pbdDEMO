\name{load.balance}
\alias{balance.info}
\alias{load.balance}
\title{Load Balance of Dataset}
\description{
  These functions will rearrance data for all processors such that the data
  amount of each processor is nearly equal.
}
\usage{
  balance.info(X.spmd)
  load.balance(X.spmd)
}
\arguments{
  \item{X.spmd}{a SPMD data set.}
}
\details{
  \code{\link{X.spmd}} is the data set with dimension \code{N.spmd * p} and
  exists on all processors where \code{N.spmd} may be vary across processors.

  \code{balance.info} provides the information how to balance data set
  such that all processors own similar amount of data. This information may
  be also useful for tracking where the data go or from. 

  \code{load.balance} does the job to transfer data from one processor with
  more data to the other processors with less data based on the balance
  information \code{balance.info}.
}
\value{
  \code{balance.info} returns a list contains two data frames \code{send} and
  \code{recv} for sending and receiving data. Each data frame has two columns
  \code{org} and \code{belong} for where data original in and new belongs.
  Number of row of \code{send} should equal to the \code{N.spmd}, and
  number of row of \code{recv} should be nearly equal to
  \code{n = N / COMM.SIZE}
  where \code{N} is the total observations of all processors.

  \code{load.balance} returns a matrix for each processor and the matrix has
  the dimension nearly equal to \code{n * p}.
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
# Save code in a file "demo.r" and run in 4 processors by
# > mpiexec -np 4 Rscript demo.r

### Setup environment.
library(pbdDEMO, quiet = TRUE)

### Generate an example data.
X.spmd <- matrix(0, nrow = 10 * (comm.rank() + 1), ncol = 3)
new.X.spmd <- load.balance(X.spmd)
comm.print(dim(new.X.spmd), all.rank = TRUE)

### Quit.
finalize()
}
}
\keyword{programming}

