\name{load.balance}
\alias{balance.info}
\alias{load.balance}
\alias{unload.balance}
\title{Load Balance of Dataset}
\description{
  These functions will rearrange data for all processors such that the data
  amount of each processor is nearly equal.
}
\usage{
  balance.info(X.spmd, comm = .SPMD.CT$comm, spmd.major = .DEMO.CT$spmd.major,
               method = .DEMO.CT$divide.method)
  load.balance(X.spmd, bal.info = NULL, comm = .SPMD.CT$comm, 
               spmd.major = .DEMO.CT$spmd.major)
  unload.balance(new.X.spmd, bal.info, comm = .SPMD.CT$comm)
}
\arguments{
  \item{X.spmd}{a SPMD data matrix (converted if not).}
  \item{comm}{a communicator number.}
  \item{bal.info}{a returned object from \code{balance.info}.}
  \item{spmd.major}{1 for row-major storage, 2 for column-major.}
  \item{new.X.spmd}{a SPMD data matrix or vector}
  \item{method}{"block.cyclic" or "block0".}
}
\details{
  \code{X.spmd} is the data matrix with dimension \code{N.spmd * p} and
  exists on all processors where \code{N.spmd} may be vary across processors.
  If \code{X.spmd} is a vector, then it is converted to a \code{N.spmd * 1}
  matrix.

  \code{balance.info} provides the information how to balance data set
  such that all processors own similar amount of data. This information may
  be also useful for tracking where the data go or from. 

  \code{load.balance} does the job to transfer data from one processor with
  more data to the other processors with less data based on the balance
  information \code{balance.info}.

  \code{unload.balance} is the inversed function of \code{load.balance}, and
  it takes the same information \code{bal.info} to reverse the balanced
  result back to the original order.
  \code{new.X.spmd} is usually the output of \code{load.balance{X.spmd}} or
  other results of further computing of it. Again, if \code{new.X.spmd} is
  a vector, then it is converted to an one column matrix.
}
\value{
  \code{balance.info} returns a list contains two data frames and two
  vectors.

  Two data frames are \code{send} and
  \code{recv} for sending and receiving data. Each data frame has two columns
  \code{org} and \code{belong} for where data original in and new belongs.
  Number of row of \code{send} should equal to the \code{N.spmd}, and
  number of row of \code{recv} should be nearly equal to
  \code{n = N / COMM.SIZE}
  where \code{N} is the total observations of all processors.

  Two vectors are \code{N.allspmd} and \code{new.N.allspmd} which are
  all numbers of rows of \code{X.spmd} on all processes before and after
  load balance, correspondingly. Both have length equals to
  \code{comm.size(comm)}.

  \code{load.balance} returns a matrix for each processor and the matrix has
  the dimension nearly equal to \code{n * p}.

  \code{unload.balance} returns a matrix with the same length/rows as the
  original number of row of \code{X.spmd}.
}
\section{Warning(s)}{
  These function only support total object length is less than
  2^32 - 1 for machines using 32-bit integer.
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
# Save code in a file "demo.r" and run in 4 processors by
# > mpiexec -np 4 Rscript demo.r

### Setup environment.
library(pbdDEMO, quiet = TRUE)

### Generate an example data.
N.spmd <- 5 * (comm.rank() * 2)
X.spmd <- rnorm(N.spmd * 3)
dim(X.spmd) <- c(N.spmd, 3)
comm.cat("X.spmd[1:5,]\n", quiet = TRUE)
comm.print(X.spmd[1:5,], rank.print = 1, quiet = TRUE)

bal.info <- balance.info(X.spmd)
new.X.spmd <- load.balance(X.spmd, bal.info)
org.X.spmd <- unload.balance(new.X.spmd, bal.info)

comm.cat("org.X.spmd[1:5,]\n", quiet = TRUE)
comm.print(org.X.spmd[1:5,], rank.print = 1, quiet = TRUE)
if(any(org.X.spmd - X.spmd != 0)){
  cat("Unbalance fails in the rank ", comm.rank(), "\n")
}

### Quit.
finalize()
}
}
\keyword{programming}

