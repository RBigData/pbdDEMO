\name{ read.ddmatrix.sql }
\alias{read.ddmatrix.sql}
\title{ A Simple Parallel SQL Reader }
\description{
  Read in a table from a SQL database in parallel as a distributed
  matrix.
}
\usage{
  read.ddmatrix.sql(dbname, table, bldim = .BLDIM, num.rdrs = 1, ICTXT = 0)
}
\arguments{
  \item{dbname}{database name}
  \item{table}{name of the table from \code{dbname} to be read}
  \item{bldim}{the blocking dimension for block-cyclically distributing 
  the matrix across the process grid}
  \item{num.rdrs}{numer of processes to be used to read in the table}
  \item{ICTXT}{BLACS context number for return}
}
\details{
  The function reads in data from a SQL database using the \code{sqldf}
  package into a distributed matrix.
  
  It operates at a 'bare bones' level, in that it will be assumed that
  the table desired to be read in 'looks' very much like a matrix. That
  is, it assumes that the table is basically just a csv-like structure
  that has been stowed away in a database.
}
\value{
  Returns a distributed matrix.
}
\seealso{
  \code{Distribute}
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
# Save code in a file "demo.r" and run with 2 processors by
# > mpiexec -np 2 Rscript demo.r

library(pbdDMAC, quiet = TRUE)
init.grid()

if (comm.rank()==0){
  x <- matrix(1:16, ncol=4)
} else {
  x <- NA
}

require(sqldf)

# WARNING: creates two files on disk in the working directory:
  # 'x.csv' and the database 'data'
write.table(x, "x.csv", row.names=FALSE, col.names=FALSE)
read.csv.sql("x.csv", 
  sql = "create table tabx as select * from file", dbname = "data", 
  header=FALSE, sep=" "
)

dx <- read.ddmatrix.sql(dbname="data", table="tabx", bldim=c(4,4),
                        num.rdrs=2, CTXT=0)
print(dx)

finalize()
}
}
\keyword{Distributing Data}
