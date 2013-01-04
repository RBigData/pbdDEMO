\name{ read.sql.ddmatrix }
\alias{read.sql.ddmatrix}
\title{ A Simple Parallel SQL Reader }
\description{
  Read in a table from a SQL database in parallel as a distributed
  matrix.
}
\usage{
  read.sql.ddmatrix(dbname, table, bldim = .BLDIM, num.rdrs = 1, ICTXT = 0)
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
\keyword{Distributing Data}
