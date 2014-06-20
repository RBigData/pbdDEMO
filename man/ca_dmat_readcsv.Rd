\name{ read.csv.ddmatrix }
\alias{read.csv.ddmatrix}
\title{ A Simple Parallel CSV Reader }
\description{
  Read in a table from a CSV file in parallel as a distributed
  matrix.
}
\usage{
  read.csv.ddmatrix(file, sep=",", nrows, ncols, header=FALSE, bldim=4, 
                    num.rdrs=1, ICTXT=0)
}
\arguments{
  \item{file}{csv file name.}
  \item{sep}{separator character.}
  \item{nrows, ncols}{dimensions of the csv file. Allowed to be 
  missing in function call.}
  \item{header}{logical indicating presence/absence of character
  header for file.}
  \item{bldim}{the blocking dimension for block-cyclically distributing 
  the matrix across the process grid}
  \item{num.rdrs}{numer of processes to be used to read in the table}
  \item{ICTXT}{BLACS context number for return}
}
\details{
  The function reads in data from a csv file into a distributed 
  matrix.  This function sits somewhere between \code{scan()} and
  \code{read.csv()}, but for parallel reads into a distributed matrix.
  
  The arguments \code{nrow=} and \code{ncol=} are optional.  In the 
  case that they are left blank, they will be determined.  However, 
  note that doing so is costly, so knowing the dimensions beforehand
  can greatly improve performance.
  
  Although frankly, the performance-minded should not be using csv's
  in the first place.  Consider using the \code{pbdNCDF4} package
  for managing data.
}
\value{
  Returns a distributed matrix.
}
\keyword{Distributing Data}
