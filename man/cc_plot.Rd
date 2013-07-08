\name{ plot_dmat }
\alias{plot_dmat}
\title{ Visualizing the DMAT Data Structure }
\description{
  Plot a (small) global matrix as though it had been chopped up
  into pieces in the block-cyclic fashion.
}
\usage{
  plot_dmat(nrow, ncol, nprow, npcol, bldim, ..., labeling="blacs", col="rainbow")
}
\arguments{
  \item{nrow, ncol}{Number of global rows/columns of the matrix.}
  \item{nprow, npcol}{Number of processor rows/columns in the BLACS grid.}
  \item{bldim}{The blocking factor for the data distribution.}
  \item{...}{Additional arguments}
  \item{labeling}{Character argument; should be "blacs" or "mpi".
  This determines how the processor labeling should be, either in
  the 2-d BLACS way, or in the 1-d MPI way.}
  \item{col}{R plots color argument}
}
\details{
  This function helps the user visualize 2-d block-cyclic distributed
  data.
}
\keyword{Distributing Data}
