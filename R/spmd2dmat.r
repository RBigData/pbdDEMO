### This file contains functions to convert "X.spmd" to "X.dmat".

spmd2dmat <- function(X.spmd, comm = .SPMD.CT$comm, bldim = .DEMO.CT$bldim,
    ICTXT = .DEMO.CT$ictxt){
  ### check data.
  if(! is.matrix(X.spmd)){
    dim(X.spmd) <- c(length(X.spmd), 1)
  }
  p <- as.integer(ncol(X.spmd))
  all.check <- spmd.allgather.integer(p, integer(comm.size(comm)), comm = comm)
  if(any(all.check != p)){
    comm.print(all.check)
    stop("X.spmd is not consistent accross processors.")
  }

  ### load balance data.
  X.spmd <- load.balance(X.spmd) 
  ldim <- as.integer(dim(X.spmd))
  N <- spmd.allreduce.integer(ldim[1], integer(1), op = "sum")
  bldim.org <- c(spmd.allreduce.integer(ldim[1], integer(1), op = "max"), p)

  ### block-cyclic in context 1.
  X.dmat <- new("ddmatrix", Data = X.spmd,
                dim = c(N, p), ldim = ldim, bldim = bldim.org, CTXT = 2)

  ### reblock to any context and block size.
  X.dmat <- base.reblock(X.dmat, bldim = bldim, ICTXT = ICTXT)

  X.dmat
} # End of spmd2dmat().

