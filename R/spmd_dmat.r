### This file contains functions to convert "X.spmd" to "X.dmat".

spmd2dmat <- function(X.spmd, skip.balance = FALSE, comm = .SPMD.CT$comm,
    bldim = .DEMO.CT$bldim, ICTXT = .DEMO.CT$ictxt){
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
  if(! skip.balance){
    X.spmd <- load.balance(X.spmd) 
  }
  ldim <- as.integer(dim(X.spmd))
  N <- spmd.allreduce.integer(ldim[1], integer(1), op = "sum")
  bldim.org <- c(spmd.allreduce.integer(ldim[1], integer(1), op = "max"), p)

  ### block-cyclic in context 2.
  X.dmat <- new("ddmatrix", Data = X.spmd,
                dim = c(N, p), ldim = ldim, bldim = bldim.org, CTXT = 2)

  ### reblock to any context and block size.
  X.dmat <- base.reblock(X.dmat, bldim = bldim, ICTXT = ICTXT)

  X.dmat
} # End of spmd2dmat().


dmat2spmd <- function(X.dmat, bal.info = NULL, comm = .SPMD.CT$comm){
  COMM.SIZE <- comm.size(comm)

  ### check data.
  all.check <- spmd.allreduce.integer(is.ddmatrix(X.dmat), integer(1),
                   op = "sum", comm = comm) == COMM.SIZE 
  if(!all.check){
    stop("X.dmat is not consistent accross processors.")
  }

  ### block-cyclic in context 2.
  bldim.new <- c(ceiling(nrow(X.dmat) / COMM.SIZE), ncol(X.dmat))
  X.dmat <- base.reblock(X.dmat, bldim = bldim.new, ICTXT = 2)

  ### copy to spmd.
  X.spmd <- X.dmat@Data

  ### unload balance data.
  if(! is.null(bal.info)){
    X.spmd <- unload.balance(X.spmd, bal.info, comm = comm)

  }

  X.spmd
} # End of dmat2spmd().

