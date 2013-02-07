### This file contains functions to convert "X.spmd" and "X.dmat".

demo.spmdr2dmat <- function(X.spmd, skip.balance = FALSE, comm = .SPMD.CT$comm,
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
    X.spmd <- load.balance(X.spmd, comm = comm, spmd.major = 1) 
  }
  ldim <- as.integer(dim(X.spmd))
  N <- spmd.allreduce.integer(ldim[1], integer(1), op = "sum", comm = comm)
  bldim.org <- c(spmd.allreduce.integer(ldim[1], integer(1), op = "max",
                                        comm = comm),
                 p)

  ### block-cyclic in context 2.
  X.dmat <- new("ddmatrix", Data = X.spmd,
                dim = c(N, p), ldim = ldim, bldim = bldim.org, CTXT = 2)

  ### reblock to any context and block size.
  X.dmat <- base.reblock(X.dmat, bldim = bldim, ICTXT = ICTXT)

  X.dmat
} # End of demo.spmdr2dmat().

demo.spmdc2dmat <- function(X.spmd, skip.balance = FALSE, comm = .SPMD.CT$comm,
    bldim = .DEMO.CT$bldim, ICTXT = .DEMO.CT$ictxt){
  ### check data.
  if(! is.matrix(X.spmd)){
    dim(X.spmd) <- c(1, length(X.spmd))
  }
  p <- as.integer(nrow(X.spmd))
  all.check <- spmd.allgather.integer(p, integer(comm.size(comm)), comm = comm)
  if(any(all.check != p)){
    comm.print(all.check)
    stop("X.spmd is not consistent accross processors.")
  }

  ### load balance data.
  if(! skip.balance){
    X.spmd <- load.balance(X.spmd, comm = comm, spmd.major = 2) 
  }
  ldim <- as.integer(dim(X.spmd))
  N <- spmd.allreduce.integer(ldim[2], integer(1), op = "sum", comm = comm)
  bldim.org <- c(p, spmd.allreduce.integer(ldim[2], integer(1), op = "max",
                                           comm = comm))

  ### block-cyclic in context 1.
  X.dmat <- new("ddmatrix", Data = X.spmd,
                dim = c(p, N), ldim = ldim, bldim = bldim.org, CTXT = 1)

  ### reblock to any context and block size.
  X.dmat <- base.reblock(X.dmat, bldim = bldim, ICTXT = ICTXT)

  X.dmat
} # End of demo.spmdc2dmat().

spmd2dmat <- function(X.spmd, skip.balance = FALSE, comm = .SPMD.CT$comm,
    spmd.major = .DEMO.CT$spmd.major, bldim = .DEMO.CT$bldim,
    ICTXT = .DEMO.CT$ictxt){
  if(spmd.major == 1){
    demo.spmdr2dmat(X.spmd, skip.balance = skip.balance, comm = comm,
                    bldim = bldim, ICTXT = ICTXT)
  } else if(spmd.major == 2){
    demo.spmdc2dmat(X.spmd, skip.balance = skip.balance, comm = comm,
                    bldim = bldim, ICTXT = ICTXT)
  } else{
    stop("spmd.major = 1 or 2.")
  }
} # End of spmd2dmat().


demo.dmat2spmdr <- function(X.dmat, bal.info = NULL, comm = .SPMD.CT$comm){
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
  if(base.ownany(dim(X.dmat), bldim(X.dmat), CTXT = 2)){
    X.spmd <- X.dmat@Data
  } else{
    X.spmd <- matrix(0, nrow = 0, ncol = 0)
  }

  ### unload balance data.
  if(! is.null(bal.info)){
    if(bal.info$spmd.major != 1){
      stop("spmd.major should be 1.")
    } 
    X.spmd <- unload.balance(X.spmd, bal.info, comm = comm)
  }

  X.spmd
} # End of demo.dmat2spmdr().

demo.dmat2spmdc <- function(X.dmat, bal.info = NULL, comm = .SPMD.CT$comm){
  COMM.SIZE <- comm.size(comm)

  ### check data.
  all.check <- spmd.allreduce.integer(is.ddmatrix(X.dmat), integer(1),
                   op = "sum", comm = comm) == COMM.SIZE 
  if(!all.check){
    stop("X.dmat is not consistent accross processors.")
  }

  ### block-cyclic in context 1.
  bldim.new <- c(nrow(X.dmat), ceiling(ncol(X.dmat) / COMM.SIZE))
  X.dmat <- base.reblock(X.dmat, bldim = bldim.new, ICTXT = 1)

  ### copy to spmd.
  if(base.ownany(dim(X.dmat), bldim(X.dmat), CTXT = 1)){
    X.spmd <- X.dmat@Data
  } else{
    X.spmd <- matrix(0, nrow = 0, ncol = 0)
  }

  ### unload balance data.
  if(! is.null(bal.info)){
    if(bal.info$spmd.major != 2){
      stop("spmd.major should be 2.")
    } 
    X.spmd <- unload.balance(X.spmd, bal.info, comm = comm)
  }

  X.spmd
} # End of demo.dmat2spmdc().

dmat2spmd <- function(X.dmat, bal.info = NULL, comm = .SPMD.CT$comm,
    spmd.major = .DEMO.CT$spmd.major){
  if(spmd.major == 1){
    demo.dmat2spmdr(X.dmat, bal.info = bal.info, comm = comm)
  } else if(spmd.major == 2){
    demo.dmat2spmdc(X.dmat, bal.info = bal.info, comm = comm)
  } else{
    stop("spmd.major = 1 or 2.")
  }
} # End of dmat2spmd().

