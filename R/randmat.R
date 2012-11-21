# ------------------------------------
# Fixed global dimension
# ------------------------------------

# Generate large matrices which are constant by column
# Somewhat equivalent to doing matrix(0, nrow=huge, ncol=huge)
Hconst <- function(dim, bldim, const=0, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  ldim <- base.numroc(dim=dim, bldim=bldim, ICTXT=ICTXT, fixme=FALSE)
    
  if (any(ldim < 1L)){
    xmat <- matrix(0)
    ldim <- c(1, 1)
  }
  else
    xmat <- matrix(const, nrow=ldim[1L], ncol=ldim[2L])
              
  dx <- new("ddmatrix", Data=xmat,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}


# Generate Huge matrices of distributional data in parallel
# Each process determines how much it needs to generate and does so,
# no redistribution necessary.
Hunif <- function(dim, bldim, min=0, max=1, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  ldim <- base.numroc(dim=dim, bldim=bldim, ICTXT=ICTXT, fixme=FALSE)
    
  if (any(ldim < 1L)){
    xmat <- matrix(0)
    ldim <- c(1, 1)
  }
  else
    xmat <- matrix(runif(prod(ldim), min=min, max=max), nrow=ldim[1L], ncol=ldim[2L])
              
  dx <- new("ddmatrix", Data=xmat,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
            
  return(dx)
}


Hnorm <- function(dim, bldim, mean=0, sd=1, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  ldim <- base.numroc(dim=dim, bldim=bldim, ICTXT=ICTXT, fixme=FALSE)
    
  if (any(ldim < 1L)){
    xmat <- matrix(0)
    ldim <- c(1, 1)
  }
  else
    xmat <- matrix(rnorm(prod(ldim), mean=mean, sd=sd), nrow=ldim[1L], ncol=ldim[2L])
              
  dx <- new("ddmatrix", Data=xmat,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
            
  return(dx)
}



# ------------------------------------
# Fixed local dimension
# ------------------------------------

# Find "next best divisor"; used in the weak-scaling generation 
# functions below
nbd <- function(n, d)
{
  if (n < d)
    stop("'n' may not be smaller than 'd'")
  
  ret <- .Fortran("NBD", 
                  as.integer(n), as.integer(d),
                  PACKAGE="pbdDEMO")$D
  
  return( ret )
}



Hconst.local <- function(ldim, bldim, const=0, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  blacs_ <- base.blacs(ICTXT=ICTXT)
  nprows <- blacs_$NPROW
  npcols <- blacs_$NPCOL
  
  dim <- c(nprows*ldim[1L], npcols*ldim[2L])
  
  if (any( (dim %% bldim) != 0 )){
    comm.cat("WARNING : at least one margin of 'bldim' does not divide the global dimension.\n", quiet=T)
    
    bldim[1L] <- nbd(dim[1L], bldim[1L])
    bldim[2L] <- nbd(dim[2L], bldim[2L])
    comm.cat(paste("Using bldim of ", bldim[1L], "x", bldim[2L], "\n\n", sep=""), quiet=T)
  }
  
  Data <- matrix(const, nrow=ldim[1L], ncol=ldim[2L])
  
  dx <- new("ddmatrix", Data=Data,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}



Hunif.local <- function(ldim, bldim, min=0, max=1, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  blacs_ <- base.blacs(ICTXT=ICTXT)
  nprows <- blacs_$NPROW
  npcols <- blacs_$NPCOL
  
  dim <- c(nprows*ldim[1L], npcols*ldim[2L])
  
  if (any( (dim %% bldim) != 0 )){
    comm.cat("WARNING : at least one margin of 'bldim' does not divide the global dimension.\n", quiet=T)
    
    bldim[1L] <- nbd(dim[1L], bldim[1L])
    bldim[2L] <- nbd(dim[2L], bldim[2L])
    comm.cat(paste("Using bldim of ", bldim[1L], "x", bldim[2L], "\n\n", sep=""), quiet=T)
  }
  
  Data <- matrix(runif(prod(ldim), min=min, max=max), nrow=ldim[1L], ncol=ldim[2L])
  
  dx <- new("ddmatrix", Data=Data,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}



Hnorm.local <- function(ldim, bldim, mean=0, sd=1, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  blacs_ <- base.blacs(ICTXT=ICTXT)
  nprows <- blacs_$NPROW
  npcols <- blacs_$NPCOL
  
  dim <- c(nprows*ldim[1L], npcols*ldim[2L])
  
  if (any( (dim %% bldim) != 0 )){
    comm.cat("WARNING : at least one margin of 'bldim' does not divide the global dimension.\n", quiet=T)
    
    bldim[1L] <- nbd(dim[1L], bldim[1L])
    bldim[2L] <- nbd(dim[2L], bldim[2L])
    comm.cat(paste("Using bldim of ", bldim[1L], "x", bldim[2L], "\n\n", sep=""), quiet=T)
  }
  
  Data <- matrix(rnorm(prod(ldim), mean=mean, sd=sd), nrow=ldim[1L], ncol=ldim[2L])
  
  dx <- new("ddmatrix", Data=Data,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}
