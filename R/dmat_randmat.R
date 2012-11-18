# ------------------------------------
# Fixed global dimension
# ------------------------------------

# Generate large matrices which are constant by column
# Somewhat equivalent to doing matrix(0, nrow=huge, ncol=huge)
demo.Hconst <- function(dim, bldim, const=0, ICTXT=0)
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

Hconst <- demo.Hconst

# Generate Huge matrices of distributional data in parallel
# Each process determines how much it needs to generate and does so,
# no redistribution necessary.
demo.Hunif <- function(dim, bldim, min=0, max=1, ICTXT=0)
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

Hunif <- demo.Hunif

demo.Hnorm <- function(dim, bldim, mean=0, sd=1, ICTXT=0)
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

Hnorm <- demo.Hnorm


# ------------------------------------
# Fixed local dimension
# ------------------------------------

demo.Hconst.local <- function(ldim, bldim, const=0, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  blacs_ <- base.blacs(ICTXT=ICTXT)
  nprows <- blacs_$NPROW
  npcols <- blacs_$NPCOL
  
  dim <- c(nprows*ldim[1L], npcols*ldim[2L])
  
  if (any( (dim %% bldim) != 0 )){
    comm.cat("WARNING : at least one margin of 'bldim' does not divide the global dimension.\n", quiet=T)
    
    bldim[1L] <- .Fortran("NBD", as.integer(dim[1L]), D=as.integer(bldim[1L]))$D
    bldim[2L] <- .Fortran("NBD", as.integer(dim[2L]), D=as.integer(bldim[2L]))$D
    comm.cat(paste("Using bldim of ", bldim[1L], "x", bldim[2L], "\n\n", sep=""), quiet=T)
  }
  
  Data <- matrix(const, nrow=ldim[1L], ncol=ldim[2L])
  
  dx <- new("ddmatrix", Data=Data,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}

Hconst.local <- demo.Hconst.local




demo.Hunif.local <- function(ldim, bldim, min=0, max=1, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  blacs_ <- base.blacs(ICTXT=ICTXT)
  nprows <- blacs_$NPROW
  npcols <- blacs_$NPCOL
  
  dim <- c(nprows*ldim[1L], npcols*ldim[2L])
  
  if (any( (dim %% bldim) != 0 )){
    comm.cat("WARNING : at least one margin of 'bldim' does not divide the global dimension.\n", quiet=T)
    
    bldim[1L] <- .Fortran("NBD", as.integer(dim[1L]), D=as.integer(bldim[1L]))$D
    bldim[2L] <- .Fortran("NBD", as.integer(dim[2L]), D=as.integer(bldim[2L]))$D
    comm.cat(paste("Using bldim of ", bldim[1L], "x", bldim[2L], "\n\n", sep=""), quiet=T)
  }
  
  Data <- matrix(runif(prod(ldim), min=min, max=max), nrow=ldim[1L], ncol=ldim[2L])
  
  dx <- new("ddmatrix", Data=Data,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}

Hunif.local <- demo.Hunif.local



demo.Hnorm.local <- function(ldim, bldim, mean=0, sd=1, ICTXT=0)
{
  if (length(bldim)==1L)
    bldim <- rep(bldim, 2L)
  
  blacs_ <- base.blacs(ICTXT=ICTXT)
  nprows <- blacs_$NPROW
  npcols <- blacs_$NPCOL
  
  dim <- c(nprows*ldim[1L], npcols*ldim[2L])
  
  if (any( (dim %% bldim) != 0 )){
    comm.cat("WARNING : at least one margin of 'bldim' does not divide the global dimension.\n", quiet=T)
    
    bldim[1L] <- .Fortran("NBD", as.integer(dim[1L]), D=as.integer(bldim[1L]))$D
    bldim[2L] <- .Fortran("NBD", as.integer(dim[2L]), D=as.integer(bldim[2L]))$D
    comm.cat(paste("Using bldim of ", bldim[1L], "x", bldim[2L], "\n\n", sep=""), quiet=T)
  }
  
  Data <- matrix(rnorm(prod(ldim), mean=mean, sd=sd), nrow=ldim[1L], ncol=ldim[2L])
  
  dx <- new("ddmatrix", Data=Data,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
  
  return(dx)
}

Hnorm.local <- demo.Hnorm.local
