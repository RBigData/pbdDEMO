# Generate large matrices which are constant by column
# Somewhat equivalent to doing matrix(0, nrow=huge, ncol=huge)
demo.Hconst <- function(dim, bldim, const=0, ICTXT=0)
{
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  ldim <- base.numroc(dim=dim, bldim=bldim, ICTXT=ICTXT, fixme=FALSE)
    
  if (any(ldim < 1)){
    xmat <- matrix(0)
    ldim <- c(1,1)
  }
  else
    xmat <- matrix(const, nrow=ldim[1], ncol=ldim[2])
              
  dx <- new("ddmatrix", Data=xmat,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
            
  return(dx)
}

Hconst <- demo.Hconst

# Generate Huge matrices of distributional data in parallel
# Each process determines how much it needs to generate and does so,
# no redistribution necessary.
demo.Hunif <- function(dim, bldim, mean=0, sd=1, ICTXT=0)
{
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  ldim <- base.numroc(dim=dim, bldim=bldim, ICTXT=ICTXT, fixme=FALSE)
    
  if (any(ldim < 1)){
    xmat <- matrix(0)
    ldim <- c(1,1)
  }
  else
    xmat <- matrix(runif(prod(ldim), mean=mean, sd=sd), nrow=ldim[1], ncol=ldim[2])
              
  dx <- new("ddmatrix", Data=xmat,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
            
  return(dx)
}

Hunif <- demo.Hunif

demo.Hnorm <- function(dim, bldim, mean=0, sd=1, ICTXT=0)
{
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  ldim <- base.numroc(dim=dim, bldim=bldim, ICTXT=ICTXT, fixme=FALSE)
    
  if (any(ldim < 1)){
    xmat <- matrix(0)
    ldim <- c(1,1)
  }
  else
    xmat <- matrix(rnorm(prod(ldim), mean=mean, sd=sd), nrow=ldim[1], ncol=ldim[2])
              
  dx <- new("ddmatrix", Data=xmat,
            dim=dim, ldim=ldim, bldim=bldim, CTXT=ICTXT)
            
  return(dx)
}

Hnorm <- demo.Hnorm
