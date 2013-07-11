image_dmat <- function(nrow, ncol, nprow, npcol, bldim, ..., labeling="blacs")
{
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  x <- matrix(0L, nrow=nrow, ncol=ncol)
  
  labeling <- match.arg(tolower(labeling), c("blacs", "mpi"))
  
#  if (labeling=="blacs")
#    proc <- c(0L, 0L)
#  else
#    proc <- 0L
  
  pr <- pc <- 0L
  
  for (j in 1:ncol){
    for (i in 1:nrow){
      pr <- floor( (i-1L)/bldim[1L] ) %% nprow
      pc <- floor( (j-1L)/bldim[2L] ) %% npcol
      
      proc <- pr * npcol + pc
      x[i, j] <- proc
    }
  }
  
  return( x )
}


plot_dmat <- function(nrow, ncol, nprow, npcol, bldim, ..., labeling="blacs", col="rainbow")
{
  if (length(bldim) == 1)
    bldim <- rep(bldim, 2L)
  
  col <- match.arg(tolower(col), c("rainbow", "heat.colors", "terrain.colors", "topo.colors", "cm.colors"))
  
  x <- image_dmat(nrow=nrow, ncol=ncol, nprow=nprow, npcol=npcol, bldim=bldim, labeling=labeling)
  
  x <- apply(X=x, MARGIN=2L, FUN=rev)
  x <- apply(X=x, MARGIN=1L, FUN=rev)
  x <- apply(X=x, MARGIN=2L, FUN=rev)
  
  col <- eval(parse(text=col))(nprow*npcol)
  
  # plot image
  image(x, axes=FALSE, useRaster=TRUE, col=col)
  
##  # add text
##  nrblocks <- nrow/bldim[1L]
##  ncblocks <- ncol/bldim[2L]
##  
##  len <- 1.1
##  offset <- .05
##  rmid <- len/nrblocks
##  cmid <- len/ncblocks
##  
##  print(ncblocks)
##  
##  text(1/nrow-offset, 1-offset, label="0")
##  text(1/nrow-offset, 1/ncol-offset + 4/ncol, label="0")
##  
##  text(1/nrow-offset + 2/nrow, 1-offset, label="1")
}

