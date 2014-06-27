read.csv.ddmatrix <- function(file, sep=",", nrows, ncols, header=FALSE, bldim=4, num.rdrs=1, ICTXT=0)
{
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  if (header)
    start <- 1
  else
    start <- 0
  
  msng <- FALSE # for printing a warning if nrows or ncols is missing
  if (missing(ncols))
  {
    msng <- TRUE
    if (comm.rank()==0)
      ncols <- length(scan(file=file, skip=start, sep=sep, nlines=1L, quiet=T))
    else 
      ncols <- 0L
    ncols <- pbdMPI::allreduce(ncols, op='sum')
  }
  
  # estimate number of rows based on number columns and file size in bytes
  # should be a slight overestimate
  if (missing(nrows))
  {
    msng <- TRUE
    if (comm.rank()==0){
      seps <- ncols * (length(unlist(strsplit(sep, split="")))) - 1
      x <- length(unlist(strsplit(scan(file=file, sep=sep, nlines=1L, quiet=T, what='character'), split="")))
      nrows <- ceiling( file.info(file)[1] / (x+seps) )#(ncols + seps) ) # adjust for sep character
    } else {
      nrows <- 0L
    }
    nrows <- pbdMPI::allreduce(nrows, op='sum')
  }
  
  dim <- c(nrows, ncols)
  
  nprocs <- comm.size()
  
  newgrid <- FALSE # flag for 'did we create a new grid'
  if (num.rdrs > nprocs)
  {
    comm.warning("Number of readers supplied is less than number requested; defaulting to ", nprocs, " readers")
    num.rdrs <- nprocs
  }
  
  # special case of 1 reader; just read on process 0
  if (num.rdrs == 1)
  {
    if (comm.rank()==0)
    {
      Data <- as.matrix(read.csv(file=file, sep=sep))
      dim <- dim(Data)
    } 
    else 
    {
      Data <- matrix(0)
      dim <- c(0, 0)
    }
    
    dim <- allreduce(dim, op='sum')
    ldim <- dim(Data)
    tmpbl <- dim
    
    out <- new("ddmatrix", Data=Data, dim=dim, ldim=ldim,
              bldim=tmpbl, ICTXT=0)
  
    if (ICTXT != 0 || any(tmpbl != bldim) )
      out <- dmat.redistribute(dx=out, bldim=bldim, ICTXT=ICTXT)
    
    return(out)
    
  } 
  else if (num.rdrs == nprocs)
  {
    MYCTXT <- 2
  } 
  else 
  {
    MYCTXT <- base.minctxt()
    
    blacs_gridinit(ICTXT=MYCTXT, NPROW=num.rdrs, NPCOL=1L)
    
    newgrid <- TRUE
  }
  
  blacs_ <- base.blacs(MYCTXT)
  
  # each process grabs its data
  nlines <- ceiling(dim[1] / num.rdrs)
  tmpbl <- c(nlines, ncols)
  if (blacs_$MYROW != -1)
  {
    skip <- comm.rank() * nlines + start
    x <- scan(file=file, skip=skip, sep=sep, nlines=nlines, quiet=T)
  } else
    x <- NULL
  
  # determine true dimensions based on loaded data size --- recall
  # that the original number of rows was overestimated
  if (msng)
  {
    ldim <- length(x) / ncols
    dim[1] <- pbdMPI::allreduce(ldim, op='sum')
    
    if (ldim==0)
    {
      ldim <- c(1,1)
      Data <- matrix(0)
    } 
    else 
    {
      ldim <- c(ldim, ncols)
      Data <- matrix(x, nrow=ldim[1L], ncol=ldim[2L], byrow=T)
    }
  } 
  else 
  {
    if (is.null(x) || length(x)==0L)
      Data <- matrix(0)
    else 
      Data <- matrix(x, ncol=dim[2L], byrow=T)
    
    ldim <- dim(Data)
  }
  
  out <- new("ddmatrix", Data=Data, dim=dim, ldim=ldim, bldim=tmpbl, ICTXT=MYCTXT)
  
  if (ICTXT != MYCTXT || any(tmpbl != bldim))
    out <- dmat.redistribute(dx=out, bldim=bldim, ICTXT=ICTXT)
  
  if (newgrid)
    gridexit(MYCTXT)
  
  if (msng)
    comm.warning(paste("Failing to supply nrows= and ncols= will dramatically impact performance.\nFor future reference, this file is ", dim[1], "x", dim[2], sep=""))
  
  return(out)
}


