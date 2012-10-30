# name of the function is terrible; FIXME
# Assumes the database is 'final', meaning that the data is stored
  # effectively like a csv.  Meaning, the data looks in the db
  # like it should look in the matrix.  Also assumes all 
  # columns are numeric since this is a distributed MATRIX, not
  # a distributed dataframe.
read.ddmatrix.sql <- function(dbname, table, bldim=.BLDIM, num.rdrs=1, ICTXT=0)
{
  nprocs <- comm.size()
  
  newgrid <- FALSE # flag for 'did we create a new grid' in ICTXT 3
  if (num.rdrs > nprocs){
    warning("Number of readers supplied is less than number requested; defaulting to", nprocs, "readers")
    num.rdrs <- nprocs
  }
  
  if (num.rdrs == nprocs){
    MYCTXT <- 2
  } else {
    assign(x=".__blacs_gridinfo_3", 
       value=.Fortran("mpi_blacs_initialize", 
              NPROW=as.integer(num.rdrs), NPCOL=as.integer(1), 
              ICTXT=as.integer(3), MYROW=as.integer(0), 
              MYCOL=as.integer(0) ),
       envir=.GlobalEnv )
    MYCTXT <- 3
    newgrid <- TRUE
  }
  
  blacs_ <- blacs(MYCTXT)
  
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  # determine dimension on first process then scatter to others, so the db only gets queried once
  if (blacs_$MYROW==0 && blacs_$MYCOL==0){
    nrow <- sqldf(paste("SELECT COUNT(*) FROM", table), dbname=dbname)
    # make sure columns are numeric
    x <- sqldf(paste("SELECT * FROM ", table, " WHERE rowid  = 1"), dbname=dbname)
    if(!all(unlist(lapply(seq_along(x), function(i) is.numeric(x[, i])))))
      stop("All data must be numeric")
    ncol <- ncol(x)
  } else {
    nrow <- ncol <- 0
  }
  
  nrow <- allreduce(as.double(nrow), op='max')
  ncol <- allreduce(as.double(ncol), op='max')
  
  dim <- c(nrow, ncol)
  
  # determine which rows each process should grab
  if (blacs_$MYROW != -1){
    modulus <- num.rdrs * bldim[1]
    
    myrows <- blacs_$MYROW*bldim[1]
    myrows <- seq(myrows, myrows+bldim[1]-1) + 1
    myrows <- myrows %% modulus

    query <- paste("SELECT * FROM ", table, " WHERE rowid % ", modulus, " in (", paste(myrows, collapse=", "), ")", sep="")
    
    x <- unlist(sqldf(query, dbname=dbname))
    if (!is.double(x))
      x <- as.double(x)

    Data <- matrix(x, ncol=dim[2])
  } else {
    Data <- matrix(0)
  }

  out <- new("ddmatrix", Data=Data, dim=dim, ldim=dim(Data),
              bldim=c(bldim[1], dim[2]), CTXT=MYCTXT)
  if (ICTXT != MYCTXT)
    out <- reblock(out, bldim=bldim, ICTXT=ICTXT)
  
  if (newgrid)
    gridexit(3)
  
  return(out)
}



# 4 readers
# bldim = (1/4, ncol)
# scan(n= first 1/4, skip = 0)
# scan(n= 2nd 1/4, skip = 1/4)
# ...


# same as above but for csv
read.ddmatrix.csv <- function(file, sep=",", nrows, ncols, bldim=4, num.rdrs=1, ICTXT=0)
{
  if (length(bldim)==1)
    bldim <- rep(bldim, 2)
  
  msng <- FALSE
  if (missing(ncols)){
    msng <- TRUE
    if (comm.rank()==0)
      ncols <- length(scan(file=file, sep=sep, nlines=1L, quiet=T))
    else 
      ncols <- 0L
  }
  ncols <- pbdMPI::allreduce(ncols, op='sum')
  
  if (missing(nrows)){
    msng <- TRUE
#     size <- as.numeric(unlist(strsplit(x=system(paste("du -b", file), intern=T), split="\t"))[1])
    if (comm.rank()==0){
#      nrows <- as.numeric(unlist(strsplit(x=system(paste("wc -l", file), intern=T), split=" "))[1])
      seps <- ncols * (length(unlist(strsplit(sep, split="")))) - 1
      x <- length(unlist(strsplit(scan(file=file, sep=sep, nlines=1L, quiet=T, what='character'), split="")))
      nrows <- ceiling( file.info(file)[1] / (x+seps) )#(ncols + seps) ) # adjust for sep character
    } else {
      nrows <- 0L
    }
  }
  nrows <- pbdMPI::allreduce(nrows, op='sum')
  comm.print(nrows)
  dim <- c(nrows, ncols)
  
  nprocs <- comm.size()
  
  newgrid <- FALSE # flag for 'did we create a new grid' in ICTXT 3
  if (num.rdrs > nprocs){
    warning("Number of readers supplied is less than number requested; defaulting to", nprocs, "readers")
    num.rdrs <- nprocs
  }
  
  if (num.rdrs == nprocs){
    MYCTXT <- 2
  } else {
    assign(x=".__blacs_gridinfo_3", 
       value=.Fortran("mpi_blacs_initialize", 
              NPROW=as.integer(num.rdrs), NPCOL=as.integer(1), 
              ICTXT=as.integer(3), MYROW=as.integer(0), 
              MYCOL=as.integer(0) ),
       envir=.GlobalEnv )
    MYCTXT <- 3
    newgrid <- TRUE
  }
  
  blacs_ <- base.blacs(MYCTXT)
  
  # each process grabs its data
  nlines <- ceiling(dim[1] / num.rdrs)
  tmpbl <- c(nlines, ncols)
  if (blacs_$MYROW != -1){
    skip <- comm.rank() * nlines
    x <- scan(file=file, skip=skip, sep=sep, nlines=nlines, quiet=T)
  } else {
    x <- NULL
  }
  
  ldim <- length(x) / ncols
  
  dim[1] <- pbdMPI::allreduce(ldim, op='sum')
  
  if (ldim==0){
    ldim <- c(1,1)
    Data <- matrix(0)
  }
  else {
    ldim <- c(ldim, ncols)
    Data <- matrix(x, nrow=ldim[1], ncol=ldim[2], byrow=T)
  }
  
  out <- new("ddmatrix", Data=Data, dim=dim, ldim=ldim,
              bldim=tmpbl, CTXT=MYCTXT)
  
  if (ICTXT != MYCTXT)
    out <- base.redistribute(dx=out, bldim=bldim, ICTXT=ICTXT)
  
  if (newgrid)
    gridexit(3)
  
  if (msng)
    warning(paste("Failing to supply nrows= and ncols= will dramatically impact performance.\nFor future reference, this file is ", dim[1], "x", dim[2], sep=""))
  
  return(out)
}
