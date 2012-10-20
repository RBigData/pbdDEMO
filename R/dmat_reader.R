# name of the function is terrible; FIXME
# Assumes the database is 'final', meaning that the data is stored
  # effectively like a csv.  Meaning, the data looks in the db
  # like it should look in the matrix.  Also assumes all 
  # columns are numeric since this is a distributed MATRIX, not
  # a distributed dataframe.
read.ddmatrix.sql <- function(dbname, table, bldim=.BLDIM, num.rdrs=1, ICTXT=0)
{
  newgrid <- FALSE # flag for 'did we create a new grid' in ICTXT 3
  
  ### WCC: we should not do require/library inside a package.
  # require(sqldf, quietly=TRUE)
  
  # Determine blacs grid information
  nprocs <- comm.size()
  
  
  if (num.rdrs > nprocs){
    warning("Numer of readers supplied is less than number requested; defaulting to", nprocs, "readers")
    num.rdrs <- nprocs
  } 
  else if (num.rdrs == nprocs){
    MYCTXT <- 2
  }
  else {
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
