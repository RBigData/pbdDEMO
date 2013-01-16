### This file contains several functions for reading and writing
### ddmatrix to NetCDF4 files

ncvar_put_2D <- function(nc, varid, vals, slice.id = NULL, verbose = FALSE,
    comm = .SPMD.CT$comm){
  ### check
  if((! comm.all(is.matrix(vals))) && (! is.ddmatrix(vals))){
    stop("vals should be a matrix or a ddmatrix.")
  }

  ### get variable id from the nc header
  idobj <- pbdNCDF4:::vobjtovarid4(nc, varid, verbose = verbose,
                                   allowdimvar = TRUE)

  ### obtain matrix size
  ndim <- length(nc$var[[idobj$list_index]]$dim)
  if(ndim > 2){
    warning("Only the first two dimensions are written")
  }

  ### MPI information
  rank <- comm.rank(comm)
  size <- comm.size(comm)

  ### divide data into pieces by rank
  nrow <- nrow(vals)
  ncol <- ncol(vals)
  ncol.per.rank <- ceiling(ncol / size)
  st <- c(1, 1 + ncol.per.rank * rank)
  co <- c(nrow, ncol.per.rank)

  ### prepare the matrix to write
  if(is.ddmatrix(vals)){
    ### redistribute data in spmd column format.
    X.dmat <- redistribution(vals, bldim = c(nrow, ncol.per.rank), ICTXT = 1)
    X.spmd <- X.dmat@Data
  } else{
    X.spmd <- vals
  }

  ### take care process overflows (size > ncol)
  if(st[2] + co[2] > ncol){
    if(st[2] <= ncol){  # fill the last piece
      co <- c(nrow, ncol - st[2] + 1)
    } else{             # empty matrix (rank > ncol)
      st <- c(1, 1)
      co <- c(0, 0)
    }
  }
  if(ndim > 2){         # fill the slice of hypercube
    if(comm.any(is.null(slice.id), comm = comm)){
      slice.id <- c(0, 0, rep(1, ndim - 2))
    } else{
      if(comm.any(length(slice.id) <= 2, comm = comm)){
        sto("length slice.id should be greater than 2.")
      }
      if(comm.any(sum(slice.id == 0) != 2, comm = comm)){
        sto("slice.id should have exactly two 0 elements.")
      }
    }
    st.new <- slice.id
    st.new[slice.id == 0] <- st
    st <- st.new 
    co.new <- rep(0, ndim)
    co.new[slice.id == 0] <- co
    co <- co.new
  }
  if(co[2] != 0){
    X.spmd.by.rank <- as.vector(X.spmd[, st[2] - 1 + (1:co[2])])
  } else{
    X.spmd.by.rank <- NULL
  }

  ### parallel write
  nc_var_par_access(nc, varid, verbose = verbose)
  ncvar_put(nc, varid, X.spmd.by.rank, start = st, count = co,
            verbose = verbose)

  invisible()
} # End of ncvar_put_2D().


ncvar_put_dmat <- ncvar_put_2D


### Modified from ncvar_get().
ncvar_get_2D <- function(nc, varid, slice.id = NULL){
  ### check
  if(class(nc) != "ncdf4"){
    stop("first argument (nc) is not of class ncdf4!")
  }

  if((mode(varid) != 'character') && (class(varid) != 'ncvar4') &&
     (class(varid) != 'ncdim4') && (! is.na(varid))){
    stop(paste("Error: second argument to ncvar_get must be an object of type ncvar or ncdim",
               "(both parts of the ncdf object returned by nc_open()), the character-string name of a variable or dimension",
               "or NA to get the default variable from the file.  If the file is netcdf version 4",
               "format and uses groups, then the fully qualified var name must be given, for",
               "example, model1/run5/Temperature"))
  }

  ### get variable id from the nc header
  idobj <- pbdNCDF4:::vobjtovarid4(nc, varid, verbose = verbose,
                                   allowdimvar = TRUE)

  ### MPI information
  rank <- comm.rank()
  size <- comm.size()

  ### obtain matrix size
  ndim <- length(nc$var[[idobj$list_index]]$dim)
  if(ndim > 2){
    warning("Only the first two dimensions are read.")
  }

  ### divide data into pieces by rank
  nrow <- nc$var[[idobj$list_index]]$dim[[1]]$len
  ncol <- nc$var[[idobj$list_index]]$dim[[2]]$len
  ncol.per.rank <- ceiling(ncol / size)
  st <- c(1, 1 + ncol.per.rank * rank)
  co <- c(nrow, ncol.per.rank)

  ### parallel read
  nc_var_par_access(nc, varid)
  X.spmd <- ncvar_get(nc, varid, x.by.rank, start = st, count = co)

  X.spmd
} # End of ncvar_get_2D().


ncvar_get_dmat <- function(nc, varid, slice.id = NULL, bldim = .DEMO.CT$bldim,
    ICTXT = .DEMO.CT$ictxt){
  X.spmd <- ncvar_get_2D(nc, varid, slice.id = slice.id)
  
  ### redistribute data in ddmatrix format.
  X.dmat <- redistribution(X.spmd, bldim = bldim, ICTXT = ICTXT)

  X.dmat
} # End of ncvar_get_dmat().

