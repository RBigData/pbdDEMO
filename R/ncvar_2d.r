### This file contains several functions for reading and writing spmd and
### ddmatrix to NetCDF4 files and mainly forcus on 2D matrices.

### Warning: All "vals" are presumed to be "spmd column-major" matrices
###          for all functions, but may be fine with other formats in
###          some functions.

demo.ncvar_put_2D <- function(nc, varid, vals, start = NA, count = NA,
    verbose = FALSE, comm = .SPMD.CT$comm){
  ### get variable id from the nc header
  idobj <- pbdNCDF4:::vobjtovarid4(nc, varid, verbose = verbose,
                                   allowdimvar = TRUE)

  ### obtain storage dimension
  ndim <- length(nc$var[[idobj$list_index]]$dim)

  ### check
  if(comm.any(is.na(start) || is.na(count), comm = comm)){
    COMM.RANK <- comm.rank(comm)
    nrow <- nrow(vals)
    ncol <- ncol(vals)
    count <- NULL
    start <- NULL
    if(ndim == 1){
      count <- nrow * ncol
      start <- c(1, cumsum(allgather(count, comm = comm)))[COMM.RANK + 1]
    }
    if(ndim == 2){
      count <- c(nrow, ncol)
      start <- c(1, c(1, cumsum(allgather(ncol, comm = comm)))[COMM.RANK + 1])
    }
  }
  if(comm.any(length(start) != ndim || length(count) != ndim, comm = comm)){
    stop("start and count should be specified correctly for a hypercube.")
  }
  if(comm.any(prod(count[count != 0]) != length(vals), comm = comm)){
    stop("dim(vals) and count are not consistent.")
  }

  ### parallel write
  nc_var_par_access(nc, varid, verbose = verbose)
  ncvar_put(nc, varid, vals, start = start, count = count, verbose = verbose)

  invisible()
} # End of demo.ncvar_put_2D().

ncvar_put_dmat <- function(nc, varid, vals, start = NA, count = NA,
    verbose = FALSE, comm = .SPMD.CT$comm){
  ### check
  if(! is.ddmatrix(vals)){
    stop("vals should be a ddmatrix.")
  }

  ### MPI information
  COMM.SIZE <- comm.size(comm)

  ### redistribute data in spmd column format.
  nrow <- nrow(vals)
  ncol <- ncol(vals)
  ncol.spmd <- ceiling(ncol / COMM.SIZE)
  X.dmat <- redistribution(vals, bldim = c(nrow, ncol.spmd), ICTXT = 1)

  demo.ncvar_put_2D(nc, varid, X.dmat@Data, start = start, count = count,
                    verbose = verbose, comm = comm)
} # End of ncvar_put_dmat().

ncvar_put_spmd <- function(nc, varid, vals, start = NA, count = NA,
    verbose = FALSE, comm = .SPMD.CT$comm){
  ### check
  if(! comm.all(is.matrix(vals), comm = comm)){
    stop("vals should be a spmd matrix")
  }

  demo.ncvar_put_2D(nc, varid, vals, start = start, count = count,
                    verbose = verbose, comm = comm)
} # End of ncvar_put_spmd().


### Modified from ncvar_get().
demo.ncvar_get_2D <- function(nc, varid, start = NA, count = NA,
    verbose = FALSE, signedbyte = TRUE, collapse_degen = TRUE,
    comm = .SPMD.CT$comm){
  ### get variable id from the nc header
  idobj <- pbdNCDF4:::vobjtovarid4(nc, varid, verbose = verbose,
                                   allowdimvar = TRUE)

  ### obtain storage dimension
  ndim <- length(nc$var[[idobj$list_index]]$dim)

  ### check
  if(comm.any(is.na(start) && is.na(count), comm = comm)){
    COMM.RANK <- comm.rank(comm)
    COMM.SIZE <- comm.size(comm)
    count <- NULL
    start <- NULL
    if(ndim == 1){
      ncol <- nc$var[[idobj$list_index]]$dim[[1]]$len
      ncol.per.rank <- ceiling(ncol / COMM.SIZE)
      start <- c(1 + ncol.per.rank * COMM.RANK)
      count <- c(ncol.per.rank)
      if(start + cont > ncol){
        count <- ncol - start + 1
      }
      if(start > ncol){
        start <- 1
        count <- 0
      }
    }
    if(ndim == 2){
      nrow <- nc$var[[idobj$list_index]]$dim[[1]]$len
      ncol <- nc$var[[idobj$list_index]]$dim[[2]]$len
      ncol.per.rank <- ceiling(ncol / COMM.SIZE)
      start <- c(1, 1 + nco.per.rank * COMM.RANK)
      count <- c(nrow, ncol.per.rank)
      if(start[2] + count[2] > ncol){
        count[2] <-  ncol - start[2] + 1
      }
      if(start[2] > ncol){
        start <- c(1, 1)
        count <- c(0, 0)
      }
    }
  }
  if(comm.any(length(start) != ndim || length(count) != ndim, comm = comm)){
    stop("start and count should be specified correctly for a hypercube.")
  }
  if(comm.any(prod(count[count != 0]) != length(vals), comm = comm)){
    stop("dim(vals) and count are not consistent.")
  }

  ### parallel read
  nc_var_par_access(nc, varid)
  vals <- ncvar_get(nc, varid, start = start, count = count,
                    verbose = verbose, signedbyte = signedbyte,
                    collapse_desgn = collapse_dsgn)
  vals
} # End of demo.ncvar_get_2D().

ncvar_get_dmat <- function(nc, varid, start = NA, count = NA,
    verbose = FALSE, signedbyte = TRUE, collapse_degen = TRUE,
    bldim = .DEMO.CT$bldim, ICTXT = .DEMO.CT$ictxt){
  vals <- demo.ncvar_get_2D(nc, varid, start = start, count = count,
                            verbose = verbose, signedbyte = signedbyte,
                            collapse_desgn = collapse_dsgn)
  
  ### block-cyclic in context 2.
  X.dmat <- new("ddmatrix", Data = vals,
                dim = c(N, p), ldim = ldim, bldim = bldim.org, CTXT = 2)


  ### redistribute data in ddmatrix format.
  redistribution(vals, bldim = bldim, ICTXT = ICTXT)
} # End of ncvar_get_dmat().

ncvar_get_spmd <- function(nc, varid, start = NA, count = NA,
    verbose = FALSE, signedbyte = TRUE, collapse_degen = TRUE,
    comm = .SPMD.CT$comm){
} # End of ncvar_get_spmd().

