#' Read and Write Parallel NetCDF4 Files in GBD and ddmatrix Format
#' 
#' These functions write and read NetCDF4 files in GBD and ddmatrix format.
#' 
#' \code{demo.ncvar_get_*} are similar to \code{ncvar_get} of \pkg{pbdNCDF4},
#' but focus on 2D arrays and return a ddmatrix or GBD matrix.
#' 
#' \code{demo.ncvar_put_*} are also similar to \code{ncvar_put} of
#' \pkg{pbdNCDF4}, but only dump 2D arrays.
#' 
#' @param nc 
#' an object of class \code{ncdf4} (as returned by either function
#' \code{nc_open_par} or function \code{nc_create_par}), indicating what file
#' to read from.
#' @param varid 
#' See \code{ncvar_get} for details.
#' @param verbose 
#' See \code{ncvar_get} for details.
#' @param signedbyte 
#' See \code{ncvar_get} for details.
#' @param collapse_degen 
#' See \code{ncvar_get} for details.
#' @param vals 
#' See \code{ncvar_put} for details.
#' @param gbd.major 
#' a GBD major, either 1 for row-major or 2 for column-major.
#' @param bldim 
#' the blocking dimension for block-cyclically distributing the
#' matrix across the process grid.
#' @param ICTXT 
#' BLACS context number for return.
#' @param comm 
#' a communicator number.
#' 
#' @return 
#' \code{demo.ncvar_get_dmat} returns a \code{ddmatrix}, and
#' \code{demo.ncvar_get_gbd} returns a GBD matrix in either row- or column
#' major specified by \code{gbd.major}.
#' 
#' @examples
#' \dontrun{
#' ### Under command mode, run the demo with 4 processors by
#' ### (Use Rscript.exe for windows system)
#' mpiexec -np 4 Rscript -e "demo(nc4_serial,'pbdDEMO',ask=F,echo=F)"
#' mpiexec -np 4 Rscript -e "demo(nc4_parallel,'pbdDEMO',ask=F,echo=F)"
#' mpiexec -np 4 Rscript -e "demo(nc4_dmat,'pbdDEMO',ask=F,echo=F)"
#' mpiexec -np 4 Rscript -e "demo(nc4_gbdc,'pbdDEMO',ask=F,echo=F)"
#' }
#' 
#' @seealso \code{.DEMO.CT}.
#' @keywords programming
#' @name ncvar
#' @rdname ncvar
NULL



demo.ncvar_ndim <- function(nc, varid, verbose = FALSE){
  ### get variable id from the nc header
  idobj <- pbdNCDF4::vobjtovarid4(nc, varid, verbose = verbose,
                                  allowdimvar = TRUE)

  ### obtain storage dimension
  length(nc$var[[idobj$list_index]]$dim)
} # End of demo.ncvar_ndim().



### put methods.
demo.ncvar_put_2D <- function(nc, varid, vals, start = NA, count = NA,
    verbose = FALSE, comm = .SPMD.CT$comm){
  ### check
  ndim <- demo.ncvar_ndim(nc, varid)
  if(comm.any(is.na(start) || is.na(count), comm = comm)){
    COMM.RANK <- comm.rank(comm)
    nrow <- nrow(vals)
    ncol <- ncol(vals)
    count <- NULL
    start <- NULL
    if(ndim == 1){
      count <- nrow * ncol
      cumsum.count <- 1 + cumsum(allgather(count, comm = comm))
      start <- c(1, cumsum.count)[COMM.RANK + 1]
      if(count == 0 && start == max(cumsum.count)){
        start <- max(cumsum.count) - 1
      }
    }
    if(ndim == 2){
      count <- c(nrow, ncol)
      cumsum.ncol <- 1 + cumsum(allgather(ncol, comm = comm))
      start <- c(1, cumsum.ncol)[COMM.RANK + 1]
      if(any(count == 0) && start == max(cumsum.ncol)){
        start <- max(cumsum.ncol) - 1
      }
      start <- c(1, start)
    }
  }
  if(comm.any(length(start) != ndim || length(count) != ndim, comm = comm)){
    comm.stop("start and count should be specified for hypercube variables.")
  }
  tl.flag <- (length(vals) == 0 && all(count != 0)) ||
             (length(vals) != 0 && length(vals) != prod(count))
  if(comm.any(tl.flag, comm = comm)){
    comm.stop("dim(vals) and count are not consistent.")
  }

  ### parallel write
  pbdNCDF4::nc_var_par_access(nc, varid, verbose = verbose)
  pbdNCDF4::ncvar_put(nc, varid, as.vector(vals),
                       start = start, count = count, verbose = verbose)

  invisible()
} # End of demo.ncvar_put_2D().



#' @rdname ncvar
#' @export
demo.ncvar_put_dmat <- function(nc, varid, vals, verbose = FALSE,
    comm = .SPMD.CT$comm){
  ### check
  ndim <- demo.ncvar_ndim(nc, varid)
  if(ndim > 2){
    comm.stop("Hypercube variables are not supported.")
  }
  if(!is.ddmatrix(vals)){
    comm.stop("vals should be a ddmatrix.")
  }

  ### MPI information
  COMM.SIZE <- comm.size(comm)

  ### redistribute data in gbd column format.
  bldim <- c(nrow(vals), ceiling(ncol(vals) / COMM.SIZE))
  X.dmat <- dmat.reblock(vals, bldim = bldim, ICTXT = 1)
  if(base.ownany(dim(X.dmat), bldim(X.dmat), ICTXT = 1)){
    vals <- X.dmat@Data
  } else{
    vals <- matrix(0, nrow = 0, ncol = 0)
  }

  demo.ncvar_put_2D(nc, varid, vals, verbose = verbose, comm = comm)
} # End of demo.ncvar_put_dmat().



#' @rdname ncvar
#' @export
demo.ncvar_put_gbd <- function(nc, varid, vals, verbose = FALSE,
    comm = .SPMD.CT$comm, gbd.major = .DEMO.CT$gbd.major){
  ### check
  ndim <- demo.ncvar_ndim(nc, varid)
  if(ndim > 2){
    comm.stop("Hypercube variables are not supported.")
  }
  if(!comm.all(is.matrix(vals), comm = comm)){
    comm.stop("vals should be a gbd matrix")
  }

  if(gbd.major == 1){
    vals <- demo.gbdr2dmat(vals, comm = comm)
    demo.ncvar_put_dmat(nc, varid, vals, verbose = verbose, comm = comm)
  } else{
    demo.ncvar_put_2D(nc, varid, vals, verbose = verbose, comm = comm)
  }
} # End of demo.ncvar_put_gbd().


### get methods modified from ncvar_get().
demo.ncvar_get_2D <- function(nc, varid, start = NA, count = NA,
    verbose = FALSE, signedbyte = TRUE, collapse_degen = TRUE,
    comm = .SPMD.CT$comm){
  ### get variable id from the nc header
  idobj <- pbdNCDF4::vobjtovarid4(nc, varid, verbose = verbose,
                                  allowdimvar = TRUE)

  ### obtain storage dimension
  ndim <- length(nc$var[[idobj$list_index]]$dim)

  ### check
  if(comm.any(is.na(start) || is.na(count), comm = comm)){
    COMM.RANK <- comm.rank(comm)
    COMM.SIZE <- comm.size(comm)
    start <- NULL
    count <- NULL
    if(ndim == 1){
      ncol <- nc$var[[idobj$list_index]]$dim[[1]]$len
      ncol.per.rank <- ceiling(ncol / COMM.SIZE)
      start <- c(1 + ncol.per.rank * COMM.RANK)
      count <- c(ncol.per.rank)
      if(start + count > ncol){
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
      start <- c(1, 1 + ncol.per.rank * COMM.RANK)
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
    comm.stop("start and count should be specified correctly for a hypercube.")
  }

  ### parallel read
  pbdNCDF4::nc_var_par_access(nc, varid)
  vals <- try(pbdNCDF4::ncvar_get(nc, varid, start = start, count = count,
                                   verbose = verbose, signedbyte = signedbyte,
                                   collapse_degen = collapse_degen),
              silent = TRUE)
  if(class(vals) == "try-error"){
    if(ndim == 1){
      vals <- vector(0, length = 0)
    } else if(ndim == 2){
      vals <- matrix(0, nrow = 0, ncol = 0)
    } else{
      vals <- array(0, rep(0, ndim))
    }
  }
  vals
} # End of demo.ncvar_get_2D().



#' @rdname ncvar
#' @export
demo.ncvar_get_dmat <- function(nc, varid,
    verbose = FALSE, signedbyte = TRUE, collapse_degen = TRUE,
    bldim = .DEMO.CT$bldim, ICTXT = .DEMO.CT$ictxt, comm = .SPMD.CT$comm){
  ### check
  ndim <- demo.ncvar_ndim(nc, varid)
  if(ndim > 2){
    comm.stop("Hypercube variables are not supported.")
  }

  ### get data out of file.
  vals <- demo.ncvar_get_2D(nc, varid,
                            verbose = verbose, signedbyte = signedbyte,
                            collapse_degen = collapse_degen)
  
  ### block-cyclic in context 1.
  if(ndim == 1){
    ldim <- as.integer(c(length(vals), 1))
    dim <- c(ldim[1], 1)
    bldim.org <- c(spmd.allreduce.integer(ldim[1], integer(1), op = "max",
                                          comm = comm),
                   1)
  } else{
    ldim <- as.integer(dim(vals))
    dim <- c(ldim[1], spmd.allreduce.integer(ldim[2], integer(1), op = "sum",
                                             comm = comm))
    bldim.org <- c(ldim[1],
                   spmd.allreduce.integer(ldim[2], integer(1), op = "max",
                                          comm = comm))
  }
  X.dmat <- new("ddmatrix", Data = vals,
                dim = dim, ldim = ldim, bldim = bldim.org, ICTXT = 1)

  ### redistribute data in ddmatrix format.
  dmat.reblock(X.dmat, bldim = bldim, ICTXT = ICTXT)
} # End of demo.ncvar_get_dmat().



#' @export
#' @rdname ncvar
demo.ncvar_get_gbd <- function(nc, varid,
    verbose = FALSE, signedbyte = TRUE, collapse_degen = TRUE,
    comm = .SPMD.CT$comm, gbd.major = .DEMO.CT$gbd.major){
  ### check
  ndim <- demo.ncvar_ndim(nc, varid)
  if(ndim > 2){
    comm.stop("Hypercube variables are not supported.")
  }

  ### get data out of file.
  if(gbd.major == 1){
    vals <- demo.ncvar_get_dmat(nc, varid, verbose = verbose,
                                signedbyte = signedbyte,
                                collapse_degen = collapse_degen, comm = comm)
    vals <- demo.dmat2gbdr(vals, comm = comm)
  } else{
    vals <- demo.ncvar_get_2D(nc, varid,
                              verbose = verbose, signedbyte = signedbyte,
                              collapse_degen = collapse_degen)
  }

  vals
} # End of demo.ncvar_get_gbd().

