# ---------------------------------------------
# SVD
# ---------------------------------------------

verify.svd <- function(nrows=1e3, ncols=1e3, mean=0, sd=1, bldim=8, tol=1e-7, ICTXT=.DEMO.CT$ictxt)
{
  # generating data
  comm.cat(paste("Generating a ", nrows, "x", ncols, " distributed matrix of random normal data, performing the SVD, and then multiplying the factorization back together and comparing it to the original matrix\n", sep=""), quiet=T)
  
  time_data <- timer({
    x <- ddmatrix("rnorm", nrow=nrows, ncol=ncols, bldim=bldim, mean=mean, sd=sd, ICTXT=ICTXT)
  })
  
  time_svd <- timer({
    svd_x <- La.svd(x)
  })
  
  newd <- ddmatrix(0.0, nrow=nrows, ncol=ncols, bldim=bldim, ICTXT=ICTXT)
  for (i in 1:length(svd_x$d)){
    newd[i, i] <- svd_x$d[i]
  }
  
  
  time_verif <- timer({
    newx <- svd_x$u %*% newd %*% svd_x$vt
  
    iseq <- all.equal(x, newx, tol=tol)
    iseq <- as.logical(allreduce(iseq, op='min'))
  })
  
  comm.cat("\nIs the factorization correct?  ", quiet=T)
  if (iseq)
    comm.cat("YES!\n", quiet=T)
  else {
    comm.cat("No...\n", quiet=T)
    s <- x-newx
    diffs <- c(min(s), mean(s), max(s))
    names(diffs) <- c("min", "mean", "max")
    comm.cat("\nPrinting min/mean/max differences between original and result from factoring and then multiplying...\n", quiet=T)
    comm.print(diffs, quiet=T)
  }
  
  
  comm.cat("\nRun times:\n", quiet=T)

  comm.print( 
         rbind(
            DataGeneration=time_data, 
            SVD=time_svd, 
            Verification=time_verif), 
             quiet=T 
         )
  
  tot <- time_data+time_svd+time_verif
  names(tot) <- c("", "", "")
  
  comm.print(rbind("Total         "=tot), quiet=T)
}

# ---------------------------------------------
# Cholesky
# ---------------------------------------------

verify.chol <- function(nrows=1e3, mean=0, sd=1, bldim=8, tol=1e-7, ICTXT=.DEMO.CT$ictxt)
{
  # generating data
  comm.cat(paste("Generating a ", nrows, "x", nrows, " distributed matrix X of random normal data, 'symmetrizing' it by computing X <- t(X)%*%X, computing the Cholesky factorization, and then multiplying the factorization back together and comparing it to the original matrix\n", sep=""), quiet=T)
  
  time_data <- timer({
    x <- ddmatrix("rnorm", nrow=nrows, ncol=nrows, bldim=bldim, mean=mean, sd=sd, ICTXT=ICTXT)
  })
  
  # symmetrize x
  time_sym <- timer({
    x <- t(x) %*% x
  })
  
  time_chol <- timer({
    chol_x <- chol(x)
  })
  
  time_verif <- timer({
    tchol_x <- t(chol_x)
    newx <- tchol_x %*% chol_x
    
    iseq <- all.equal(x, newx, tol=tol)
    iseq <- as.logical(allreduce(iseq, op='min'))
  })
  
  comm.cat("\nIs the factorization correct?  ", quiet=T)
  if (iseq)
    comm.cat("YES!\n", quiet=T)
  else {
    comm.cat("No...\n", quiet=T)
    s <- x-newx
    diffs <- c(min(s), mean(s), max(s))
    names(diffs) <- c("min", "mean", "max")
    comm.cat("\nPrinting min/mean/max differences between original and result from factoring and then multiplying...\n", quiet=T)
    comm.print(diffs, quiet=T)
  }
  
  comm.cat("\n\nRun times:\n", quiet=T)
  
  comm.print( 
         rbind(
            DataGeneration=time_data, 
            Symmetrizing=time_sym,
            CholFactorize=time_chol, 
            Verification=time_verif),
             quiet=T 
         )
  
  tot <- time_data+time_sym+time_chol+time_verif
  names(tot) <- c("", "", "")
  
  comm.print(rbind("Total         "=tot), quiet=T)
}

# ---------------------------------------------
# Inverse
# ---------------------------------------------

verify.inverse <- function(nrows=1e3, mean=0, sd=1, bldim=8, tol=1e-7, ICTXT=.DEMO.CT$ictxt)
{
  # generating data
  comm.cat(paste("Generating a ", nrows, "x", nrows, " distributed matrix X of random normal data, inverting it, and then multiplying the inverse against the original matrix and verifying that the identity matrix is produced\n", sep=""), quiet=T)
  
  time_data <- timer({
    x <- ddmatrix("rnorm", nrow=nrows, ncol=nrows, bldim=bldim, mean=mean, sd=sd, ICTXT=ICTXT)
  })
  
  # 
  time_inv <- timer({
    inv_x <- solve(x)
  })
  
  time_verif <- timer({
    id <- x %*% inv_x
    dg <- diag(id)
    
    for (i in 1:nrows){
      id[i, i] <- dg[i] - 1
    }
    
    iseq <- (id < tol)
    iseq <- min(iseq@Data)
    iseq <- as.logical(allreduce(iseq, op='min'))
  })
  
  comm.cat("\nIs the factorization correct?  ", quiet=T)
  if (iseq)
    comm.cat("YES!\n", quiet=T)
  else {
    comm.cat("No...\n", quiet=T)
  }
  
  comm.cat("\n\nRun times:\n", quiet=T)
  
  comm.print( 
         rbind(
            DataGeneration=time_data, 
            Invert=time_inv, 
            Verification=time_verif),
             quiet=T 
         )
  
  tot <- time_data+time_inv+time_verif
  names(tot) <- c("", "", "")
  
  comm.print(rbind("Total         "=tot), quiet=T)
}

# ---------------------------------------------
# Solving a system
# ---------------------------------------------

verify.solve <- function(nrows=1e3, mean=0, sd=1, const=1, bldim=8, tol=1e-7, ICTXT=.DEMO.CT$ictxt)
{
  # generating data
  comm.cat(paste("Generating a ", nrows, "x", nrows, " distributed matrix X of random normal data and a 'true' solution as a vector of 1's. Then the vector of right hand sides is produced by projecting the system onto the true solution, and then finally the system is solved. The numerically determined solution is compared against the vector of 1's.\n", sep=""), quiet=T)
  
  time_data <- timer({
    x <- ddmatrix("rnorm", nrow=nrows, ncol=nrows, bldim=bldim, mean=mean, sd=sd, ICTXT=ICTXT)
    truesol <- ddmatrix(const, nrow=nrows, ncol=1, bldim=bldim, const=const, ICTXT=ICTXT)
  })
  
  time_rhs <- timer({
    rhs <- x %*% truesol
  })
  
  # solving
  time_sol <- timer({
    sol <- solve(x, rhs)
  })
  
  # verifying
  time_verif <- timer({
    iseq <- all.equal(sol, truesol, tol=tol)
    iseq <- as.logical(allreduce(iseq, op='min'))
  })
  
  comm.cat("\nIs the factorization correct?  ", quiet=T)
  if (iseq)
    comm.cat("YES!\n", quiet=T)
  else {
    comm.cat("No...\n", quiet=T)
    s <- sol-truesol
    diffs <- c(min(s), mean(s), max(s))
    names(diffs) <- c("min", "mean", "max")
    comm.cat("\nPrinting min/mean/max differences between numerical solution estimate and the true solution...\n", quiet=T)
    comm.print(diffs, quiet=T)
  }
  
  comm.cat("\n\nRun times:\n", quiet=T)
  
  comm.print( 
         rbind(
            DataGeneration=time_data, 
            RHSgeneration=time_rhs,
            Solving=time_sol, 
            Verification=time_verif),
          quiet=T)
  
  tot <- time_data+time_rhs+time_sol+time_verif
  names(tot) <- c("", "", "")
  
  comm.print(rbind("Total         "=tot), quiet=T)
}
