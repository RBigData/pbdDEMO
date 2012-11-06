### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Initial MPI.
library(pbdDEMO, quiet = TRUE)
init.grid()
if(comm.size() != 4){
  stop("This example requries 4 processors.")
}
comm.set.seed(1234, diff = TRUE)

### X.spmd can be readed from .csv files distributedly.
N.spmd <- 1 + comm.rank()
X.spmd <- matrix(rnorm(N.spmd * 3), ncol = 3)

### Run.
X.dmat <- spmd2dmat(X.spmd)
X <- as.matrix(X.dmat)
new.X.spmd <- dmat2spmd(X.dmat)

### Output.
if(comm.rank() == 1){
  cat("(local,part) new.X.spmd on rank = 1:\n")
  print(new.X.spmd)
}
if(comm.rank() == 2){
  cat("\n(global,all) X[4:6,] on all processors:\n")
  print(X[4:6,])
}
finalize()
