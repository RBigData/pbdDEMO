### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Setup environment.
library(pbdDEMO, quiet = TRUE)
if(comm.size() != 4){
  stop("This example requries 4 processors.")
}
comm.set.seed(1234, diff = TRUE)

### X.spmd can be readed from .csv files distributedly.
N.spmd <- 1 + comm.rank()
X.spmd <- matrix(rnorm(N.spmd * 3), ncol = 3)
comm.cat("X.spmd on rank 2:\n", quiet = TRUE)
comm.print(X.spmd, rank.print = 2, quiet = TRUE)

### Run
bal.info <- balance.info(X.spmd)
new.X.spmd <- load.balance(X.spmd, bal.info)
org.X.spmd <- unload.balance(new.X.spmd, bal.info)

comm.cat("\nnew.X.spmd on rank 2:\n", quiet = TRUE)
comm.print(new.X.spmd, rank.print = 2, quiet = TRUE)

comm.cat("\nbal.info on rank 2:\n", quiet = TRUE)
comm.print(bal.info, rank.print = 2, quiet = TRUE)

if(any(org.X.spmd - X.spmd != 0)){
  cat("Unbalance fails in the rank ", comm.rank(), "\n")
}

### Quit.
finalize()
