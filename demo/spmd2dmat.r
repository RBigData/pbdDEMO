### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Initial MPI.
library(pbdDEMO, quiet = TRUE)
init.grid()

### Generate unbalance fake data.
comm.set.seed(1234, diff = TRUE)
N <- 10 + 2 * comm.rank()
p <- 4
X.spmd <- matrix(rnorm(N * p), ncol = p)

### Run.
X.dmat <- spmd2dmat(X.spmd)
X <- as.matrix(X.dmat)

### Output.
comm.cat("(local,part) X.spmd[1:4,]\n", quiet = TRUE)
comm.print(X.spmd[1:4,], quiet = TRUE)
comm.cat("(local,all) X[1:4,]\n", quiet = TRUE)
comm.print(X[1:4,], quiet = TRUE)
comm.cat("(local,part) dim(X.spmd)\n", quiet = TRUE)
comm.print(dim(X.spmd), quiet = TRUE)
comm.cat("(block-cyclic) dim(X.dmat@Data)\n", quiet = TRUE)
comm.print(dim(X.dmat@Data), quiet = TRUE)
finalize()
