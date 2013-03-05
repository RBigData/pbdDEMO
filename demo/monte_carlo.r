### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Setup environment.
library(pbdDEMO, quiet = TRUE)
comm.set.seed(1234, diff = TRUE)

### Run
N.spmd <- 1000
X.spmd <- matrix(runif(N.spmd * 2), ncol = 2)
r.spmd <- sum(sqrt(rowSums(X.spmd^2)) <= 1)
ret <- allreduce(c(N.spmd, r.spmd), op = "sum")
PI <- 4 * ret[2] / ret[1]
comm.print(PI)

### Quit.
finalize()
