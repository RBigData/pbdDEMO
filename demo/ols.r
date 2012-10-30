### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Initial MPI.
library(pbdDEMO, quiet = TRUE)
init()

### Generate balanced fake data.
comm.set.seed(1234, diff = TRUE)
N <- 100                  # Pretend N is large.
p <- 2
### Distributed data.
X.spmd <- matrix(rnorm(N * p), ncol = p)
beta <- 1:p
epsilon <- rnorm(N)
y.spmd <- X.spmd %*% beta + epsilon 

### Run.
ret.spmd <- demo.ols(y.spmd, X.spmd)

### Output.
comm.print(ret.spmd)
finalize()
