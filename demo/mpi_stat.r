### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Initial MPI.
library(pbdDEMO, quiet = TRUE)
init()

### Generate balanced fake data.
comm.set.seed(1234)
N <- 100 * comm.size()    # Pretend N is large.
x <- rnorm(N)
id.get <- get.jid(N)
x.spmd <- x[id.get]       # Distributed data.

### Run.
ret.spmd <- demo.stat(x.spmd)

### Output.
comm.print(ret.spmd)
finalize()
