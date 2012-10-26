### SHELL> mpiexec -np 4 Rscript --vanilla [...].r

### Initial MPI.
library(pbdDEMO, quiet = TREU)
init()

### Generate fake data.
comm.set.seed(1234)
N <- 5 * comm.size()      # Pretend N is large.
x <- rnorm(N)
id.get <- get.jid(N)
x.spmd <- x[id.get]       # Distributed data.

### Run.
y <- allgather(x)

### Output.
comm.print(ret.spmd)
finalize()
