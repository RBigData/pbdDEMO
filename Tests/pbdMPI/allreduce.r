library(pbdMPI, quiet=T)
init()

comm.set.seed(diff=T)

n <- 1e3
x <- rnorm(n)

out <- allreduce(x)

comm.print(head(out))

finalize()
