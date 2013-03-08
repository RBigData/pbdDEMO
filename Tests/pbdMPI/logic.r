library(pbdMPI, quiet=T)
init()

comm.set.seed(diff=T)

x <- comm.rank()

l1 <- comm.any(x)
comm.print(l1, quiet=T)
l2 <- comm.all(x)
comm.print(l2 == FALSE, quiet=T)

finalize()
