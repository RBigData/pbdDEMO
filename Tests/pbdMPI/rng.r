library(pbdMPI, quiet=T)
init()

comm.set.seed(diff=F)

x <- rnorm(1, mean=100, sd=1000)
x <- gather(x)
comm.print(sum(diff(unlist(x))) == 0, quiet=T)


comm.set.seed(diff=T)

x <- rnorm(1, mean=100, sd=1000)
x <- gather(x)
comm.print(sum(diff(unlist(x))) != 0, quiet=T)

finalize()
