library(pbdMPI, quiet=T)
init()

n <- 1L
out <- allgather(n)
comm.print(sum(unlist(out))==comm.size(), rank.print=comm.size()-1, quiet=T)

n <- comm.rank()
out <- gather(n)
comm.print(sum(unlist(out)) == comm.size()*(comm.size()-1)/2, quiet=T)

finalize()
