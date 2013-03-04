library(pbdMPI, quiet=T)
init()

comm.warning("this is a warning")

comm.stop("this is an error")

finalize()
