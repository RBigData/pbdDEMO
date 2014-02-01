library(pbdDEMO, quiet=T)
init.grid()

# set independent seeds using Rlecuyer
comm.set.seed(diff = TRUE)

# verify system solving at scale
verify.solve(nrows=1e3)

finalize()
