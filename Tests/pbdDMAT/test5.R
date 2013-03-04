# ################################################
# ------------------------------------------------
# Stats
# ------------------------------------------------
# ################################################

# For each test, script returns TRUE if the test was successful 
# (produced the correct value), and returns FALSE if the test was
# unsuccessful (produced the incorrect value).


suppressPackageStartupMessages(library(pbdDMAT, quiet=T))

init.grid()

comm.print <- function(x) pbdMPI::comm.print(x, quiet=T)

M <- 250
N <- 250
#M <- 6
#N <- 6
BL <- 4
#BL <- 2

seed <- sample(1:1000, size=1)
seed <- allreduce(seed, op='sum')
set.seed(seed)

tol <- 1e-8

# ---------------------------------------------------
# Tests
# ---------------------------------------------------

tests <- function(.)
{
  out1 <- scale(A)
  out2 <- as.matrix( scale(dA) )
  comm.print(all.equal(out1, out2))
  
#  comm.print(out1)
#  comm.print(out2)
  
  out1 <- prcomp(A)$sdev
  out2 <- prcomp(dA)$sdev
  comm.print(all.equal(out1, out2))
  
  out1 <- cov(A)
  out2 <- as.matrix(cov(dA))
  comm.print(all.equal(out1, out2))
}

# ---------------------------------------------------
# PBLAS
# ---------------------------------------------------

comm.print("-------STATS-------")
comm.print("       Square")

A <- matrix(rnorm(M*N, 10, 100), M, N)
dA <- as.ddmatrix(A, BL)
tests()

comm.print("       Column")

A <- matrix(rnorm(M*1, 10, 100), M, 1)
dA <- as.ddmatrix(A, BL)
tests()

comm.print("       Row")

A <- matrix(rnorm(1*N, 10, 100), 1, N)
dA <- as.ddmatrix(A, BL)
tests()



#comm.print(out1)
#comm.print(out2)

finalize()
