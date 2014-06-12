# File name: dist_iris.r
# Run: mpiexec -np 4 Rscript dist_iris.r

rm(list = ls())                                       # Clean environment
library(pbdMPI, quietly = TRUE)                       # Load library
if(comm.size() != 4)
  comm.stop("4 processors are required.")

### Load data
X <- as.matrix(iris[, -5])                            # Dimension 150 by 4
X.cid <- as.numeric(iris[, 5])                        # True id

### Distribute data
jid <- get.jid(nrow(X))
X.gbd <- X[jid,]                                      # GBD row-major format

### Standardized
N <- allreduce(nrow(X.gbd))                           # 150
p <- ncol(X.gbd)                                      # 4

### Finish
finalize()
