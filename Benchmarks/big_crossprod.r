library(pbdDMAT, quiet = TRUE)
library(rbenchmark)

###################SETTINGS######################

init.grid()

comm.set.seed(diff = TRUE)

# size ~ 30gb
N <- 750000
p <- 5200

# blocking
bldim <- 2

# normal family
mean <- 100
sd <- 1000

# rbenchmark
reps <- 25
cols <- c("test", "replications", "elapsed")

#################################################


# benchmark
datatimes <- system.time(dx <- ddmatrix("rnorm", nrow=N, ncol=p, bldim=bldim, mean=mean, sd=sd, ICTXT=0))[3]

comm.print(datatimes)

times <- benchmark(crossprod(dx), cov(dx), columns=cols, replications=reps)
comm.print(times)

finalize()
