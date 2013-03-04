library(pbdDMAT, quiet = TRUE)
library(rbenchmark)

###################SETTINGS######################

init.grid()

comm.set.seed(diff = TRUE)

# minimum and maximum order magnitude
mnom <- 1e2
mxom <- 1e5

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
sizes <- 10^(0:(log10(mxom/mnom))) * mnom

# serial first

for (size in 1:sizes){
  
  
}

barrier()

# parallel

dx <- ddmatrix("rnorm", nrow=N, ncol=p, bldim=bldim, mean=mean, sd=sd, ICTXT=0)

comm.print(datatimes)

times <- benchmark(crossprod(dx), columns=cols, replications=reps)
comm.print(times)

finalize()
