library(pbdDMAT, quiet = TRUE)
library(rbenchmark)

###################SETTINGS######################
init.grid()

comm.set.seed(diff = TRUE)

# size
N <- 15000
p <- 500


# blocking
bldim <- 4

# normal family
mean <- 100
sd <- 1000

# rbenchmark
reps <- 10
cols <- c("test", "replications", "elapsed")

#################################################


# benchmark
datatimes <- system.time({
	dx <- ddmatrix("rnorm", nrow=N, ncol=p, bldim=bldim, mean=mean, sd=sd, ICTXT=0)
	dy <- ddmatrix("rnorm", nrow=N, ncol=1, bldim=bldim, mean=mean, sd=sd, ICTXT=0)
})[3]

datatimes <- allreduce(datatimes, op='max')

size <- N*p*8/1024
unit <- "kb"
if (log10(size) > 3){
	size <- size/1024
	unit <- "mb"
}
if (log10(size) > 3){
	size <- size/1024
	unit <- "gb"
}

comm.cat(sprintf("\n%.2f %s of data generated in %.3f seconds\n\n", size, unit, datatimes), quiet=T)


times <- benchmark(lm.fit(x=dx, y=dy), columns=cols, replications=reps)
times[3] <- allreduce(times[3], op='max')
times$mean <- times[3]/reps
names(times)[3] <- "total.runtime"
names(times$mean) <- "mean.runtime"
comm.print(times, quiet=T)

finalize()
