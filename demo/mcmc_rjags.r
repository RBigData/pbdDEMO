### Initial library
library(pbdMPI, quietly = TRUE)
COMM.SIZE <- comm.size()
COMM.RANK <- comm.rank()

### Set configurations. 
I.b <- 1000                                              # burn-in iteration
I.t <- 10                                                # thinning iteration
I.n <- 1000                                              # total samples

### Set data and parameters.
x <- MASS::galaxies                                      # obtain galaxies
n <- length(x)
mu.0 <- mean(x)                                          # prior mean of mu.x
sigma.0 <- sqrt(var(x) / n)                              # prior std of sigma.x
sigma.x <- sqrt(var(x))                                  # data std

# Load JAGS
options(warn = -1)
suppressMessages(library(rjags, quietly = TRUE))
options(warn = 0)
load.module("lecuyer", quiet = TRUE)

### JAGS model.
model <- "
model{
  ### likelihood
  for(i in 1:n){
    x[i] ~ dnorm(mu, tau.x)
  }
  tau.x <- 1 / sigma.x
  ### prior
  mu ~ dnorm(mu.0, tau.0)
  tau.0 <- 1 / sigma.0
}
"
fn.model <- tempfile()
cat(model, file = fn.model)

### JAGS's monitoring variables.
monitor.vars <- "mu"

### JAGS's data.
data <- list(x = x, n = n, mu.0 = mu.0, sigma.0 = sigma.0, sigma.x = sigma.x)

### Use task.pull.
FUN <- function(jid, n.job){
  ### JAGS's initial values.
  set.seed(123 + jid)   # seed for initial rnorm only.
  inits <- list(list(mu = rnorm(1, mean = mu.0),
                     .RNG.name = "lecuyer::RngStream",
                     .RNG.seed = 1234 + jid))    # seed for JAGS.

  ### Run rjags.
  m <- jags.model(fn.model, data, inits, n.chains = length(inits),
                  quiet = TRUE)
  update(m, n.iter = I.b, progress.bar = "none")
  I.n.sub <- ceiling(I.n / n.job) * I.t
  x.post <- coda.samples(m, monitor.vars, n.iter = I.n.sub, thin = I.t,
                         progress.bar = "none")
  x.post[[1]]
}

### Run tasks.
n.job <- COMM.SIZE
# ret.all <- pbdLapply(1:n.job, FUN, n.job)
ret.all <- task.pull(1:n.job, FUN, n.job)

### Summary.
if(COMM.RANK == 0){
  save(ret.all, file = "tmp.rda")
  class(ret.all) <- "mcmc.list"
  pdf("galaxy_rjags.pdf", width = 8, height = 4)
    plot(ret.all)
  dev.off()

}

### Finalize jobs.
finalize()

