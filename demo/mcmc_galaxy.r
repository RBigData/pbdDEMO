library(pbdMPI, quiet = TRUE)
x <- MASS::galaxies                                       # obtain galaxies
n <- length(x)

### data and parameters
I.b <- 1000                                               # burn-in iteration
I.t <- 10                                                 # thinning iteration
I.n <- 100                                                # total samples
I.c <- 1                                                  # number of chains
comm.set.seed(1234)                                       # set seed

mu.0 <- mean(x)                                           # prior mean
sigma.0 <- sqrt(var(x) / n)                               # prior std
sigma.x <- sqrt(var(x))                                   # data std

### a(theta.new, theta.org)
acceptance <- function(x, mu.new, mu.org, sigma = sigma.x){
  pi.org <- dnorm(mu.org, mean = mu.0, sd = sigma.0, log = TRUE)
  q.org <- sum(dnorm(x, mean = mu.org, sd = sigma, log = TRUE) )
  pi.new <- dnorm(mu.new, mean = mu.0, sd = sigma.0, log = TRUE)
  q.new <- sum(dnorm(x, mean = mu.new, sd = sigma, log = TRUE))
  print(c(pi.org, q.org, pi.new, q.new))
  min(1, exp(pi.new + q.org - pi.org - q.new))
} # End of acceptance

### Hastings-Metropolis MCMC
ret <- rnorm(1, mean = mu.0, sd = sigma.0)
for(i in 2:(I.b + ceiling((I.t * I.n) / I.c))){
  mu.org <- ret[i - 1]
  mu.new <- rnorm(1, mean = mu.0, sd = sigma.0)

  a <- acceptance(x, mu.new, mu.org) 
  U <- runif(1)
  if(U <= a){
    ret[i] <- mu.new
  } else{
    ret[i] <- mu.org
  }
}

finalize()
