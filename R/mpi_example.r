### These functions are selected from cookbook of HPSC website at
### "http://thirteen-01.stat.iastate.edu/snoweye/hpsc/?item=cookbook"

mpi.stat <- function(x.gbd){
  ### For mean(x).
  N <- allreduce(length(x.gbd), op = "sum")
  bar.x.gbd <- sum(x.gbd / N)
  bar.x <- allreduce(bar.x.gbd, op = "sum")

  ### For var(x).
  s.x.gbd <- sum(x.gbd^2 / (N - 1))
  s.x <- allreduce(s.x.gbd, op = "sum") - bar.x^2 * (N / (N - 1))

  list(mean = bar.x, s = s.x)
} # End of mpi.stat().

mpi.bin <- function(x.gbd, breaks = pi / 3 * (-3:3)){
  bin.gbd <- table(cut(x.gbd, breaks = breaks))
  bin <- as.array(allreduce(bin.gbd, op = "sum"))
  dimnames(bin) <- dimnames(bin.gbd)
  class(bin) <- class(bin.gbd)
  bin
} # End of mpi.bin().

mpi.quantile <- function(x.gbd, prob = 0.5){
  if(sum(prob < 0 | prob > 1) > 0){ 
    stop("prob should be in (0, 1)")
  }

  N <- allreduce(length(x.gbd), op = "sum")
  x.max <- allreduce(max(x.gbd), op = "max")
  x.min <- allreduce(min(x.gbd), op = "min")

  f.quantile <- function(x, prob = 0.5){
    allreduce(sum(x.gbd <= x), op = "sum") / N - prob
  }

  uniroot(f.quantile, c(x.min, x.max), prob = prob[1])$root
} # End of mpi.quantile().

mpi.ols <- function(y.gbd, X.gbd){
  if(length(y.gbd) != nrow(X.gbd)){
    stop("length(y.gbd) != nrow(X.gbd)")
  }
  
  t.X.gbd <- t(X.gbd)
  A <- allreduce(t.X.gbd %*% X.gbd, op = "sum")
  B <- allreduce(t.X.gbd %*% y.gbd, op = "sum")

  solve(matrix(A, ncol = ncol(X.gbd))) %*% B
} # End of mpi.ols().

