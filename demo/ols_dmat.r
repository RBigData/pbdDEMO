library(pbdDEMO, quiet=TRUE)

init.grid()

n <- 1250
p <- 40

mean <- 100
sd <- 1000

ymin <- 0
ymax <- 500

bldim <- c(2,2)

comm.set.seed(1234, diff=TRUE)

dx <- Hnorm(c(n, p), bldim=bldim, mean=mean, sd=sd)
dy <- Hunif(c(n, 1), bldim=bldim, min=ymin, max=ymax)

mdl <- lm.fit(dx, dy)

dx.new <- Hunif(c(1, p), bldim=bldim, min=ymin, max=ymax)
pred <- dx.new %*% mdl$coefficients

comm.cat(paste("\nThe predicted y value is:", submatrix(pred), "\n"), quiet=T)

finalize()
