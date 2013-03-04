library(pbdDMAT, quiet=T)
 
init.grid()
 
.pbdBASEEnv$x <- TRUE
comm.print(.pbdBASEEnv$x, quiet=T)
 
assign("x", TRUE, envir=.pbdBASEEnv)
comm.print(get("x", envir=.pbdBASEEnv), quiet=T)
 
finalize()
