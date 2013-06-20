library(pbdDEMO, quiet = TRUE)

FUN <- funtion(jid){
  jid * 10
}

ret <- task.pull(1:10, FUN)

comm.print(ret, all.rank = TRUE)

finalize()
