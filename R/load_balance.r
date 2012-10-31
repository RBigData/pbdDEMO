### This file contains functions to load balance of data X.spmd.

balance.info.spmd <- function(X.spmd){
  COMM.SIZE <- spmd.comm.size()
  COMM.RANK <- spmd.comm.rank()

  N.spmd <- nrow(X.spmd)
  N.allspmd <- spmd.allgather.integer(as.integer(N.spmd), integer(COMM.SIZE))
  N <- sum(N.allspmd)
  n <- ceiling(N / COMM.SIZE)

  rank.org <- rep(0:(COMM.SIZE - 1), N.allspmd)
  rank.belong <- rep(0:(COMM.SIZE - 1), each = n)[1:N]

  send.info <- data.frame(org = rank.org[rank.org == COMM.RANK],
                          belong = rank.belong[rank.org == COMM.RANK])
  recv.info <- data.frame(org = rank.org[rank.belong == COMM.RANK],
                          belong = rank.belong[rank.belong == COMM.RANK])

  list(send = send.info, recv = recv.info)
} # End of balance.info.spmd()

balance.info <- balance.info.spmd

load.balance.spmd <- function(X.spmd){
  COMM.RANK <- spmd.comm.rank()
  bal.info <- balance.info.spmd(X.spmd)
  p <- ncol(X.spmd)

  send.to <- as.integer(unique(bal.info$send$belong))
  if(length(send.to) > 0){
    for(i in send.to){
      if(i != COMM.RANK){
        tmp <- matrix(X.spmd[bal.info$send$belong == i,], ncol = p)
        isend(tmp, rank.dest = i, tag = COMM.RANK)
      }
    }
  }

  recv.from <- as.integer(unique(bal.info$recv$org))
  if(length(recv.from) > 0){
    ret <- NULL
    for(i in recv.from){
      if(i != COMM.RANK){
        tmp <- recv(rank.source = i, tag = i)
      } else{
        tmp <- matrix(X.spmd[bal.info$send$belong == i,], ncol = p)
      }
      ret <- rbind(ret, tmp)
    }
  } else{
    ret <- X.spmd
  }

  ret
} # End of load.balance.spmd().

load.balance <- load.balance.spmd
