### This file contains functions to load balance of data X.spmd.

balance.info <- function(X.spmd, comm = .SPMD.CT$comm){
  COMM.SIZE <- spmd.comm.size(comm)
  COMM.RANK <- spmd.comm.rank(comm)

  if(!is.matrix(X.spmd)){
    X.spmd <- as.matrix(X.spmd)
  }

  N.spmd <- nrow(X.spmd)
  N.allspmd <- spmd.allgather.integer(as.integer(N.spmd), integer(COMM.SIZE),
                                      comm = comm)
  N <- sum(N.allspmd)
  n <- ceiling(N / COMM.SIZE)
  new.N.allspmd <- c(rep(n, COMM.SIZE - 1), N - n * (COMM.SIZE - 1))

  rank.org <- rep(0:(COMM.SIZE - 1), N.allspmd)
  rank.belong <- rep(0:(COMM.SIZE - 1), each = n)[1:N]

  send.info <- data.frame(org = rank.org[rank.org == COMM.RANK],
                          belong = rank.belong[rank.org == COMM.RANK])
  recv.info <- data.frame(org = rank.org[rank.belong == COMM.RANK],
                          belong = rank.belong[rank.belong == COMM.RANK])

  list(send = send.info, recv = recv.info, N.allspmd = N.allspmd,
       new.N.allspmd = new.N.allspmd)
} # End of balance.info()


load.balance <- function(X.spmd, bal.info = NULL, comm = .SPMD.CT$comm){
  COMM.RANK <- spmd.comm.rank(comm)
  if(is.null(bal.info)){
    bal.info <- balance.info(X.spmd)
  }

  if(!is.matrix(X.spmd)){
    X.spmd <- as.matrix(X.spmd)
  }
  p <- ncol(X.spmd)

  send.to <- as.integer(unique(bal.info$send$belong))
  if(length(send.to) > 0){
    for(i in send.to){
      if(i != COMM.RANK){
        tmp <- matrix(X.spmd[bal.info$send$belong == i,], ncol = p)
        isend(tmp, rank.dest = i, tag = COMM.RANK, comm = comm)
      }
    }
  }

  recv.from <- as.integer(unique(bal.info$recv$org))
  if(length(recv.from) > 0){
    ret <- NULL
    for(i in recv.from){
      if(i != COMM.RANK){
        tmp <- recv(rank.source = i, tag = i, comm = comm)
      } else{
        tmp <- matrix(X.spmd[bal.info$send$belong == i,], ncol = p)
      }
      ret <- base:::rbind(ret, tmp)
    }
  } else{
    ret <- X.spmd
  }

  if(bal.info$new.N.allspmd[spmd.comm.rank(comm) + 1] == 0){
    ret <- matrix(0, nrow = 0, ncol = p)
  }

  ret
} # End of load.balance().

unload.balance <- function(new.X.spmd, bal.info, comm = .SPMD.CT$comm){
  rev.bal.info <- list(send = data.frame(org = bal.info$recv$belong,
                                         belong = bal.info$recv$org),
                       recv = data.frame(org = bal.info$send$belong,
                                         belong = bal.info$send$org),
                       N.allspmd = bal.info$new.N.allspmd,
                       new.N.allspmd = bal.info$N.allspmd)
  load.balance(new.X.spmd, bal.info = rev.bal.info, comm = comm)
} # End of unload.balance().

