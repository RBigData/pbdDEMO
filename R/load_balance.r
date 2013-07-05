### This file contains functions to load balance of data X.gbd.

balance.info <- function(X.gbd, comm = .SPMD.CT$comm,
    gbd.major = .DEMO.CT$gbd.major, method = .DEMO.CT$divide.method){
  COMM.SIZE <- spmd.comm.size(comm)
  COMM.RANK <- spmd.comm.rank(comm)

  if(!is.matrix(X.gbd)){
    X.gbd <- as.matrix(X.gbd)
  }

  if(gbd.major == 1){
    N.gbd <- nrow(X.gbd)
  } else if(gbd.major == 2){
    N.gbd <- ncol(X.gbd)
  } else{
    stop("gbd.major = 1 or 2.")
  }
  N.allgbd <- spmd.allgather.integer(as.integer(N.gbd), integer(COMM.SIZE),
                                      comm = comm)
  N <- sum(N.allgbd)

  if(method[1] == "block.cyclic"){
    ### This can cause problems.
    # n <- ceiling(N / COMM.SIZE)
    # new.N.allgbd <- c(rep(n, COMM.SIZE - 1), N - n * (COMM.SIZE - 1))
    # rank.org <- rep(0:(COMM.SIZE - 1), N.allgbd)
    # rank.belong <- rep(0:(COMM.SIZE - 1), each = n)[1:N]

    ### Try again.
    n <- ceiling(N / COMM.SIZE)
    rep.n <- N %/% n
    new.N.allgbd <- rep(n, rep.n)
    if(n * rep.n < N){
      new.N.allgbd <- c(new.N.allgbd, (N - n * rep.n))
    }
    if(length(new.N.allgbd) < COMM.SIZE){
      new.N.allgbd <- c(new.N.allgbd,
                         rep(0, COMM.SIZE - length(new.N.allgbd)))
    }
    rank.org <- rep(0:(COMM.SIZE - 1), N.allgbd)
    rank.belong <- rep(0:(COMM.SIZE - 1), new.N.allgbd) 
  } else if(method[1] == "block0"){
    ### Try block0 method which is a better way to balance data. However,
    ### this is not necessary in block-cyclic, so useless for ddmatrix.
    n <- floor(N / COMM.SIZE)
    n.residual <- N %% COMM.SIZE
    new.N.allgbd <- rep(n, COMM.SIZE) +
                     rep(c(1, 0), c(n.residual, COMM.SIZE - n.residual))
    rank.org <- rep(0:(COMM.SIZE - 1), N.allgbd)
    rank.belong <- rep(0:(COMM.SIZE - 1), new.N.allgbd)
  } else{
    comm.stop("method is not found.")
  }

  ### Build send and recv information if any.
  send.info <- data.frame(org = rank.org[rank.org == COMM.RANK],
                          belong = rank.belong[rank.org == COMM.RANK])
  recv.info <- data.frame(org = rank.org[rank.belong == COMM.RANK],
                          belong = rank.belong[rank.belong == COMM.RANK])

  list(send = send.info, recv = recv.info, N.allgbd = N.allgbd,
       new.N.allgbd = new.N.allgbd, gbd.major = gbd.major)
} # End of balance.info()


load.balance <- function(X.gbd, bal.info = NULL, comm = .SPMD.CT$comm,
    gbd.major = .DEMO.CT$gbd.major){
  COMM.RANK <- spmd.comm.rank(comm)
  if(is.null(bal.info)){
    bal.info <- balance.info(X.gbd, comm = comm, gbd.major = gbd.major)
  }

  if(!is.matrix(X.gbd)){
    X.gbd <- as.matrix(X.gbd)
  }
  if(gbd.major == 1){
    p <- ncol(X.gbd)
  } else if(gbd.major == 2){
    p <- nrow(X.gbd)
  } else{
    stop("gbd.major = 1 or 2.")
  }

  send.to <- as.integer(unique(bal.info$send$belong))
  if(length(send.to) > 0){
    if(gbd.major == 1){
      for(i in send.to){
        if(i != COMM.RANK){
          tmp <- matrix(X.gbd[bal.info$send$belong == i,], ncol = p)
          send(tmp, rank.dest = i, tag = COMM.RANK, comm = comm)
        }
      }
    } else{
      for(i in send.to){
        if(i != COMM.RANK){
          tmp <- matrix(X.gbd[, bal.info$send$belong == i], nrow = p)
          send(tmp, rank.dest = i, tag = COMM.RANK, comm = comm)
        }
      }
    }
  }

  recv.from <- as.integer(unique(bal.info$recv$org))
  if(length(recv.from) > 0){
    ret <- NULL
    if(gbd.major == 1){
      for(i in recv.from){
        if(i != COMM.RANK){
          tmp <- recv(rank.source = i, tag = i, comm = comm)
          dim(tmp) <- c(length(tmp) / p, p)
        } else{
          tmp <- matrix(X.gbd[bal.info$send$belong == i,], ncol = p)
        }
        ret <- base:::rbind(ret, tmp)
      }
    } else{
      for(i in recv.from){
        if(i != COMM.RANK){
          tmp <- recv(rank.source = i, tag = i, comm = comm)
          dim(tmp) <- c(p, length(tmp) / p)
        } else{
          tmp <- matrix(X.gbd[, bal.info$send$belong == i], nrow = p)
        }
        ret <- base:::cbind(ret, tmp)
      }
    }
  } else{
    ret <- X.gbd
  }

  if(bal.info$new.N.allgbd[spmd.comm.rank(comm) + 1] == 0){
    if(gbd.major == 1){
      ret <- matrix(0, nrow = 0, ncol = p)
    } else{
      ret <- matrix(0, nrow = p, ncol = 0)
    }
  }

  ret
} # End of load.balance().


unload.balance <- function(new.X.gbd, bal.info, comm = .SPMD.CT$comm){
  rev.bal.info <- list(send = data.frame(org = bal.info$recv$belong,
                                         belong = bal.info$recv$org),
                       recv = data.frame(org = bal.info$send$belong,
                                         belong = bal.info$send$org),
                       N.allgbd = bal.info$new.N.allgbd,
                       new.N.allgbd = bal.info$N.allgbd,
                       gbd.major = bal.info$gbd.major)
  load.balance(new.X.gbd, bal.info = rev.bal.info, comm = comm)
} # End of unload.balance().

