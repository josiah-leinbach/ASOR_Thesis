########################
### Emission density ###
########################

emmission_den <- function(y, alpha, gamma, StateList){
  n <- length(y)
  K <- length(alpha)
  xtemp <- table(y)
  stemp <- names(xtemp)
  mtch <- match(StateList, stemp)
  xfreq <- rep(NA, K)
  ctr <- 0
  for (k in 1:K){
    if (!is.na(mtch[k])){
      ctr <- ctr+1
      xfreq[k] <- xtemp[ctr]
    }else{
      xfreq[k] <- 0
    }
  }
  xtran <- matrix(0, nrow = K, ncol = K)
  if (n > 1){
    for (i in 2:n){
      for (k in 1:K){
        if (y[i-1] == StateList[k]){
          xtran[k,match(y[i],StateList)] <- xtran[k,match(y[i],StateList)] + 1
        }
      }
    } 
  }
  den <- 0
  for (k in 1:K){
    if (y[1] == StateList[k]){
      den <- den + log(alpha[k])
    }
    for (l in 1:K){
      den <- den + xtran[k,l]*log(gamma[k,l])
    }
  }
  den <- exp(den)
  return(den)
}

##############
### E-step ###
##############


E.step <- function(y, R1, d1, N, pi, A, Alpha, Gamma, StateList){
  
  R <- length(pi)
  R2 <- R - R1
  B <- matrix(0, nrow = R1, ncol = R)
  for (i in 1:R1){
    B[i,i] <- 1
  }
  
  L <- length(y)
  N <- rep(NA, L)
  
  for (l in 1:L){
    N[l] <- length(y[[l]])
  }
  
  #### a, b, gamma, zeta are components of the forward-backward algorithm ####
  
  a <- array(0, dim = c(L, max(N), R))
  b <- array(0, dim = c(L, max(N), R))
  
  gamma <- array(0, dim = c(L, max(N), R))
  zeta <- array(0, dim = c(L, max(N), R, R))
  
  log_f <- rep(NA, L)
  
  for (i in 1:L){
    
    if (i <= d1){
      aa <- B%*%pi
      pit <- aa/norm(aa)
      bb <- B%*%A%*%t(B)
      At <- bb/apply(bb, 1,sum)
      Rcurrent <- R1
    }else{
      pit <- pi
      At <- A
      Rcurrent <- R
    }
    
    scale <- rep(1,N[i])
    
    for (r in 1:Rcurrent){
      a[i,1,r] <- pit[r]*emmission_den(y[[i]][[1]], Alpha[r,], Gamma[r,,], StateList)
      b[i,N[i],r] <- 1
      a[i,1,r] <- min(a[i,1,r], .Machine$double.xmax)
      a[i,1,r] <- max(a[i,1,r], .Machine$double.xmin)
    }
    
    for (t in 2:N[i]){
      for (r in 1:Rcurrent){
        s1 <- 0
        for (s in 1:Rcurrent){
          s1 <- s1 + a[i,t-1,s]*At[s,r] 
        }
        a[i,t,r] <- s1*emmission_den(y[[i]][[t]], Alpha[r,], Gamma[r,,], StateList)
        a[i,t,r] <- min(a[i,t,r], .Machine$double.xmax)
        a[i,t,r] <- max(a[i,t,r], .Machine$double.xmin)
      }
      scale[t] <- 1/ifelse(sum(a[i,t,]) == 0, 1, sum(a[i,t,])) 
      a[i,t,] <- a[i,t,]*scale[t]
    }
    
    for (t in (N[i]-1):1){
      for (r in 1:Rcurrent){
        s2 <- 0
        for (s in 1:Rcurrent){
          s2 <- s2 + At[r,s]*emmission_den(y[[i]][[t+1]], Alpha[s,], Gamma[s,,], StateList)*b[i,(t+1),s]
        }
        b[i,t,r] <- s2
        b[i,t,r] <- b[i,t,r]*scale[t]
        b[i,t,r] <- min(b[i,t,r], .Machine$double.xmax)
        b[i,t,r] <- max(b[i,t,r], .Machine$double.xmin)
      }
    }
    
    for (t in 1:N[i]){
      for (r in 1:Rcurrent){
        gamma[i,t,r] <- a[i,t,r]*b[i,t,r]
        gamma[i,t,r] <- min(gamma[i,t,r], .Machine$double.xmax)
        gamma[i,t,r] <- max(gamma[i,t,r], .Machine$double.xmin)
      }
      gamma[i,t,] <- gamma[i,t,]/ifelse(sum(gamma[i,t,]) == 0, 1, sum(gamma[i,t,])) 
      if (t == 1){
        zeta[i,t,,] <- 0
      }
      if (t > 1){
        for (r in 1:Rcurrent){
          for (s in 1:Rcurrent){
            zeta[i,t,r,s] <- a[i,(t-1),r]*At[r,s]*emmission_den(y[[i]][[t]], Alpha[s,], Gamma[s,,], StateList)*b[i,t,s]
            zeta[i,t,r,s] <- min(zeta[i,t,r,s], .Machine$double.xmax)
            zeta[i,t,r,s] <- max(zeta[i,t,r,s], .Machine$double.xmin)
          }
        }
        zeta[i,t,,] <- zeta[i,t,,]/ifelse(sum(zeta[i,t,,]) == 0, 1, sum(zeta[i,t,,])) 
      }
    }
    
    log_f[i] <- - sum(log(scale))
    #cat("logL =", log_f[i], "\n")
  }
  # log_f <- pmin(log_f, log(.Machine$double.xmax))
  # log_f <- pmax(log_f, log(.Machine$double.xmin))
  logL <- sum(log_f)
  return(list(gamma=gamma, zeta=zeta,logL=logL))
}

##############
### M step ###
##############

M.step <- function(y, R1, d1, N, gamma, zeta, StateList){
  R <- dim(gamma)[3]
  R2 <- R - R1
  B <- matrix(0, nrow = R1, ncol = R)
  for (i in 1:R1){
    B[i,i] <- 1
  }
  
  K <- length(StateList)
  L <- length(y)
  N <- rep(NA, L)
  
  for (l in 1:L){
    N[l] <- length(y[[l]])
  }
  
  n <- matrix(NA, nrow = L, ncol = max(N))
  for (l in 1:L){
    for (j in 1:N[l]){
      n[l,j] <- length(y[[l]][[j]])
    }
  }
  
  pi <- rep(0, R)
  A <- matrix(0, nrow = R, ncol = R)
  
  Xtran <- array(0, dim = c(L,max(N),K,K))
  for (l in 1:L){
    for (j in 1:N[l]){
      if (n[l,j] > 1){
        for (i in 2:n[l,j]){
          for (k in 1:K){
            if (y[[l]][[j]][i-1] == StateList[k]){
              Xtran[l,j,k,match(y[[l]][[j]][i],StateList)] <- Xtran[l,j,k,match(y[[l]][[j]][i],StateList)] + 1
            }
          }
        }
      }
    }
  }
  
  for (r in 1:R){
    if (r <= R1){
      for (l in 1:L){
        pi[r] <- pi[r] + gamma[l,1,r]
      }
      pi[r] <- pi[r]/L
      for (s in 1:R){
        if (s <= R1){
          for (l in 1:L){
            for (j in 2:N[l]){
              A[r,s] <- A[r,s] + zeta[l,j,r,s] 
            }
          }
        }else{
          for (l in (d1+1):L){
            for (j in 2:N[l]){
              A[r,s] <- A[r,s] + zeta[l,j,r,s] 
            }
          }
        }
      }
    }else{
      for (l in (d1+1):L){
        pi[r] <- pi[r] + gamma[l,1,r]
      }
      pi[r] <- pi[r]/(L-d1)
      for (s in 1:R){
        for (l in (d1+1):L){
          for (j in 2:N[l]){
            A[r,s] <- A[r,s] + zeta[l,j,r,s] 
          }
        }
      }
    }
  }
  pi <- pi/sum(pi)
  A <- A/apply(A, 1, sum)
  
  Alpha <- matrix(0, nrow = R, ncol = K)
  Gamma <- array(0, dim = c(R,K,K))
  
  for (r in 1:R){
    if (r <= R1){
      for (k in 1:K){
        num1 <- 0
        den1 <- 0
        ctr <- 0
        for (i in 1:L){
          for (j in 1:N[i]){
            den1 <- den1 + gamma[i,j,r]
            if (y[[i]][[j]][1] == StateList[k]){
              ctr <- ctr+1
              num1 <- num1 + gamma[i,j,r]
            }
          }
        }
        # cat("R", r, "K", k, "ctr", ctr, "num1", num1, "den1", den1, "\n")
        Alpha[r,k] <- num1/den1
      } 
    }else{
      for (k in 1:K){
        num1 <- 0
        den1 <- 0
        ctr <- 0
        for (i in (d1+1):L){
          for (j in 1:N[i]){
            den1 <- den1 + gamma[i,j,r]
            if (y[[i]][[j]][1] == StateList[k]){
              ctr <- ctr+1
              num1 <- num1 + gamma[i,j,r]
            }
          }
        }
        # cat("R", r, "K", k, "ctr", ctr, "num1", num1, "den1", den1, "\n")
        Alpha[r,k] <- num1/den1
      }
    }
  }
  Alpha <- Alpha/apply(Alpha, 1, sum)
  
  for (r in 1:R){
    if (r <= R1){
      for (k in 1:K){
        for (m in 1:K){
          num2 <- 0
          den2 <- 0
          for (i in 1:L){
            for (j in 1:N[i]){
              num2 <- num2 + gamma[i,j,r]*Xtran[i,j,k,m]
              denfac <- 0
              for (mm in 1:R){
                denfac <- denfac + Xtran[i,j,k,mm]
              }
              den2 <- den2 + gamma[i,j,r]*denfac
            }
          }
          Gamma[r,k,m] <- num2/den2
        }
      } 
    }else{
      for (k in 1:K){
        for (m in 1:K){
          num2 <- 0
          den2 <- 0
          for (i in (d1+1):L){
            for (j in 1:N[i]){
              num2 <- num2 + gamma[i,j,r]*Xtran[i,j,k,m]
              denfac <- 0
              for (mm in 1:R){
                denfac <- denfac + Xtran[i,j,k,mm]
              }
              den2 <- den2 + gamma[i,j,r]*denfac
            }
          }
          Gamma[r,k,m] <- num2/den2
        }
      } 
    }
    Gamma[r,,] <- Gamma[r,,]/apply(Gamma[r,,], 1, sum)
    draw <- Gamma[r,,]
    draw[is.nan(draw)] <- 1e-6
    draw[draw == 0] <- 1e-6
    Gamma[r,,] <- draw
  }
  
  return(list(pi=pi, A=A, Alpha=Alpha, Gamma=Gamma))
}


###############
### Long EM ###
###############

EM <- function(y, R1, d1, N, pi, A, Alpha, Gamma, StateList, eps=0.00001, max.itr){
  
  R <- length(pi)
  R2 <- R - R1
  B <- matrix(0, nrow = R1, ncol = R)
  for (i in 1:R1){
    B[i,i] <- 1
  }
  
  K <- length(StateList)
  L <- length(y)
  N <- rep(NA, L)
  
  for (l in 1:L){
    N[l] <- length(y[[l]])
  }
  
  n <- matrix(NA, nrow = L, ncol = max(N))
  for (l in 1:L){
    for (j in 1:N[l]){
      n[l,j] <- length(y[[l]][[j]])
    }
  }
  
  b <- 0
  ll.old <- -Inf
  cat("Iteration", b, "logL =", ll.old, "\n")
  
  repeat{
    b <- b + 1
    E <- E.step(y, R1, d1, N, pi, A, Alpha, Gamma, StateList)
    
    gamma <- E$gamma
    zeta <- E$zeta
    
    ll <- E$logL
    cat("Iteration", b, "logL =", ll, "\n")
    if (b == max.itr) break
    if (abs(ll - ll.old) / abs(ll) < eps) break 
    ll.old <- ll
    M <- M.step(y, R1, d1, N, gamma, zeta, StateList)
    pi <- M$pi
    A <- M$A
    Alpha <- M$Alpha
    Gamma <- M$Gamma
  }
  par <- (R-1) + R*(R-1) + R*((K-1) + K*(K-1))
  
  BIC <- -2* ll.old + par * log(sum(N))
  AIC <- -2* ll.old + par * 2
  
  id <- array(NA, dim = c(L, max(N)))
  for (l in 1:L){
    for (j in 1:N[l]){
      id[l,j] <- which.max(gamma[l,j,])
    }
  }
  
  return(list(gamma=gamma, zeta=zeta, logL=ll, BIC = BIC, AIC=AIC, Alpha=Alpha, Gamma=Gamma,
              pi=pi, A=A, id=id))
}
