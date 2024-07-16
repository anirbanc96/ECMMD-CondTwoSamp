library(parallel)

mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer))) 
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer)) 
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

# define Gaussian kernel
k_G <- function(X, Y, l = 1){
  l*X*Y
}

# define test statistic
test_stat <- function(k, X, Y, nn_ind, l){
  
  n <- length(X)
  
  T1 <- numeric(n)
  T2 <- numeric(n)
  T3 <- numeric(n)
  T4 <- numeric(n)
  
  for (i in 1:n) {
    T1[i] <- sum(k_G(X[rep(i, k - 1)], X[nn_ind[i,-1]], l)) / (k - 1)
    T2[i] <- sum(k_G(Y[rep(i, k - 1)], Y[nn_ind[i,-1]], l)) / (k - 1)
    T3[i] <- sum(k_G(X[rep(i, k - 1)], Y[nn_ind[i,-1]], l)) / (k - 1)
    T4[i] <- sum(k_G(Y[rep(i, k - 1)], X[nn_ind[i,-1]], l)) / (k - 1)
  }
  
  return (sum(T1+T2-T3-T4)/n)
}


# compute asymptotic variance
asy_var <- function(k, X, Y, nn_ind, l){
  
  n <- length(X)
  
  eta1 <- numeric(n)
  eta2 <- numeric(n)
  
  T1 <- matrix(0, nrow = k-1, ncol = n)
  T2 <- matrix(0, nrow = k-1, ncol = n)
  T3 <- matrix(0, nrow = k-1, ncol = n)
  T4 <- matrix(0, nrow = k-1, ncol = n)
  
  T_s <- numeric(n)
  TT_s <- numeric(n)
  
  for (i in 1:n) {
    T1[, i] <- k_G(X[rep(i, k - 1)], X[nn_ind[i,-1]], l)
    T2[, i] <- k_G(Y[rep(i, k - 1)], Y[nn_ind[i,-1]], l)
    T3[, i] <- k_G(X[rep(i, k - 1)], Y[nn_ind[i,-1]], l)
    T4[, i] <- k_G(Y[rep(i, k - 1)], X[nn_ind[i,-1]], l)
    T_s[i] <- sum((T1[, i] + T2[, i] - T3[, i] - T4[, i])^2)
    
    # exclude the edges that are not two stars
    for (j in 1:(k-1)) {
      if(!(i %in% nn_ind[nn_ind[i,j+1], ])){
        T1[j, i] <- 0
        T2[j, i] <- 0
        T3[j, i] <- 0
        T4[j, i] <- 0
      }
    }
    TT_s[i] <- sum((T1[, i] + T2[, i] - T3[, i] - T4[, i])^2)
  }
  eta1 <- sum(T_s) / (n*(k-1))
  eta2 <- sum(TT_s) / (n*(k-1))
  return(eta1 + eta2)
}


test_stat_derandom <- function(k, X, Y, nn_ind, l){
  
  M <- dim(X)[2]
  
  test_stat <- mean(mcsapply(1:M,
               function(m){test_stat(k, X[, m], Y, nn_ind, l)}))
  
  return (test_stat)
  
}

asy_var_derandom <- function(k, X, Y, nn_ind, l){
  
  n <- dim(X)[1]
  M <- dim(X)[2]
  
  eta1 <- numeric(n)
  eta2 <- numeric(n)
  
  T1 <- matrix(0, nrow = k-1, ncol = n)
  T2 <- matrix(0, nrow = k-1, ncol = n)
  T3 <- matrix(0, nrow = k-1, ncol = n)
  T4 <- matrix(0, nrow = k-1, ncol = n)
  
  T_s <- numeric(n)
  TT_s <- numeric(n)
  
  for (i in 1:n) {
    T1[, i] <- apply(mcsapply(1:M, 
               function(m){k_G(X[rep(i, k - 1), m], X[nn_ind[i,-1], m], l)}),
               1, mean)
    T2[, i] <- k_G(Y[rep(i, k - 1)], Y[nn_ind[i,-1]], l)
    T3[, i] <- apply(mcsapply(1:M,
               function(m){k_G(X[rep(i, k - 1), m], Y[nn_ind[i,-1]], l)}),
               1, mean)
    T4[, i] <- apply(mcsapply(1:M,
               function(m){k_G(Y[rep(i, k - 1)], X[nn_ind[i,-1], m], l)}),
               1, mean)
    T_s[i] <- sum((T1[, i] + T2[, i] - T3[, i] - T4[, i])^2)
    
    # exclude the edges that are not two stars
    for (j in 1:(k-1)) {
      if(!(i %in% nn_ind[nn_ind[i,j+1], ])){
        T1[j, i] <- 0
        T2[j, i] <- 0
        T3[j, i] <- 0
        T4[j, i] <- 0
      }
    }
    TT_s[i] <- sum((T1[, i] + T2[, i] - T3[, i] - T4[, i])^2)
  }
  eta1 <- sum(T_s) / (n*(k-1))
  eta2 <- sum(TT_s) / (n*(k-1))
  return(eta1 + eta2)
}