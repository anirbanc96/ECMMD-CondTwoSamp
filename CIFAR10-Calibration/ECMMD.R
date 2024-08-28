#' A parallel version of the sapply function.

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

################################################################################

#' The Linear Kernel
#' @param X - the sample X
#' @param Y - the sample Y
#' @param kernel.bandwidth - a multiplier used for linear kernel
#' @returns the value of the linear kernel applied on X and Y

linear.kernel <- function(X, Y, kernel.bandwidth = 1){
  kernel.bandwidth * X * Y
}

################################################################################

#' The (estimated) ECMMD statistic
#' @param knn - number of nearest neighbors
#' @param X - the sample X
#' @param Y - the sample Y
#' @param knn.graph - the constructed nearest neighbor graph
#' @param kernel.bandwidth - multiplier used for linear kernel
#' @returns the value of the (estimated) ECMMD statistic

ECMMD.test.stat <- function(knn, X, Y, knn.graph, kernel.bandwidth){
  
  num.samples <- length(X)
  
  inner.sum <- mcsapply(1:num.samples, function(i){
    
    sum(linear.kernel(X[rep(i, knn - 1)], X[knn.graph[i,-1]],
                      kernel.bandwidth) - 
        linear.kernel(X[rep(i, knn - 1)], Y[knn.graph[i,-1]],
                      kernel.bandwidth) -
        linear.kernel(Y[rep(i, knn - 1)], X[knn.graph[i,-1]],
                      kernel.bandwidth) + 
        linear.kernel(Y[rep(i, knn - 1)], Y[knn.graph[i,-1]],
                      kernel.bandwidth))
    })/(knn - 1)
  
  return (sum(inner.sum)/num.samples)
}

################################################################################

#' The (estimated) asymptotic variance of ECMMD statistic
#' @param knn - number of nearest neighbors
#' @param X - the sample X
#' @param Y - the sample Y
#' @param knn.graph - the constructed nearest neighbor graph
#' @param kernel.bandwidth - multiplier used for linear kernel
#' @return the value of the (estimated) asymptotic variance of ECMMD statistic

ECMMD.asymptotic.variance <- function(knn, X, Y, knn.graph, kernel.bandwidth){
  
  num.samples <- length(X)
  
  T1 <- matrix(0, nrow = knn-1, ncol = num.samples)
  T2 <- matrix(0, nrow = knn-1, ncol = num.samples)
  T3 <- matrix(0, nrow = knn-1, ncol = num.samples)
  T4 <- matrix(0, nrow = knn-1, ncol = num.samples)
  
  Term.1 <- numeric(num.samples)
  Term.2 <- numeric(num.samples)
  
  for (i in 1:num.samples) {
    T1[, i] <- linear.kernel(X[rep(i, knn - 1)], X[knn.graph[i,-1]],
                             kernel.bandwidth)
    T2[, i] <- linear.kernel(X[rep(i, knn - 1)], Y[knn.graph[i,-1]],
                             kernel.bandwidth)
    T3[, i] <- linear.kernel(Y[rep(i, knn - 1)], X[knn.graph[i,-1]],
                             kernel.bandwidth)
    T4[, i] <- linear.kernel(Y[rep(i, knn - 1)], Y[knn.graph[i,-1]],
                             kernel.bandwidth)
    
    Term.1[i] <- sum((T1[, i] - T2[, i] - T3[, i] + T4[, i])^2)
    
    # exclude the edges that are not neighbor of neighbors
    for (j in 1:(knn-1)) {
      if(!(i %in% knn.graph[knn.graph[i,j+1], ])){
        T1[j, i] <- 0
        T2[j, i] <- 0
        T3[j, i] <- 0
        T4[j, i] <- 0
      }
    }
    Term.2[i] <- sum((T1[, i] - T2[, i] - T3[, i] + T4[, i])^2)
  }
  
  eta1 <- sum(Term.1) / (num.samples * (knn-1))
  eta2 <- sum(Term.2) / (num.samples * (knn-1))
  
  return(eta1 + eta2)
}

################################################################################
