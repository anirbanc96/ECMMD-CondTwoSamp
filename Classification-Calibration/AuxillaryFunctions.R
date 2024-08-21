source("ECMMD.R")

#' P-value of asymptotic test with given number of nearest neighbors
#' @param k - number of nearest neighbors
#' @param num.samples - number of samples
#' @param predicted.probs - probabilities predicted by the model
#' @param X - the sample X
#' @param Y - the sample Y
#' @param kernel.bandwidth - a multiplier to be used for linear kernel
#' @returns p-value of the asymptotic ECMMD based test

asymptotic.test.k <- function(k, num.samples, predicted.probs,
                              X, Y, kernel.bandwidth){
  
  knn.graph <- RANN::nn2(data = cbind(predicted.probs, 1 - predicted.probs), 
                   query = cbind(predicted.probs, 1 - predicted.probs), k = k)$nn.idx
  
  test.stat <- sqrt((k - 1)*num.samples) * 
    ECMMD.test.stat(k, X, Y, knn.graph, kernel.bandwidth) / 
    sqrt(ECMMD.asymptotic.variance(k, X, Y, knn.graph, kernel.bandwidth))
  
  return(2*(1 - pnorm(abs(test.stat))))
  
}

################################################################################

#' Asymptotic test with vector of nearest neighbors
#' @param predicted.probs - probabilities predicted by the model
#' @param test.labels - the labels of the test sample
#' @param knn.vector - vector of number of nearest neighbors
#' @returns a list of knn values and corresponding p-value of asymptotic test

asymptotic.test <- function(predicted.probs, test.labels, knn.vector){
  
  num.samples <- length(predicted.probs)
  X <- rbinom(num.samples, 1, predicted.probs)
  Y <- test.labels
  
  kernel.bandwidth <- 1
  
  p.values <- mcsapply(knn.vector, function(k){
    asymptotic.test.k(k, num.samples, predicted.probs, X, Y, kernel.bandwidth)
  })
  
  return(list(nearest.neighbours = knn.vector,
              p.values = p.values))
  
}

################################################################################

#' P-value of finite sample test with given number of nearest neighbors
#' @param k - number of nearest neighbors
#' @param num.samples - number of samples
#' @param predicted.probs - probabilities predicted by the model
#' @param X - the sample X
#' @param X.resampled.list - list of samples resampled from the distribution X
#' @param Y - the sample Y
#' @param kernel.bandiwidth - a multiplier for linear kernel
#' @param M - number of resamples from distribution of X
#' @returns p-value of the finite sample test

finite.sample.test.k <- function(k, num.samples, predicted.probs, 
                                 X, X.resampled.list, Y, kernel.bandwidth, M){
  
  knn.graph <- RANN::nn2(data = cbind(predicted.probs, 1 - predicted.probs), 
                query = cbind(predicted.probs, 1 - predicted.probs), k = k)$nn.idx
  
  eta.resampled <- mcsapply(1:M, 
                            function(m){sqrt((k-1) * num.samples) * 
                                ECMMD.test.stat(k, X, X.resampled.list[[m]], 
                                                knn.graph, kernel.bandwidth)})
  
  eta.observed <- sqrt((k-1) * num.samples) * 
    ECMMD.test.stat(k, X, Y, knn.graph, kernel.bandwidth)
  
  p.value <- (1 + sum(abs(eta.resampled) >= abs(eta.observed))) / (1 + M)
  
  return (p.value)
  
}

################################################################################

#' Finite sample test with vector of nearest neighbors
#' @param predicted.probs - probabilities predicted by the model
#' @param test.labels - the lables of the test sample
#' @param knn.vector - vector of nearest neighbors
#' @param M - number of resamples from the distribution of X
#' @returns a list of knn values and corresponding p-value of finite sample test

finite.sample.test <- function(predicted.probs, test.labels, knn.vector, M){
  
  num.samples <- length(predicted.probs)
  X <- rbinom(num.samples, 1, predicted.probs)
  X.resampled.list <- lapply(1:M, function(m){rbinom(num.samples,
                                                     1, predicted.probs)})
  Y <- test.labels
  
  kernel.bandwidth <- 1
  p.values <- mcsapply(knn.vector, function(k){finite.sample.test.k(k, 
                                                  num.samples, predicted.probs, 
                                                  X, X.resampled.list, Y,
                                                  kernel.bandwidth, M)})
  
  return(list(nearest.neighbours = knn.vector,
              p.values = p.values))
  
}

################################################################################

#' Compute ECE values
#' @param predicted_probs - the probabilities predicted by the model
#' @param true_labels - true labels from the test sample
#' @param n_bins - number of bins for computing ECE
#' @returns the estimated ECE value

compute.ECE <- function(predicted_probs, true_labels, n_bins=10){
  
  if(length(predicted_probs) != length(true_labels)){
    stop("Length of predicted probabilities and true labels must be the same.")
  }
  
  bin_edges <- seq(0, 1, length.out=n_bins+1)
  
  bin_lowers <- bin_edges[-(n_bins+1)]
  bin_uppers <- bin_edges[-1]
  ece <- 0
  
  for(i in 1:n_bins){

    idx <- which(predicted_probs > bin_lowers[i] & predicted_probs <= bin_uppers[i])
    
    if(length(idx) == 0) next
    
    bin_prob <- mean(predicted_probs[idx])
    
    bin_acc <- mean(true_labels[idx])
    
    ece <- ece + (length(idx) / length(predicted_probs)) * abs(bin_prob - bin_acc)
  }
  
  return(ece)
}

################################################################################

#' Train the isotonic regression for re-calibration
#' @param labels - the binary labels of the data
#' @param probs - the probabilities predicted by the model
#' @returns - the trained isotonic regression model

isotonic.calibration.train <- function (labels, probs, regularization = FALSE) 
  {
  if (length(probs) != length(labels)) 
    stop("Vectors do not match")
  if (!is.numeric(labels)) 
    if (is.factor(labels)) {
      labels <- as.numeric(as.character(labels))
    }
  else {
    stop("y is not valid binomial vector")
  }
  if (length(unique(labels)) > 2) 
    stop("labels are not a valid binomial vector")
  if (!min(unique(labels)) == 0) 
    stop("labels are not a valid binomial vector")
  if (!max(unique(labels)) == 1) 
    stop("labels are not a valid binomial vector")
  if (!is.numeric(probs)) 
    stop("probs arguments must be numeric")
  if (regularization == TRUE) {
    probs.max <- (length(labels[labels == 1]) + 1)/(length(labels[labels == 1]) + 2)
    probs.min <- 1/(length(labels[labels == 0]) + 2)
    probs <- ifelse(probs < probs.min, probs.min, probs)
    probs <- ifelse(probs > probs.max, probs.max, probs)
  }
  idx <- duplicated(probs)
  idx <- which(idx == TRUE)
  if(length(idx) == 0){
    probs.unique <- probs
    labels.unique <- labels
  }else{
    probs.unique <- probs[-idx]
    labels.unique <- labels[-idx]
  }
  
  iso.trained <- stats::isoreg(probs.unique, labels.unique)
  return(iso.trained)
}

################################################################################

#' Function to use trained isotonic regression to transform predicted probs
#' @param trained.isotonic - the trained isotonic regression
#' @param pred.probs - the predicted probabilities to be transformed
#' @returns vector of transformed probabilities

isotonic.calibration.pred <- function(trained.isotonic, pred.probs) {
  o = trained.isotonic$o
  if (is.null(o)) 
    o = 1:length(pred.probs)
  x = trained.isotonic$x[o]
  y = trained.isotonic$yf
  ind = cut(pred.probs, breaks = x, labels = FALSE, include.lowest = TRUE)
  min.x <- min(x)
  max.x <- max(x)
  adjusted.knots <- 
    trained.isotonic$iKnots[c(1,
                      which(trained.isotonic$yf[trained.isotonic$iKnots] > 0))]
  fits = sapply(seq(along = pred.probs), function(i) {
    j = ind[i]
    if (is.na(j)) {
      if (pred.probs[i] > max.x) 
        j <- length(x)
      else if (pred.probs[i] < min.x) 
        j <- 1
    }
    upper.step.n <- min(which(adjusted.knots > j))
    upper.step <- adjusted.knots[upper.step.n]
    lower.step <- ifelse(upper.step.n == 1, 1, adjusted.knots[upper.step.n - 
                                                                1])
    denom <- x[upper.step] - x[lower.step]
    denom <- ifelse(denom == 0, 1, denom)
    val <- y[lower.step] + (y[upper.step] - y[lower.step]) * 
      (pred.probs[i] - x[lower.step])/(denom)
    val <- ifelse(val > 1, max.x, val)
    val <- ifelse(val < 0, min.x, val)
    val <- ifelse(is.na(val), max.x, val)
    val
  })
  return(fits)
}

################################################################################

#' Function to return a dataframe for plotting reliability diagrams
#' @param predicted_probs - the predicted probabilities by the model
#' @param true_lables - true lables from the test set
#' @param n_bins - number of bins to plot the reliability diagram
#' @returns a datafram with Center of the bin, Actual accuracy for this bin,
#'          Average confidence (predicted probability) for this bin and Number of observations in this bin

data.reliability.diagram <- function(predicted_probs, true_labels,
                                     n_bins=10, zoom_range=c(0, 1)) {

  bin_edges <- seq(0, 1, length.out=n_bins+1)
  
  bin_centers <- numeric(n_bins)
  bin_accuracies <- numeric(n_bins)
  bin_confs <- numeric(n_bins)
  bin_counts <- numeric(n_bins)
  
  for(i in 1:n_bins) {
    idx <- which(predicted_probs > bin_edges[i] & predicted_probs <= bin_edges[i+1])
    if(length(idx) == 0) next
    bin_centers[i] <- mean(c(bin_edges[i], bin_edges[i+1]))  
    bin_accuracies[i] <- mean(true_labels[idx])              
    bin_confs[i] <- mean(predicted_probs[idx])               
    bin_counts[i] <- length(idx)                             
  }
  
  df <- data.frame(bin_centers, bin_accuracies, bin_confs, bin_counts)
  
  return(df)
}

