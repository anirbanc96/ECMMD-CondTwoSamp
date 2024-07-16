source("Test_Var_Stat.R")
library(RANN)

asymp_test <- function(pred, label, K){
  
  n <- length(pred)
  X <- rbinom(n, 1, pred)
  prob_Y <- pred
  Y <-label
  
  l <- 1
  
  test_stat_vals <- numeric(length(K))
  p_value <- numeric(length(K))
  
  for (i in 1:length(K)) {
    k <- K[i]
    
    nn_ind <- nn2(data = cbind(prob_Y, 1 - prob_Y), 
                  query = cbind(prob_Y, 1 - prob_Y), k = k)$nn.idx
    
    test_stat_vals[i] <- sqrt((k - 1)*n) * 
      test_stat(k, X, Y, nn_ind, l) / sqrt(asy_var(k, X, Y, nn_ind, l))
    
    p_value[i] <- 2*(1 - pnorm(abs(test_stat_vals[i])))
  }
  return(list(test = test_stat_vals, p_value = p_value))
  
}

finite_sample_test <- function(pred, label, K, B){
  
  test_value <- matrix(0, nrow = B, ncol = length(K))
  n <- length(pred)
  
  X <- rbinom(n, 1, pred)
  X1.list <- lapply(1:B, function(b){rbinom(n, 1, pred)})
  Y <- label
  
  prob_Y <- pred
  l <- 1
  
  obs_test <- numeric(length(K))
  
  for (i in 1:length(K)) {
    
    k <- K[i]
    nn_ind <- nn2(data = cbind(prob_Y, 1 - prob_Y), 
                  query = cbind(prob_Y, 1 - prob_Y), k = k)$nn.idx
    
    test_value[,i] <- mcsapply(1:B,
                      function(b){sqrt((k - 1)*n) * 
                          test_stat(k, X, X1.list[[b]], nn_ind, l)})
    
    obs_test[i] <- sqrt((k - 1)*n) * test_stat(k, X, Y, nn_ind, l)
    
  }
  
  bind_test <- cbind(as.matrix(obs_test), t(test_value))
  
  p_value <- apply(abs(bind_test), 1, function(x){
    (sum(x[-1] >= x[1]) + 1) / (B+1)
  })
  
  return(list(test_value = obs_test, p_value = p_value))
}


derandomized_test <- function(pred, label, K, M){
  
  n <- length(pred)
  X <- matrix(rbinom(n*M, 1, pred), nrow = n, ncol = M)
  
  prob_Y <- pred
  
  Y <- label
  
  l <- 1
  
  derandom_test <- numeric(length(K))
  p_value <- numeric(length(K))
  
  for (i in 1:length(K)){
    
    k <- K[i]
    nn_ind <- nn2(data = cbind(prob_Y, 1 - prob_Y), 
                  query = cbind(prob_Y, 1 - prob_Y), k = k)$nn.idx
    
    derandom_test[i] <- sqrt((k - 1)*n) * 
      test_stat_derandom(k, X, Y, nn_ind, l) / 
      sqrt(asy_var_derandom(k, X, Y, nn_ind, l))
    
    p_value[i] <- 2*(1 - pnorm(abs(derandom_test[i])))
    
  }
  
  return(list(test = derandom_test, p_value = p_value))
}


compute_ECE <- function(predicted_probs, true_labels, n_bins=10){
  # Arguments:
  # predicted_probs: A vector of predicted probabilities for the positive class.
  # true_labels: A vector of true labels (0 or 1).
  # n_bins: Number of bins to use.
  
  # Check lengths
  if(length(predicted_probs) != length(true_labels)){
    stop("Length of predicted probabilities and true labels must be the same.")
  }
  
  # Create bin edges
  bin_edges <- seq(0, 1, length.out=n_bins+1)
  
  # Initialize variables
  bin_lowers <- bin_edges[-(n_bins+1)]
  bin_uppers <- bin_edges[-1]
  ece <- 0
  
  for(i in 1:n_bins){
    # Indices of instances in the current bin
    idx <- which(predicted_probs > bin_lowers[i] & predicted_probs <= bin_uppers[i])
    
    # Continue if bin is empty
    if(length(idx) == 0) next
    
    # Compute average predicted probability in the bin
    bin_prob <- mean(predicted_probs[idx])
    
    # Compute true accuracy in the bin
    bin_acc <- mean(true_labels[idx])
    
    # Update ECE
    ece <- ece + (length(idx) / length(predicted_probs)) * abs(bin_prob - bin_acc)
  }
  
  return(ece)
}

isotonic_calibration <- function (y, p, p_test, regularization = FALSE) 
{
  if (length(p) != length(y)) 
    stop("Vectors do not match")
  if (!is.numeric(y)) 
    if (is.factor(y)) {
      y <- as.numeric(as.character(y))
    }
  else {
    stop("y is not valid binomial vector")
  }
  if (length(unique(y)) > 2) 
    stop("y is not a valid binomial vector")
  if (!min(unique(y)) == 0) 
    stop("y is not a valid binomial vector")
  if (!max(unique(y)) == 1) 
    stop("y is not a valid binomial vector")
  if (!is.numeric(p)) 
    stop("p arguments must be numeric")
  if (regularization == TRUE) {
    p.max <- (length(y[y == 1]) + 1)/(length(y[y == 1]) + 
                                        2)
    p.min <- 1/(length(y[y == 0]) + 2)
    p <- ifelse(p < p.min, p.min, p)
    p <- ifelse(p > p.max, p.max, p)
  }
  idx <- duplicated(p)
  idx <- which(idx == TRUE)
  if(length(idx) == 0){
    p.unique <- p
    y.unique <- y
  }else{
    p.unique <- p[-idx]
    y.unique <- y[-idx]
  }
  isotonic.calibration <- function(iso, x0) {
    o = iso$o
    if (is.null(o)) 
      o = 1:length(x0)
    x = iso$x[o]
    y = iso$yf
    ind = cut(x0, breaks = x, labels = FALSE, include.lowest = TRUE)
    min.x <- min(x)
    max.x <- max(x)
    adjusted.knots <- iso$iKnots[c(1, which(iso$yf[iso$iKnots] > 
                                              0))]
    fits = sapply(seq(along = x0), function(i) {
      j = ind[i]
      if (is.na(j)) {
        if (x0[i] > max.x) 
          j <- length(x)
        else if (x0[i] < min.x) 
          j <- 1
      }
      upper.step.n <- min(which(adjusted.knots > j))
      upper.step <- adjusted.knots[upper.step.n]
      lower.step <- ifelse(upper.step.n == 1, 1, adjusted.knots[upper.step.n - 
                                                                  1])
      denom <- x[upper.step] - x[lower.step]
      denom <- ifelse(denom == 0, 1, denom)
      val <- y[lower.step] + (y[upper.step] - y[lower.step]) * 
        (x0[i] - x[lower.step])/(denom)
      val <- ifelse(val > 1, max.x, val)
      val <- ifelse(val < 0, min.x, val)
      val <- ifelse(is.na(val), max.x, val)
      val
    })
    return(fits)
  }
  iso.mdl <- stats::isoreg(p.unique, y.unique)
  return(isotonic.calibration(iso.mdl, p_test))
}



data_reliability_diagram <- function(predicted_probs, true_labels, n_bins=10, zoom_range=c(0, 1)) {
  # Create bin edges
  bin_edges <- seq(0, 1, length.out=n_bins+1)
  
  # Initialize variables for plotting
  bin_centers <- numeric(n_bins)
  bin_accuracies <- numeric(n_bins)
  bin_confs <- numeric(n_bins)
  bin_counts <- numeric(n_bins)   # Track the number of observations in each bin
  
  for(i in 1:n_bins) {
    idx <- which(predicted_probs > bin_edges[i] & predicted_probs <= bin_edges[i+1])
    if(length(idx) == 0) next
    bin_centers[i] <- mean(c(bin_edges[i], bin_edges[i+1]))  # Center of the bin
    bin_accuracies[i] <- mean(true_labels[idx])              # Actual accuracy for this bin
    bin_confs[i] <- mean(predicted_probs[idx])               # Average confidence (predicted probability) for this bin
    bin_counts[i] <- length(idx)                             # Number of observations in this bin
  }
  
  df <- data.frame(bin_centers, bin_accuracies, bin_confs, bin_counts)
  
  return(df)
}

