
#' Kernel function
#'
#' @param X First argument
#' @param Y Second argument
#' @param l The hyperparameter
#' @param kernel_choice Choice of kernel function
#'
#' @return Kernel value
#' @export

kernel <- function(X, Y, l = NULL, kernel_choice = "Linear"){
  switch (kernel_choice,
          Linear = {
            return(X*Y)
          },
          Gaussian = {
            return(exp(-(X - Y)^2 / l^2))
          }
  )
}

#' Multivariate kernel function
#'
#' @param X First argument with dimension greater or equal than 2
#' @param Y Second argument with dimension greater or equal than 2
#' @param l Bandwidth
#' @param kernel_choice Choice of kernel
#'
#' @return Kernel value
#' @export

kernel_multivariate <- function(X, Y, l = NULL, kernel_choice = "Linear"){
  switch (kernel_choice,
          Linear = {
            return(sum(X*Y))
          },
          Gaussian = {
            return(exp(-sum((X - Y)^2) / l^2))
          }
  )
}

#' Define test statistic
#'
#' @param k Number of nearest neighbors used
#' @param X Observed sample X
#' @param Y Observed sample Y
#' @param nn_ind Nearest neighbor matrix of size n by k; the first column is the i-th unit value
#' @param l The hyperparameter for Gaussian kernel
#' @param kernel_choice Choice of kernel function, the default is linear kernel
#' @param num_derandom Number of derandomized sample used
#'
#' @return Test statistic
#' @export

test_stat <- function(k, X, Y, nn_ind, l,
                      kernel_choice = "Linear", num_derandom = 1){
  n <- nrow(as.matrix(X))
  if(is.list(Y)){
    Y
  }else{
    Y <- list(Y)
  }
  if(length(Y) != num_derandom){
    stop("The length of Y as a list should be same as num_derandom!")
  }
  T1 <- numeric(n)
  T2 <- numeric(n)
  T3 <- numeric(n)
  T4 <- numeric(n)
  for (i in 1:n) {
    T1[i] <- mean(sapply(Y, function(y){
      y <- as.vector(y)
      sum(kernel(X[rep(i, k)], X[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)) / k
    }))
    T2[i] <- mean(sapply(Y, function(y){
      y <- as.vector(y)
      sum(kernel(y[rep(i, k)], y[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)) / k
    }))
    T3[i] <- mean(sapply(Y, function(y) {
      y <- as.vector(y)
      sum(kernel(X[rep(i, k)], y[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)) / k
    }))
    T4[i] <- mean(sapply(Y, function(y) {
      y <- as.vector(y)
      sum(kernel(y[rep(i, k)], X[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)) / k
    }))
  }
  sum(T1+T2-T3-T4) / n
}


#' Compute asymptotic conditional variance estimate
#'
#' @param k Number of nearest neighbors
#' @param X Observed sample X
#' @param Y Observed sample Y
#' @param nn_ind earest neighbor matrix of size n by k; the first column is the i-th unit value
#' @param l The hyperparameter
#' @param kernel_choice Choice of kernel function, the default is linear kernel
#' @param num_derandom Number of derandomized sample used
#'
#' @return asymptotic variance estimate
#' @export

asy_var <- function(k, X, Y, nn_ind, l, kernel_choice = "Linear", num_derandom = 1){
  n <- length(X)
  if(!is.list(Y)){
    Y <- list(Y)
  }
  if(length(Y) != num_derandom){
    stop("The length of Y as a list should be same as num_derandom!")
  }
  eta1 <- numeric(n)
  eta2 <- numeric(n)
  T1 <- matrix(0, nrow = k, ncol = n)
  T2 <- matrix(0, nrow = k, ncol = n)
  T3 <- matrix(0, nrow = k, ncol = n)
  T4 <- matrix(0, nrow = k, ncol = n)
  T_s <- numeric(n)
  TT_s <- numeric(n)
  for (i in 1:n) {
    T1[, i] <- apply(as.matrix(sapply(Y, function(y) {
      y <- as.vector(y)
      kernel(X[rep(i, k)], X[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)
    })), 1, mean)
    T2[, i] <- apply(as.matrix(sapply(Y, function(y) {
      y <- as.vector(y)
      kernel(y[rep(i, k)], y[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)
    })), 1, mean)
    T3[, i] <- apply(as.matrix(sapply(Y, function(y) {
      y <- as.vector(y)
      kernel(X[rep(i, k)], y[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)
    })), 1, mean)
    T4[, i] <- apply(as.matrix(sapply(Y, function(y) {
      y <- as.vector(y)
      kernel(y[rep(i, k)], X[nn_ind[i, ]], l = sqrt(median((X-y)^2)), kernel_choice)
    })), 1, mean)
    T_s[i] <- sum((T1[, i] + T2[, i] - T3[, i] - T4[, i])^2)
    # exclude the edges that are not two stars
    for (j in 1:k) {
      if(!(i %in% nn_ind[nn_ind[i, j], ])){
        T1[j, i] <- 0
        T2[j, i] <- 0
        T3[j, i] <- 0
        T4[j, i] <- 0
      }
    }
    TT_s[i] <- sum((T1[, i] + T2[, i] - T3[, i] - T4[, i])^2)
  }
  eta1 <- sum(T_s) / (n*k)
  eta2 <- sum(TT_s) / (n*k)
  return(eta1 + eta2)
}

#' Expit function
#'
#' @param x Argument
#'
#' @return Expit function value
#' @export

expit <- function(x){
  exp(x) / (1 + exp(x))
}


#' Score function
#'
#' @param x Conditioning argument
#' @param y Gaussian argument
#' @param mu Mean function
#' @param sigma Standard deviation function
#'
#' @return Score value evaluated at y|x
#' @export

score <- function(x, y, mu, sigma){
  return((mu(x) - y) / (sigma(x))^2)
}

#' Kernel function in the U-statistic
#'
#' @param x1 First argument of kernel function in X space
#' @param x2 Second argument of kernel function in X space
#' @param y1 First argument of kernel function in Y space
#' @param y2 Second argument of kernel function in Y space
#' @param lx Bandwidth chosen for kernel function in X space
#' @param ly Bandwidth chosen for kernel function in Y space
#' @param mu Mean function
#' @param sigma Standard deviation function
#'
#' @return Kernel value in the U-stat
#' @export

kernel_Ustat <- function(x1, x2, y1, y2, lx, ly, mu, sigma){
  l <- kernel_multivariate(y1, y2, l = ly, kernel_choice = "Gaussian")
  k <- kernel_multivariate(x1, x2, l = lx, kernel_choice = "Gaussian")
  der_y1_l <- 2 * (y2 - y1) / (ly^2) * l
  der_y2_l <- 2 * (y1 - y2) / (ly^2) * l
  der_y1y2_l <- 2 * (1 - 2 * (y1 - y2)^2 / ly^2) * l / ly^2
  s_x1_y1 <- score(x1, y1, mu, sigma)
  s_x2_y2 <- score(x2, y2, mu, sigma)
  T1 <- l*s_x1_y1*s_x2_y2
  T2 <- der_y1y2_l
  T3 <- s_x1_y1*der_y2_l
  T4 <- s_x2_y2*der_y1_l
  return(k*(T1 + T2 + T3 + T4))
}

#' SKCE estimator for classification calibration
#'
#' @param predictor Predictor matrix
#' @param response Response vector
#' @param weight_mat Weight matrix of size n by n
#' @param gamma Hyperparameter in Laplace dot kernel
#'
#' @return SKCE estimator
#' @export

SKCE_classification <- function(predictor,
                                response,
                                weight_mat = matrix(1,
                                                    nrow(predictor),
                                                    nrow(predictor)
                                ),
                                gamma = 2){

  # define the kernel function
  laplace <- laplacedot(sigma = gamma)
  kernel_mat_laplace <- kernelMatrix(laplace, x = as.matrix(predictor))

  # quadratic term
  diff_response_predictor <- response - predictor
  n_choose_2_diff_1 <- t(combn(diff_response_predictor[, 1], 2))
  n_choose_2_diff_2 <- t(combn(diff_response_predictor[, 2], 2))
  vectorize_result <- apply(n_choose_2_diff_1, 1, prod) + apply(n_choose_2_diff_2, 1, prod)
  quadratic_mat <- matrix(0, nrow(predictor), nrow(predictor))
  quadratic_mat[lower.tri(quadratic_mat, diag=FALSE)] <- vectorize_result
  quadratic_mat2 <- t(quadratic_mat)
  quadratic_mat2[lower.tri(quadratic_mat2, diag=FALSE)] <- vectorize_result
  diag(quadratic_mat2) <- apply(diff_response_predictor^2, 1, sum)

  # final matrix
  final_mat <- weight_mat*quadratic_mat2*kernel_mat_laplace

  return(sum(final_mat) / nrow(final_mat))
}

#' Bootstrap for SKCE classification calibration
#'
#' @param predictor Predictor matrix
#' @param gamma Hyperparameter in Laplace kernel
#' @param B Number of bootstrap
#'
#' @return A vector of bootstrap sample of length B
#' @export

bootstrap_SKCE_classification <- function(predictor, gamma = 1, B = 500){

  # create empty bootstrap vector
  boot_list <- numeric(B)
  n <- nrow(predictor)
  for (i in 1:B) {
    response_resample <- matrix(0, 2, n)
    output_resample <- rbinom(n,
                              size = 1,
                              prob = predictor[, 1])
    response_resample[1, which(output_resample != 0)] <- 1
    response_resample[2, which(output_resample == 0)] <- 1
    boot_list[i] <- SKCE_classification(predictor = predictor,
                                        response = t(response_resample), gamma)
  }
  return(boot_list)
}

#' SKCE for regression calibration
#'
#' @param pseduo_data Containing observed data and sufficient statistics
#' @param weight_mat Weight matrix used in constructing test statistic
#' @param gamma Hyperparameter in Laplace kernel
#'
#' @return SKCE value
#' @export


SKCE_regression <- function(pseduo_data,
                            weight_mat = matrix(1, nrow(pseduo_data), nrow(pseduo_data)),
                            gamma = 1){

  # define the kernel function
  laplace <- laplacedot(sigma = 1)
  se <- rbfdot(sigma = gamma)
  kernel_mat_laplace <- kernelMatrix(laplace, x = as.matrix(pseduo_data[, 2:3]))
  kernel_mat_se <- kernelMatrix(se, x = as.matrix(pseduo_data[, 1]))
  random_term <- weight_mat*kernel_mat_laplace*kernel_mat_se

  # cross term
  integrate_part <- function(mean, sd, y){
    (1+2*gamma*sd^2)^{-1/2}*exp(-gamma*(mean - y)^2 / (1+2*gamma*sd^2))
  }
  repeat_mean <- rep(pseduo_data$px_mean, times = length(pseduo_data$px_mean))
  repeat_sd <- rep(pseduo_data$px_sd, times = length(pseduo_data$px_sd))
  copy_y <- rep(pseduo_data$y, each = length(pseduo_data$y))
  vectorize_part_result <- integrate_part(repeat_mean, repeat_sd, copy_y)
  cross_term <- weight_mat*kernel_mat_laplace * matrix(vectorize_part_result,
                                                       nrow(pseduo_data),
                                                       nrow(pseduo_data))

  # integrate_out part
  integrate_complete <- function(mean_x, sd_x, mean_y, sd_y){
    (1+2*gamma*(sd_x^2+sd_y^2))^{-1/2}*exp(-gamma*(mean_x - mean_y)^2 / (1+2*gamma*(sd_x^2+sd_y^2)))
  }

  n_choose_2_mean <- t(combn(pseduo_data$px_mean, 2))
  n_choose_2_sd <- t(combn(pseduo_data$px_sd, 2))
  vectorize_result <- integrate_complete(n_choose_2_mean[,1], n_choose_2_sd[,1],
                                         n_choose_2_mean[,2], n_choose_2_sd[,2])

  m1 <- matrix(0, nrow = nrow(pseduo_data), ncol = nrow(pseduo_data))
  m1[lower.tri(m1, diag=FALSE)] <- vectorize_result
  m2 <- t(m1)
  m2[lower.tri(m2, diag=FALSE)] <- vectorize_result
  diag(m2) <- integrate_complete(pseduo_data$px_mean, pseduo_data$px_sd,
                                 pseduo_data$px_mean, pseduo_data$px_sd)

  integration_term <- weight_mat*m2*kernel_mat_laplace

  return((sum(random_term) + sum(integration_term) - 2*sum(cross_term)) / nrow(pseduo_data))
}


#' Bootstrap for SKCE regression
#'
#' @param pseduo_data Same for SKCE_regression
#' @param gamma Same for SKCE_regression
#' @param B Number of bootstrap
#'
#' @return A vector of bootstrap sample under null
#' @export

bootstrap_SKCE_regression <- function(pseduo_data, gamma = 1, B = 500){
  boot_list <- numeric(B)
  for (i in 1:B) {
    weight_mat <- matrix(rep(rnorm(nrow(pseduo_data)), times = nrow(pseduo_data)), nrow(pseduo_data), nrow(pseduo_data))
    boot_list[i] <- SKCE_regression(pseduo_data, weight_mat, gamma)
  }
  return(boot_list)
}


#' Compute the expected calibration error
#'
#' @param predicted_probs A vector of predicted probabilities for the positive class.
#' @param true_labels A vector of true labels (0 or 1).
#' @param n_bins Number of bins to use.
#'
#' @return ECE value
#' @export

compute_ECE <- function(predicted_probs, true_labels, n_bins=10){

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


#' Return calibration using Isotonic regression (fit the test point x0 with the model)
#'
#' @param iso Isotonic regression obtained from isoreg function
#' @param x0 Test point, predicted probability vector
#'
#' @return Calibrated predicted probability
#' @export

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

#' Isotonic calibration function
#'
#' @param y Training observation for Isotonic regression (a subset of test data)
#' @param p Prediction probability obtained from some trained model
#' @param p_test Prediction proabability on the rest of data
#'
#' @return Calibrated predicted probability
#' @export

isotonic_calibration <- function (y, p, p_test)
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

  # compute the unique training probability and outcome observation
  idx <- duplicated(p)
  idx <- which(idx == TRUE)
  if(length(idx) == 0){
    p.unique <- p
    y.unique <- y
  }else{
    p.unique <- p[-idx]
    y.unique <- y[-idx]
  }

  # training isotonic regression
  iso.mdl <- stats::isoreg(p.unique, y.unique)

  # output the calibrated probability
  return(isotonic.calibration(iso.mdl, p_test))
}



#' Data preparation for plotting reliability diagram
#'
#' @param predicted_probs Predicted probability vector
#' @param true_labels True observation label
#' @param n_bins Number of bins used
#'
#' @return A dataframe containing necessary ingredients for reliability diagram
#' @export

data_reliability_diagram <- function(predicted_probs, true_labels, n_bins=10) {
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

