#' Asymptotic test
#'
#' @param data Data containing observed sample X, sample Y and covariate Z
#' @param k Number of nearest neighbors used
#' @param hyper Hyperparameter used in kernel function
#' @param kernel_choice Choice of kernel function
#'
#' @return Normalized test statistic and p-value
#' @export
#' @import RANN
#' @importFrom stats dist median pnorm rnorm
#' @importFrom utils combn data

asy_test <- function(data, k, hyper = NULL, kernel_choice = "Linear"){

  # extract data
  X <- data$X
  Y <- data$Y
  Z <- data$Z
  n <- nrow(as.matrix(X))

  # compute the nearest neighbor (exclude the first column)
  nn_ind <- nn2(data = Z, query = Z, k = k + 1)$nn.idx[, -1]

  # specify the hyperparameter for Gaussian kernel function
  if(kernel_choice == "Gaussian"){
    l <- switch (hyper,
      NULL = {
        median(sqrt(apply(as.matrix((X - Y)^2), 1, sum)))
      },
      median_diff = {
        median(sqrt(apply(as.matrix((X - Y)^2), 1, sum)))
      }
    )
  }

  # compute the test statistic
  asy_variance <- asy_var(k, X, Y, nn_ind, l, kernel_choice)
  test_statistic <- test_stat(k, X, Y, nn_ind, l, kernel_choice)
  normalized_test_stat <- sqrt(k * n) * test_statistic / sqrt(asy_variance)

  # compute the p-value
  p_value <- 2*(1 - pnorm(abs(normalized_test_stat)))

  # return the normalized test statistic and the p-value
  return(list(test_stat = normalized_test_stat,
              p_value = p_value))
}


#' Derandomized test
#'
#' @param data Same as df_test
#' @param k Same as df_test
#' @param hyper Same as df_test
#' @param kernel_choice Same as df_test
#' @param num_derandom Number of derandomized sample used
#' @param resampling_dist Same as df_test
#' @param resamp_hyper Same as df_test
#'
#' @return Nomalized test statistic and p-value
#' @export

derandomized_test <- function(data, k, hyper = NULL,
                              kernel_choice = "Linear",
                              num_derandom = 300,
                              resampling_dist = "Gaussian",
                              resamp_hyper = list(mean = 0, sd = 1)){

  # extract data
  Y <- data$Y
  Z <- data$Z
  n <- nrow(as.matrix(Y))

  # M resample from the distribution
  Y_resam <- lapply(1:num_derandom, function(m){
    switch (resampling_dist,
            Gaussian = {
              as.matrix(rnorm(n, mean = resamp_hyper$mean, sd = resamp_hyper$sd))
            },
            Binomial = {
              as.matrix(rbinom(n, 1, prob = resamp_hyper$mean))
            }
    )
  })

  # compute the p-value
  ## compute the nearest neighbor (exclude the first column)
  nn_ind <- nn2(data = Z, query = Z, k = k + 1)$nn.idx[, -1]

  # compute the test statistic
  asy_variance <- asy_var(k, X = Y, Y = Y_resam, nn_ind, l = NULL, kernel_choice, num_derandom = num_derandom)
  test_statistic <- test_stat(k, X = Y, Y = Y_resam, nn_ind, l = NULL, kernel_choice, num_derandom = num_derandom)
  normalized_test_stat <- sqrt(k * n) * test_statistic / sqrt(asy_variance)

  # compute the p-value
  p_value <- 2*(1 - pnorm(abs(normalized_test_stat)))

  # return the normalized test statistic and the p-value
  return(list(test_stat = normalized_test_stat,
              p_value = p_value))
}


#' Distribution-free finite-sample valid test
#'
#' @param data Data containing observed sample Y and covariate Z
#' @param k Number of nearest neighbors
#' @param hyper Hyperparameter used in kernel function
#' @param kernel_choice Choice of kernel
#' @param M Number of resamples
#' @param resampling_dist Resampling distribution
#' @param resamp_hyper Hyperparameter in the resampling distribution
#'
#' @return P-value
#' @export

df_test <- function(data, k, hyper = NULL,
                    kernel_choice = "Linear",
                    M = 300,
                    resampling_dist = "Gaussian",
                    resamp_hyper = list(mean = 0, sd = 1)){

  # extract data
  Y <- data$Y
  Z <- data$Z
  n <- nrow(as.matrix(Y))

  # M resample from the distribution
  Y_resam <- lapply(1:(M+1), function(m){
    switch (resampling_dist,
      Gaussian = {
        as.matrix(rnorm(n, mean = resamp_hyper$mean, sd = resamp_hyper$sd))
      },
      Binomial = {
        as.matrix(rbinom(n, 1, prob = resamp_hyper$mean))
      }
    )
  })
  refer_sample <- Y_resam[[1]]

  # put the observed outcome to the first position in Y_resam
  Y_resam[[1]] <- Y

  # compute the p-value
  ## compute the nearest neighbor (exclude the first column)
  nn_ind <- nn2(data = Z, query = Z, k = k + 1)$nn.idx[, -1]

  ## compute the statistic
  null_set <- sapply(1:(M+1), function(m){
    # specify the hyperparameter for Gaussian kernel function
    if(kernel_choice == "Gaussian"){
      l <- switch (hyper,
                   NULL = {
                     median(sqrt(apply(as.matrix((Y_resam[[m]] - refer_sample)^2), 1, sum)))
                   },
                   median_diff = {
                     median(sqrt(apply(as.matrix((Y_resam[[m]] - refer_sample)^2), 1, sum)))
                   }
      )
    }
    return(test_stat(k, refer_sample, Y_resam[[m]], nn_ind, l, kernel_choice = kernel_choice, num_derandom = 1))
  })

  # return the p-value
  return(p_value = mean(abs(null_set) >= abs(null_set[1])))
}


#' KCSD test (for conditional goodness-of-fit test)
#'
#' @param data Data consisting observed outcome Y and conditional covariate X
#' @param mu Mean function x
#' @param sigma Standard deviation function x
#' @param M Number of bootstrap samples
#'
#' @return Bootstrap-based p-value
#' @export

KCSD_test <- function(data, mu, sigma, M = 200){
  Y <- data$Y
  X <- data$X
  n <- nrow(X)

  # compute the median of data distance in X and Y
  distances_X <- dist(X)
  distances_Y <- dist(as.data.frame(Y))

  # Convert to vector
  dist_vector_X <- as.vector(distances_X)
  dist_vector_Y <- as.vector(distances_Y)

  # Compute median
  lx <- median(dist_vector_X)
  ly <- median(dist_vector_Y)

  # compute kernel matrix
  kernel_mat <- sapply(1:n, function(i){
    sapply(1:n, function(j){
      kernel_Ustat(x1 = X[i, ], x2 = X[j, ], y1 = Y[i], y2 = Y[j], lx, ly, mu, sigma)
    })
  })

  # compute the observed test statistic
  obs_test <- (sum(kernel_mat) - sum(diag(kernel_mat))) / (n-1)

  # bootstrap sample
  boot_resample <- sapply(1:M, function(m){
    W <- rnorm(n)
    t(as.matrix(W)) %*% (kernel_mat / n) %*% W - sum(diag(kernel_mat / n))
  })

  # return p-value
  return(p_value = (1 + sum(boot_resample >= obs_test)) / (M + 1))
}

#' SKCE test (for classification model calibration test)
#'
#' @param data Data consisting predictor and response matrix
#' @param B Number of bootstrap
#'
#' @return P-value
#' @export
#' @import kernlab

SKCE_classification_test <- function(data, B = 300){

  # extract predictor and response
  predictor <- data$predictor
  response <- data$response

  # compute observed test statistic
  test_stat <- SKCE_classification(predictor = predictor, response = response,
                                   gamma = 1)
  stats_list <- c(test_stat,
                  bootstrap_SKCE_classification(predictor = predictor,
                                                gamma = 1, B = B))

  # return p-value
  return(p_value = sum(stats_list >= test_stat) / (B + 1))
}


#' SKCE test for regression calibration
#'
#' @param data Pseduo data
#' @param B Number of bootstrap
#'
#' @return P-value
#' @export

SKCE_regression_test <- function(data, B = 300){

  # compute observed test statistic
  test_stat <- SKCE_regression(pseduo_data = data,
                               gamma = 1)
  stats_list <- c(test_stat,
                  bootstrap_SKCE_regression(pseduo_data = data, gamma = 1, B = B))

  # return p-value
  return(p_value = sum(stats_list >= test_stat) / (B + 1))
}

