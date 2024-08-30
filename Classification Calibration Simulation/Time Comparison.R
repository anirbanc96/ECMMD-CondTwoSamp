library(NNCDT)
library(LaplacesDemon)
library(dplyr)
library(MCMCpack)
set.seed(1)

# hyperparameter
B <- 500
n_list <- c(100)
mag <- seq(0.1)
method_mat <- data.frame(
  method_list = c("SKCE", "derandomized_NN15", "derandomized_NN25", "asym_NN15", "asym_NN25"),
  value = c(0, rep(c(15, 25), 2))
)

# loop over sample size
for (n in n_list) {
  time_mat <- matrix(NA, nrow = B, ncol = nrow(method_mat),
                     dimnames = list(
                       reps = 1:B,
                       method = method_mat$method_list
                     ))
  
  # loop over magnitude
  for (signal in mag) {
    # loop over realization
    for (b in 1:B) {
      
      # specify the predictor probability
      predictor <- rdirichlet(n, c(signal, 1 - signal))
      X <- rbinom(n, 1, predictor[, 1]) # specify a model for Y
      Y <- rbinom(n, 1, predictor[, 1])
      
      # loop over method_list
      for (method in method_mat$method_list) {
        if (method != "SKCE"){
          k <- method_mat |>
            filter(method_list == method) |>
            dplyr::select(value) |>
            pull()
          
          # split the string 
          method_type <- strsplit(method, "_")[[1]][1]
          
          if(method_type == "derandomized"){
            time_mat[b, method] <- system.time(derandomized_test(data = list(Y = Y,
                                                                             Z = predictor),
                                                                 k = k, hyper = NULL,
                                                                 kernel_choice = "Linear",
                                                                 num_derandom = 20,
                                                                 resampling_dist = "Binomial",
                                                                 resamp_hyper = list(mean = predictor[, 1]))$p_value)[[1]]
          }else{
            time_mat[b, method] <- system.time(asy_test(data = list(X = X,
                                                                    Y = Y,
                                                                    Z = predictor),
                                                        k = k, hyper = NULL,
                                                        kernel_choice = "Linear")$p_value)[[1]]
          }
          
        }else{
          
          # create a two-dimensional response vector
          response <- matrix(0, 2, n)
          response[1, which(Y != 0)] <- 1
          response[2, which(Y == 0)] <- 1
          data <- list(predictor = predictor, response = t(response))
          time_mat[b, method] <- system.time(SKCE_classification_test(data, B = 300))[[1]]
        }
      }
      # Type-I error
      print(time_mat[b, ])
    }
  }
  
  # save the result
  saveRDS(time_mat,
          sprintf("Time%d.rds", n))
  
}
