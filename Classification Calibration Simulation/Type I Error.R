library(NNCDT)
library(LaplacesDemon)
library(dplyr)
library(MCMCpack)
set.seed(1) 
B <- 500
n_list <- c(75, 100)
mag <- seq(0.1, 0.5, by = 0.1)
K <- seq(5, 25, by = 5)
method_mat <- data.frame(
  method_list = c("SKCE", "derandomized_NN15", "derandomized_NN25", "asym_NN15", "asym_NN25"),
  value = c(0, rep(c(15, 25), 2))
)

# loop over sample size
for (n in n_list) {
  p_value <- array(NA, dim = c(length(method_mat$method_list), B, length(mag)),
                   dimnames = list(method = method_mat$method_list,
                                   reps = 1:B, signal = mag))

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
            p_value[method, b, as.character(signal)] <- derandomized_test(data = list(Y = Y,
                                                                                      Z = predictor),
                                                                          k = k, hyper = NULL,
                                                                          kernel_choice = "Linear",
                                                                          num_derandom = 20,
                                                                          resampling_dist = "Binomial",
                                                                          resamp_hyper = list(mean = predictor[, 1]))$p_value
          }else{
            p_value[method, b, as.character(signal)] <- asy_test(data = list(X = X,
                                                                             Y = Y,
                                                                             Z = predictor),
                                                                 k = k, hyper = NULL,
                                                                 kernel_choice = "Linear")$p_value
          }
          
        }else{
          
          # create a two-dimensional response vector
          response <- matrix(0, 2, n)
          response[1, which(Y != 0)] <- 1
          response[2, which(Y == 0)] <- 1
          data <- list(predictor = predictor, response = t(response))
          p_value["SKCE", b, as.character(signal)] <- SKCE_classification_test(data, B = 300)
        }
      }
      # Type-I error
      print(p_value[, b, as.character(signal)])
    }
  }

  # save the result
  saveRDS(p_value,
          sprintf("TypeI%d.rds", n))

}
