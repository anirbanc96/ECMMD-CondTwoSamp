library(NNCDT)
library(dplyr)

## calibration for regression
set.seed(1)
B <- 500
n_train <- 200
mag <- 0.2
n_list <- c(75)
method_mat <- data.frame(
  method_list = c("SKCE", "derandomized_NN2", "derandomized_NN4", "asym_NN2", "asym_NN4"),
  value = c(0, rep(c(2, 4), 2))
)

# loop over test size
for (n_test in n_list) {
  
  # create empty array
  time_mat <- matrix(NA, nrow = B, ncol = length(method_mat$method_list),
                     dimnames = list(reps = 1:B, method = method_mat$method_list))
  
  # loop over magnitude
  for (signal in mag) {
    # loop over realization
    for (b in 1:B) {
      n_total <- n_train + n_test
      input <- runif(n_train + n_test, -1, 1)
      output <- signal*sin(pi*input) + abs(1+input)*rnorm(length(input), 0, 0.15)
      data <- data.frame(input = input,
                         output = output)
      
      # ols estimate
      ols_fit <- lm(output ~ input, data = data[1:n_train, ])
      
      # loop over method_list
      for (method in method_mat$method_list) {
        # obtain data for proposed method
        test_data <- data[(n_train + 1):n_total, ]
        prediction <- predict(ols_fit, newdata = test_data)
        sampled_data <- rnorm(n_test, mean = prediction, sd = sd(ols_fit$residuals))
        sampled_data_control <- rnorm(n_test, mean = prediction, sd = sd(ols_fit$residuals))
        
        if (method != "SKCE"){
          
          # specify the test data
          Z <- cbind(prediction, rep(sd(ols_fit$residuals), nrow(test_data)))
          X <- sampled_data_control
          Y <- sampled_data
          
          # specify the number of nearest neighbors
          k <- method_mat |> filter(method_list == method) |>
            dplyr::select(value) |> pull()
          
          # split the string 
          method_type <- strsplit(method, "_")[[1]][1]
          
          if(method_type == "derandomized"){
            time_mat[b, method] <- system.time(derandomized_test(data = list(Y = X,
                                                                             Z = Z),
                                                                 k = k, hyper = "median_diff",
                                                                 kernel_choice = "Gaussian",
                                                                 num_derandom = 20,
                                                                 resampling_dist = "Gaussian",
                                                                 resamp_hyper = list(mean = Z[, 1],
                                                                                     sd = Z[, 2]))$p_value)[[1]]
          }else{
            time_mat[b, method] <- system.time(asy_test(data = list(X = X,
                                                                    Y = Y,
                                                                    Z = Z),
                                                        k = k, hyper = "median_diff",
                                                        kernel_choice = "Gaussian")$p_value)[[1]]
          }
          
        }else{
          
          # obtain data for SKCE method
          pseduo_data <- data.frame(y = sampled_data,
                                    px_mean = unname(predict(ols_fit, newdata = test_data)),
                                    px_sd = unname(rep(sd(ols_fit$residuals), nrow(test_data))))
          
          # apply SKCE method
          time_mat[b, method] <- system.time(SKCE_regression_test(data = pseduo_data,
                                                                  B = 300))[[1]]
        }
      }
      print(time_mat[b, ])
    }
  }
  
  saveRDS(time_mat, sprintf("Time%d.rds", n_test)) 
  
}

