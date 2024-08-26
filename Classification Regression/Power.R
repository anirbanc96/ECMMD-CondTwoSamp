library(NNCDT)
library(dplyr)

## calibration for regression
set.seed(1)
B <- 500
n_train <- 200
mag <- seq(0.2, 1,  by = 0.2)
n_list <- c(50, 75)
method_mat <- data.frame(
  method_list = c("SKCE", "derandomized_NN2", "derandomized_NN4", "asym_NN2", "asym_NN4"),
  value = c(0, rep(c(2, 4), 2))
)


# loop over test size
for (n_test in n_list) {

  # create empty array
  p_value <- array(NA, dim = c(length(method_mat$method_list), B, length(mag)),
                   dimnames = list(method = method_mat$method_list,
                                   reps = 1:B, signal = mag))

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
        
        if (method != "SKCE"){

          # obtain data for proposed method
          test_data <- data[(n_train + 1):n_total, ]
          prediction <- predict(ols_fit, newdata = test_data)
          sampled_data <- rnorm(n_test, mean = prediction, sd = sd(ols_fit$residuals))

          # specify the test data
          Z <- cbind(prediction, rep(sd(ols_fit$residuals), nrow(test_data)))
          X <- test_data$output
          Y <- sampled_data

          # specify the number of nearest neighbors
          k <- method_mat |> filter(method_list == method) |>
            dplyr::select(value) |> pull()
          
          # split the string 
          method_type <- strsplit(method, "_")[[1]][1]
          
          if(method_type == "derandomized"){
            p_value[method, b, as.character(signal)] <- derandomized_test(data = list(Y = X,
                                                                                      Z = Z),
                                                                          k = k, hyper = "median_diff",
                                                                          kernel_choice = "Gaussian",
                                                                          num_derandom = 20,
                                                                          resampling_dist = "Gaussian",
                                                                          resamp_hyper = list(mean = Z[, 1],
                                                                                              sd = Z[, 2]))$p_value
          }else{
            p_value[method, b, as.character(signal)] <- asy_test(data = list(X = X,
                                                                             Y = Y,
                                                                             Z = Z),
                                                                 k = k, hyper = "median_diff",
                                                                 kernel_choice = "Gaussian")$p_value
          }
          
        }else{

          # obtain data for SKCE method
          test_data <- data[(n_train + 1):n_total, ]
          pseduo_data <- data.frame(y = test_data$output,
                                    px_mean = unname(predict(ols_fit, newdata = test_data)),
                                    px_sd = unname(rep(sd(ols_fit$residuals), nrow(test_data))))

          # apply SKCE method
          p_value["SKCE", b, as.character(signal)] <- SKCE_regression_test(data = pseduo_data,
                                                                           B = 300)
        }
      }
      print(p_value[, b, as.character(signal)])
    }
  }

  saveRDS(p_value,
          sprintf("Power%d.rds", n_test))

}
