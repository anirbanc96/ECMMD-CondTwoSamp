library(NNCDT)
library(dplyr)
set.seed(1)

# Heteroscedastic Gaussian Model(HGM)
variance <- function(x, rho){
  1 + rho*exp(-sum((x-0.8)^2) / (2*0.8^2))
}

# an example on classfication
B <- 500
M <- 300
d <- 3
N_test <- seq(50, 250, by = 50)
rho_list <- c(0, 10)
method_mat <- data.frame(
  method_list = c("KCSD", "derandomized_NN20", "derandomized_NN40", "df_NN20", "df_NN40"),
  value = c(0, rep(c(20, 40), 2))
)


# loop over rho_list
for (signal in rho_list) {
  
  # create empty array
  p_value <- array(NA, dim = c(length(method_mat$method_list), B, length(N_test)),
                   dimnames = list(method = method_mat$method_list,
                                   reps = 1:B, N = N_test))

  # loop over N_test and B
  for (n in N_test) {
    for (b in 1:B) {
      # generate X
      X <- matrix(rnorm(n*d), nrow = n, ncol = d)

      # sample M resample according to the density
      Y <- rnorm(n, mean = apply(X, 1, sum), sd = 1)

      # loop over method
      for (method in method_mat$method_list) {
        if (method != "KCSD"){
          k <- method_mat |>
            filter(method_list == method) |>
            dplyr::select(value) |>
            pull()
          
          # split the string 
          method_type <- strsplit(method, "_")[[1]][1]
          
          if(method_type == "derandomized"){
            p_value[method, b, as.character(n)] <- derandomized_test(data = list(Y = Y,
                                                                                 Z = X),
                                                                     k = k, hyper = "median_diff",
                                                                     kernel_choice = "Gaussian",
                                                                     num_derandom = 20,
                                                                     resampling_dist = "Gaussian",
                                                                     resamp_hyper = list(mean = apply(X, 1, sum),
                                                                                         sd = apply(X, 1,
                                                                                                    function(x) sqrt(variance(x, rho = signal)))))$p_value
          }else{
            p_value[method, b, as.character(n)] <- df_test(data = list(Y = Y,
                                                                       Z = X),
                                                           k = k, hyper = "median_diff",
                                                           kernel_choice = "Gaussian",
                                                           M = 200,
                                                           resampling_dist = "Gaussian",
                                                           resamp_hyper = list(mean = apply(X, 1, sum),
                                                                               sd = apply(X, 1,
                                                                                          function(x) sqrt(variance(x, rho = signal)))))
          }
          
        }else{
          p_value["KCSD", b, as.character(n)] <- KCSD_test(data = list(Y = Y, X = X),
                                                           mu = function(x) sum(x),
                                                           sigma = function(x) sqrt(variance(x, rho = signal)),
                                                           M = 500)
        }
      }
      
      # power
      print(p_value[, b, as.character(n)])
    }
  }

  # save rds file
  saveRDS(p_value, sprintf("Rho%d.rds", signal))
}












#
#
#
#
# # Quadratic Gaussian Model (QGM)
# B <- 200
# N_test <- seq(100, 1000, by = 300)
# for (aa in 1:length(N_test)) {
#   l <- seq(1, 21, by = 5)
#   Test <- matrix(0, nrow = B, ncol = length(l))
#   for (r in 1:B) {
#     # a naive simulation study for conditional two sample test
#     set.seed(r)
#     n <- N_test[aa]
#     d <- 1
#     k <- round(n / log(n))
#     X <- matrix(runif(n*d, -2, 2), nrow = n, ncol = d)
#     Y <- rnorm(n, mean = 0.1*X^2 + X + 1, sd = 1)
#     Y_sam <- rnorm(n, mean = X + 1, sd = 1)
#     # l <- median(as.vector(dist(as.matrix(c(Y, Y_sam))))^2) / 2
#
#     for (i in 1:length(l)) {
#       # find the nearest neighbor
#       nn_ind <- nn2(data = X, query = X, k = k)$nn.idx
#
#       # compute the test statistic
#       Test[r, i] <- sqrt((k - 1)*n) *
#         test_stat(k, Y_sam, Y, nn_ind, l[i]) / sqrt(asy_var(k, Y_sam, Y, nn_ind, l[i]))
#     }
#     # print (r)
#   }
#
#   # power
#   print(apply(Test, 2, function(x) mean(abs(x) > 1.96)))
# }
#
#
