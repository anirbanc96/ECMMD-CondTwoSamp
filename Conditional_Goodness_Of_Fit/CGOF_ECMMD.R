# define Gaussian kernel
k_G <- function(X, Y, bandwidth){
  exp(-(X - Y)^2 / bandwidth)
}

# define test statistic
test_stat <- function(k, X, Y, nn_ind, l){
  n <- length(X)
  T1 <- sapply(1:n, function(i){sum(k_G(X[rep(i, k - 1)], X[nn_ind[i,-1]], l)) / (k - 1)})
  T2 <- sapply(1:n, function(i){sum(k_G(Y[rep(i, k - 1)], Y[nn_ind[i,-1]], l)) / (k - 1)})
  T3 <- sapply(1:n, function(i){sum(k_G(X[rep(i, k - 1)], Y[nn_ind[i,-1]], l)) / (k - 1)})
  T4 <- sapply(1:n, function(i){sum(k_G(Y[rep(i, k - 1)], X[nn_ind[i,-1]], l)) / (k - 1)})
  sum(T1+T2-T3-T4)/n
}

# define the variance function
f_variance <- function(x, rho = 0){
  
  return (1 + rho*exp(-sum((x-0.8)^2) / (2*0.8^2)))
  
}

knn_p_value.B_r <- function(r, k, B, M, n, rho, 
                            test_type = "alternative", dim = 3){
  
  set.seed(r)
  
  Z <- matrix(rnorm(n*dim), nrow = n, ncol = dim)
  
  Y <- matrix(rnorm(n*(M+1), mean = apply(Z, 1, sum),
                    sd = apply(Z, 1, function(x){sqrt(f_variance(x, rho = rho))})),
              nrow = n, ncol = M+1)
  reference_sample <- Y[, (M+1)]
  
  if (test_type == "alternative"){
    
    Y[, (M+1)] <- rnorm(n, mean = apply(Z, 1, sum), 
                        sd = 1)
    
  }
  else{
    
    Y[, (M+1)] <- rnorm(n, mean = apply(Z, 1, sum), 
                        sd = apply(Z, 1, function(x){sqrt(f_variance(x, rho = rho))}))
    
  }
  
  nn_ind <- nn2(data = Z, query = Z, k = k)$nn.idx
  
  null_set <- apply(Y, 2, function(x){
    
    l <- median(abs(x - reference_sample))^2
    test_stat(k, reference_sample, x, nn_ind, l)
    
  })
  
  return (mean(null_set >= null_set[(M+1)]))
  
}

library(foreach)
library(doParallel)

cores=detectCores()
cl <- makeCluster(cores[1]-1)

clusterExport(cl, c("f_variance", "k_G", "knn_p_value.B_r", "test_stat"))
registerDoParallel(cl)

knn_p_value <- function(k, B, M, N_list, rho, test_type, dim = 3){
  
  p_value_mat <- matrix(0, B, length(N_list))
  
  writeLines(c("Starting Experiment with :", test_type, " KNN :", k, "\n"), "log.txt")
  
  for (i in 1:length(N_list)){
    
    p_value_mat[, i] <- foreach(r=1:B, .combine=rbind, .packages = c("RANN")) %dopar% {
      
      output <- knn_p_value.B_r(r, k, B, M, N_list[i], 
                                rho, test_type)
      
      cat(paste("Finished iteration ", r, " with Sample Size ", N_list[i], "\n"), file = "log.txt", append = T)
      
      output
    }
    
  }
  
  return (p_value_mat)
  
}

################################################################################

B <- 200
M <- 100
N_list <- c(50, 100, 150, 200, 250, 300)
K <- c(10, 20, 30, 40)
rho <- 10
test_type_list <- c("null", "alternative")

for (test_type in test_type_list){
  
  p_value <- list(NN_10 = matrix(0, B, length(N_list)),
                  NN_25 = matrix(0, B, length(N_list)),
                  NN_40 = matrix(0, B, length(N_list)))
  
  p_value <- lapply(1:length(K),
                    function(x){knn_p_value(K[x], B, M, N_list, rho, test_type)})
  
  print (test_type)
  
  for (i in 1:length(K)){
    
    print (apply(p_value[[i]], 2, function(x){mean(x<=0.05)}))
    
  }
  
  # save rds file
  saveRDS(p_value,
          sprintf("CGOF_ECMMD_%s.rds",
                  test_type))
  
}

TypeI_pvalues <- readRDS("CGOF_ECMMD_null.rds")
Power_pvalues <- readRDS("CGOF_ECMMD_alternative.rds")

TypeI <- matrix(0, nrow = length(K), ncol = length(N_list))
Power <- matrix(0, nrow = length(K), ncol = length(N_list))

for (i in 1:length(K)){
  
  TypeI[i, ] <- apply(TypeI_pvalues[[i]], 2, function(x){mean(x<=0.05)})
  Power[i, ] <- apply(Power_pvalues[[i]], 2, function(x){mean(x<=0.05)})
  
}

colnames(TypeI) <- N_list; colnames(Power) <- N_list

TypeI <- cbind(K, TypeI); Power <- cbind(K, Power)

write.csv(as.data.frame(TypeI), "CGOF_ECMMD_TypeI.csv")
write.csv(as.data.frame(Power), "CGOF_ECMMD_Power.csv")
