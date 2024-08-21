#' Loading libraries and auxillary scripts
require(readr)
require(knitr)
require(kableExtra)
source("ECMMD.R")
source("AuxillaryFunctions.R")

#' Load the data

#' matrix containing named pairs of classes

name_mat <- matrix(c("bird", "cat", 
                     "cat", "dog",
                     "cat", "deer",
                     "cat", "frog",
                     "cat", "horse"), nrow = 2, ncol = 5)

#' dataframe with class index corresponding to each class

index_df <- data.frame(
  bird = 2,
  cat = 3,
  dog = 4,
  deer = 5,
  frog = 6,
  horse = 7
)

#' Test parameters

#' Starting and ending knn values (notice we take k + 1 many knn's since 
#' RANN package includes self as a nearest neighbour)

k_1 = 61; k_2 = 121

by_knn <- 20

knn.vector <- seq(k_1, k_2, by = by_knn)

#' Number of resamples in finite sample test

M <- 500

#' Number of iterations done

n_iter <- 100

#' Storing asymptotic and finite sample test results before 
#' and after re-calibration

asymptotic_test_before <- vector("list", length = n_iter)
fs_test_before <- vector("list", length = n_iter)
asymptotic_test_after <- vector("list", length = n_iter)
fs_test_after <- vector("list", length = n_iter)

#' Store values of ECE before and after re-calibration

ECE_before <- vector("list", length = n_iter)
ECE_after <- vector("list", length = n_iter)

#' Number of random samples takes from test set in each iteration

sampling_size <- 1000

#' Libraries for parallelising over pairs of classes

library(foreach)
library(doParallel)

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#' Run the ECMMD based tests for validating calibration

foreach(j=1:ncol(name_mat), .export = ls(globalenv()), .packages = c("readr", "RANN", "parallel")) %dopar% {
  
  set.seed(42)
  
  class_a <- name_mat[1, j]
  class_b <- name_mat[2, j]
  
  index_a <- index_df[, class_a]
  index_b <- index_df[, class_b]
  
  pred <- read_csv(sprintf("Predictions_TestLabels/predictions_%d%d.csv", index_a, index_b), 
                   col_names = c(class_a, class_b), show_col_types = F)
  label <- read_csv(sprintf("Predictions_TestLabels/test_labels_%d%d.csv", index_a, index_b), 
                    col_names = c(class_a, class_b), show_col_types = F)
  
  #' separate data for re-calibration and test
  
  calibration_id <- sample(1:nrow(pred), 2*nrow(pred) / 3)
  test_id <- setdiff(1:nrow(pred), calibration_id)
  
  pred_calibration <- pred[calibration_id, ]
  label_calibration <- label[calibration_id, ]
  
  pred_calibration <- as.vector(unlist(pred_calibration[, 1]))
  label_calibration <- as.vector(unlist(label_calibration[, 1]))
  
  #' train the isotonic calibration
  
  trained_isotonic <- isotonic.calibration.train(labels = label_calibration,
                                                 probs = pred_calibration)
  
  #' Run the tests for n_iter many iterations
  
  writeLines(c("Classes: ", index_a, ",", index_b, "\n"), "log.txt")
  
  for (iter in 1:n_iter){
    
    cat(paste(c("Starting Iteration: ", iter, "\n")), file = "log.txt", append = T)
    
    set.seed(iter)
    
    test_id_sample <- sample(test_id, sampling_size)
    
    pred_test <- pred[test_id_sample, ]
    label_test <- label[test_id_sample, ]
    
    pred_test <- as.vector(unlist(pred_test[, 1]))
    label_test <- as.vector(unlist(label_test[, 1]))
    
    calibrated_prob <- isotonic.calibration.pred(trained.isotonic = trained_isotonic,
                                                 pred.probs = pred_test)
    
    set.seed(iter)
    
    ECE_before[[iter]] <- 2*compute.ECE(predicted_probs = pred_test, 
                                        true_labels = label_test, 
                                        n_bins = 100)
    
    set.seed(iter)
      
    asymptotic_test_before[[iter]] <- asymptotic.test(predicted.probs = pred_test,
                                                      test.labels = label_test, 
                                                      knn.vector = knn.vector)$p.values
  
    set.seed(iter)
    
    fs_test_before[[iter]] <- finite.sample.test(predicted.probs = pred_test,
                                                 test.labels = label_test, 
                                                 knn.vector = knn.vector, M = M)$p.values
    
    set.seed(iter)
  
    asymptotic_test_after[[iter]] <- asymptotic.test(predicted.probs = calibrated_prob,
                                                     test.labels = label_test, 
                                                     knn.vector = knn.vector)$p.values
    
    set.seed(iter)
  
    fs_test_after[[iter]] <- finite.sample.test(predicted.probs = calibrated_prob,
                                                test.labels = label_test, 
                                                knn.vector = knn.vector, M = M)$p.values
    
    set.seed(iter)
    
    ECE_after[[iter]] <- 2*compute.ECE(predicted_probs = calibrated_prob, 
                                            true_labels = label_test, 
                                            n_bins = 100)
    
    cat(paste(c("Finished Iteration: ", iter, "\n")), file = "log.txt", append = T)
    
  }
  
  #' Save reults of tests and ECE for each pair of classes
  
  saveRDS(ECE_before,
          file = sprintf("Results/%s_%s_ECE.rds",
                         class_a,
                         class_b))
  saveRDS(asymptotic_test_before,
          file = sprintf("Results/%s_%s_asymp_test.rds",
                         class_a,
                         class_b))
  
  saveRDS(fs_test_before,
          file = sprintf("Results/%s_%s_finite_sample_test.rds",
                         class_a,
                         class_b))
  saveRDS(ECE_after,
          file = sprintf("Results/%s_%s_calibrated_ECE.rds",
                         class_a,
                         class_b))
  
  saveRDS(asymptotic_test_after,
          file = sprintf("Results/%s_%s_calibrated_asymp_test.rds",
                         class_a,
                         class_b))
  
  saveRDS(fs_test_after,
          file = sprintf("Results/%s_%s_calibrated_finite_sample_test.rds",
                         class_a,
                         class_b))
  
  print(sprintf("%s_%s: finished", class_a, class_b))
}

#' Prepare table of test reults and ECE values

num_knn <- length(knn.vector)

for (j in 1:ncol(name_mat)){
  
  class_a <- name_mat[1, j]
  class_b <- name_mat[2, j]
  
  ECE <- readRDS(sprintf("Results/%s_%s_ECE.rds", class_a, class_b))
  f_test <- readRDS(sprintf("Results/%s_%s_finite_sample_test.rds", class_a, class_b))
  asymp_test <- readRDS(sprintf("Results/%s_%s_asymp_test.rds", 
                                class_a, class_b))
  ECE_calibrated <- readRDS(sprintf("Results/%s_%s_calibrated_ECE.rds", 
                                    class_a, class_b))
  f_calibrated_test <- readRDS(sprintf("Results/%s_%s_calibrated_finite_sample_test.rds", 
                                       class_a, class_b))
  asymp_calibrated_test <- readRDS(
    sprintf("results/%s_%s_calibrated_asymp_test.rds",
            class_a, class_b))
  before_asymp <- rep(0, num_knn)
  after_asymp <- rep(0, num_knn)
  before_fs <- rep(0, num_knn)
  after_fs <- rep(0, num_knn)
  
  ECE_b <- 0
  ECE_a <- 0
  
  for (i in 1:n_iter){
    
    before_asymp <- before_asymp + (asymp_test[[i]] <= 0.05)/n_iter
    before_fs <- before_fs + (f_test[[i]] <= 0.05)/n_iter
    after_asymp <- after_asymp + (asymp_calibrated_test[[i]] <= 0.05)/n_iter
    after_fs <- after_fs + (f_calibrated_test[[i]] <= 0.05)/n_iter
    
    ECE_b <- ECE_b + ECE[[i]]/n_iter
    ECE_a <- ECE_a + ECE_calibrated[[i]]/n_iter
    
  }
  
  result <- data.frame(Test = c("FS", "FS", "FS", "FS", "Asymp", "Asymp", "Asymp", "Asymp", "ECE"),
                       K = c(rep(seq(k_1-1, k_2-1, by_knn),2), ""),
                       Before = sapply(c(before_fs, before_asymp, ECE_b), round, 2),
                       After = sapply(c(after_fs, after_asymp, ECE_a), round, 2))
  
  kable_output <- kable(result, "latex", booktabs = T, escape = FALSE, align = c('c', 'c', 'c', 'c')) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    row_spec(nrow(result)) %>%
    add_header_above(c(" " = 1, " " = 1, "Rejection Proportion" = 2))
  
  writeLines(kable_output, sprintf("Figures/%s_%s_result.tex",
                                   class_a, class_b))
  print (result)
  
}
