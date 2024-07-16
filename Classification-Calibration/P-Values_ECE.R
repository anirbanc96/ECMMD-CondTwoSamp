# This is a Rscript for real data analysis for CNN model trained for CIFAR 10 
library(readr)
source("Test_Var_Stat.R")
source("Tests_ECE_Isotonic_Reliability.R")
set.seed(42)
# load the data and perform the test
## bird & cat
name_mat <- matrix(c("bird", "cat", 
                     "cat", "dog",
                     "cat", "deer",
                     "cat", "frog",
                     "cat", "horse"), nrow = 2, ncol = 5)

index_df <- data.frame(
  bird = 2,
  cat = 3,
  dog = 4,
  deer = 5,
  frog = 6,
  horse = 7
)

k_1 = 41; k_2 = 101
derandom_M <- 100
B <- 200


for (j in 1:ncol(name_mat)) {
  class_a <- name_mat[1, j]
  class_b <- name_mat[2, j]
  index_a <- index_df[, class_a]
  index_b <- index_df[, class_b]
  pred <- read_csv(sprintf("Predictions_TestLabels/predictions_%d%d.csv", index_a, index_b), 
                   col_names = c(class_a, class_b))
  label <- read_csv(sprintf("Predictions_TestLabels/test_labels_%d%d.csv", index_a, index_b), 
                    col_names = c(class_a, class_b))
  
  # separate data for calibration and test
  calibration_id <- sample(1:nrow(pred), 2*nrow(pred) / 3)
  test_id <- setdiff(1:nrow(pred), calibration_id)
  pred_calibration <- pred[calibration_id, ]
  pred_test <- pred[test_id, ]
  label_calibration <- label[calibration_id, ]
  label_test <- label[test_id, ]
  
  
  calibrated_prob <- isotonic_calibration(y = as.vector(unlist(label_calibration[, 1])),
                                          p = as.vector(unlist(pred_calibration[, 1])),
                                          p_test = as.vector(unlist(pred_test[, 1])))
  
  
  ECE <- compute_ECE(predicted_probs = as.vector(unlist(pred_test[, 1])), 
                     true_labels = as.vector(unlist(label_test[, 1])), 
                     n_bins = 100)
  
  print(2*ECE)
  
  test_result <- asymp_test(pred = as.vector(unlist(pred_test[, 1])),
                                  label = as.vector(unlist(label_test[, 1])), 
                                  K = seq(k_1, k_2, 20))
  
  print(test_result)
  
  # derandom_test_result <- derandomized_test(pred = as.vector(unlist(pred_test[, 1])),
  #                                           label = as.vector(unlist(label_test[, 1])),
  #                                           K = seq(k_1, k_2, 20), M = derandom_M)
  # print (derandom_test_result)
  
  df_test_result <- finite_sample_test(pred = as.vector(unlist(pred_test[, 1])),
                            label = as.vector(unlist(label_test[, 1])),
                            K = seq(k_1, k_2, 20), B = B)

  print(df_test_result)
  
  ECE_calibrated <- compute_ECE(predicted_probs = calibrated_prob, 
                                true_labels = as.vector(unlist(label_test[, 1])), 
                                n_bins = 100)
  
  print(2*ECE_calibrated)
  
  
  test_result_calibrated <- asymp_test(pred = calibrated_prob, 
                                             label = as.vector(unlist(label_test[, 1])), 
                                             K = seq(k_1, k_2, 20))
  
  print(test_result_calibrated)
  
  # derandom_test_result_calibrated <- derandomized_test(pred = calibrated_prob,
  #                                                      label = as.vector(unlist(label_test[, 1])),
  #                                                      K = seq(k_1, k_2, 20), M = derandom_M)
  # print (derandom_test_result_calibrated)
  
  df_test_calibrated_result <- finite_sample_test(pred = calibrated_prob,
                                        label = as.vector(unlist(label_test[, 1])),
                                        K = seq(k_1, k_2, 20), B = B)

  print(df_test_calibrated_result)
  
  saveRDS(2*ECE,
          file = sprintf("Results/%s_%s_ECE.rds",
                         class_a,
                         class_b))
  saveRDS(test_result,
          file = sprintf("Results/%s_%s_asymp_test.rds",
                         class_a,
                         class_b))

  # saveRDS(derandom_test_result,
  #         file = sprintf("Results/%s_%s_derandomized_test.rds",
  #                        class_a,
  #                        class_b))
  #
  saveRDS(df_test_result,
          file = sprintf("Results/%s_%s_finite_sample_test.rds",
                         class_a,
                         class_b))
  saveRDS(2*ECE_calibrated,
          file = sprintf("Results/%s_%s_calibrated_ECE.rds",
                         class_a,
                         class_b))

  saveRDS(test_result_calibrated,
          file = sprintf("Results/%s_%s_calibrated_asymp_test.rds",
                         class_a,
                         class_b))

  # saveRDS(derandom_test_result_calibrated,
  #         file = sprintf("Results/%s_%s_calibrated_derandomized_test.rds",
  #                        class_a,
  #                        class_b))
  #
  saveRDS(df_test_calibrated_result,
          file = sprintf("Results/%s_%s_calibrated_finite_sample_test.rds",
                         class_a,
                         class_b))
  print(sprintf("%s_%s: finished", class_a, class_b))
}

