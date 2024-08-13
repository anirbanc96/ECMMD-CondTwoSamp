# This is a Rscript for real data analysis for CNN model trained for CIFAR 10 
library(readr)
library(knitr)
library(kableExtra)
source("Test_Var_Stat.R")
source("Tests_ECE_Isotonic_Reliability.R")
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

k_1 = 61; k_2 = 121
by_knn <- 20
derandom_M <- 100
B <- 500

last_seed <- 100
seed_list <- 1:last_seed

test_result <- vector("list", length = length(seed_list))
df_test_result <- vector("list", length = length(seed_list))
test_result_calibrated <- vector("list", length = length(seed_list))
df_test_calibrated_result <- vector("list", length = length(seed_list))

ECE <- vector("list", length = length(seed_list))
ECE_calibrated <- vector("list", length = length(seed_list))

sampling_size <- 1000

library(foreach)
library(doParallel)

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

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
  
  # separate data for calibration and test
  calibration_id <- sample(1:nrow(pred), 2*nrow(pred) / 3)
  test_id <- setdiff(1:nrow(pred), calibration_id)
  pred_calibration <- pred[calibration_id, ]
  
  label_calibration <- label[calibration_id, ]
  
  
  writeLines(c("Classes: ", index_a, ",", index_b, "\n"), "log.txt")
  
  for (seed in seed_list){
    
    cat(paste(c("Starting Iteration: ", seed, "\n")), file = "log.txt", append = T)
    
    set.seed(seed)
    
    test_id_sample <- sample(test_id, sampling_size)
    
    pred_test <- pred[test_id_sample, ]
    label_test <- label[test_id_sample, ]
    
    calibrated_prob <- isotonic_calibration(y = as.vector(unlist(label_calibration[, 1])),
                                            p = as.vector(unlist(pred_calibration[, 1])),
                                            p_test = as.vector(unlist(pred_test[, 1])))
    
    set.seed(seed)
    
    ECE[[seed]] <- 2*compute_ECE(predicted_probs = as.vector(unlist(pred_test[, 1])), 
                       true_labels = as.vector(unlist(label_test[, 1])), 
                       n_bins = 100)
    
    set.seed(seed)
      
    test_result[[seed]] <- asymp_test(pred = as.vector(unlist(pred_test[, 1])),
                              label = as.vector(unlist(label_test[, 1])), 
                              K = seq(k_1, k_2, by_knn))$p_value
  
    set.seed(seed)
    
    df_test_result[[seed]] <- finite_sample_test(pred = as.vector(unlist(pred_test[, 1])),
                                         label = as.vector(unlist(label_test[, 1])),
                                         K = seq(k_1, k_2, by_knn), B = B)$p_value
    
    set.seed(seed)
  
    test_result_calibrated[[seed]] <- asymp_test(pred = calibrated_prob, 
                                         label = as.vector(unlist(label_test[, 1])), 
                                         K = seq(k_1, k_2, by_knn))$p_value
    
    set.seed(seed)
  
    df_test_calibrated_result[[seed]] <- finite_sample_test(pred = calibrated_prob,
                                                    label = as.vector(unlist(label_test[, 1])),
                                                    K = seq(k_1, k_2, by_knn), B = B)$p_value
    
    set.seed(seed)
    
    ECE_calibrated[[seed]] <- 2*compute_ECE(predicted_probs = calibrated_prob, 
                                    true_labels = as.vector(unlist(label_test[, 1])), 
                                    n_bins = 100)
    
    cat(paste(c("Finished Iteration: ", seed, "\n")), file = "log.txt", append = T)
    
  }
  
  saveRDS(ECE,
          file = sprintf("Results/%s_%s_ECE.rds",
                         class_a,
                         class_b))
  saveRDS(test_result,
          file = sprintf("Results/%s_%s_asymp_test.rds",
                         class_a,
                         class_b))
  
  saveRDS(df_test_result,
          file = sprintf("Results/%s_%s_finite_sample_test.rds",
                         class_a,
                         class_b))
  saveRDS(ECE_calibrated,
          file = sprintf("Results/%s_%s_calibrated_ECE.rds",
                         class_a,
                         class_b))
  
  saveRDS(test_result_calibrated,
          file = sprintf("Results/%s_%s_calibrated_asymp_test.rds",
                         class_a,
                         class_b))
  
  saveRDS(df_test_calibrated_result,
          file = sprintf("Results/%s_%s_calibrated_finite_sample_test.rds",
                         class_a,
                         class_b))
  
  print(sprintf("%s_%s: finished", class_a, class_b))
}

num_knn <- length(seq(k_1, k_2, by_knn))

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
  
  ECE_before <- 0
  ECE_after <- 0
  
  for (i in 1:length(seed_list)){
    
    before_asymp <- before_asymp + (asymp_test[[i]] <= 0.05)/length(seed_list)
    before_fs <- before_fs + (f_test[[i]] <= 0.05)/length(seed_list)
    after_asymp <- after_asymp + (asymp_calibrated_test[[i]] <= 0.05)/length(seed_list)
    after_fs <- after_fs + (f_calibrated_test[[i]] <= 0.05)/length(seed_list)
    
    ECE_before <- ECE_before + ECE[[i]]/length(seed_list)
    ECE_after <- ECE_after + ECE_calibrated[[i]]/length(seed_list)
    
  }
  
  result <- data.frame(Test = c("FS", "FS", "FS", "FS", "Asymp", "Asymp", "Asymp", "Asymp", "ECE"),
                       K = c(rep(seq(k_1-1, k_2-1, by_knn),2), ""),
                       Before = sapply(c(before_fs, before_asymp, ECE_before), round, 2),
                       After = sapply(c(after_fs, after_asymp, ECE_after), round, 2))
  
  kable_output <- kable(result, "latex", booktabs = T, escape = FALSE, align = c('c', 'c', 'c', 'c')) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    row_spec(nrow(result)) %>%
    add_header_above(c(" " = 1, " " = 1, "Rejection Proportion" = 2))
  
  writeLines(kable_output, sprintf("Figures/%s_%s_result.tex",
                                   class_a, class_b))
  print (result)
  
}
