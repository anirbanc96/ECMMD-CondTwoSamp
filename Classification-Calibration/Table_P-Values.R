# This is a Rscript for calibration plot
library(readr)
library(ggplot2)
library(knitr)
library(kableExtra)
# upload the bird-cat model
B <- 100
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
K_list <- seq(40, 100, 20)
for (j in 1:ncol(name_mat)) {
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
  
  # Your table data
  results <- as.data.frame(
    lapply(data.frame(
      Test = c(c("Finite Sample", "Finite Sample", "Finite Sample", "Finite Sample"), 
                  c("Asymptotic", "Asymptotic", "Asymptotic", "Asymptotic"),
                  "ECE"),
      K = c(rep(seq(40, 100, 20), 2), ""),
      Before = c(sapply(c(f_test$p_value,
                                     asymp_test$p_value), round, 3),
                            round(ECE, 3)),
      After = c(sapply(c(f_calibrated_test$p_value,
                                    asymp_calibrated_test$p_value), round, 3),
                           round(ECE_calibrated,3))
    ), format)
  )
  
  # Create the kable
  kable_output <- kable(results, "latex", booktabs = T, escape = FALSE, align = c('c', 'c', 'c', 'c')) %>%
                        # caption = sprintf("Calibration results for %s-%s classification", 
                        #                   class_a, 
                        #                   class_b)) %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    row_spec(nrow(results)) %>%
    add_header_above(c(" " = 1, " " = 1, "P-values" = 2))
  
  print (results)
  
  # save latex file
  writeLines(kable_output, sprintf("Figures/%s_%s_result.tex",
                                   class_a, class_b))
}

