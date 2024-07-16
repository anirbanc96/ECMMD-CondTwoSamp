# This is a Rscript for calibration plot
library(readr)
library(ggplot2)
library(tibble)
source("Tests_ECE_Isotonic_Reliability.R")
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

name_mat_upper <- matrix(c("Bird", "Cat", 
                     "Cat", "Dog",
                     "Cat", "Deer",
                     "Cat", "Frog",
                     "Cat", "Horse"), nrow = 2, ncol = 5)



index_df <- data.frame(
  bird = 2,
  cat = 3,
  dog = 4,
  deer = 5,
  frog = 6,
  horse = 7
)

for (j in 1:ncol(name_mat)) {
  class_a <- name_mat[1, j]
  class_b <- name_mat[2, j]
  class_a_u <- name_mat_upper[1, j]
  class_b_u <- name_mat_upper[2, j]
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
  
  # calibration result
  calibrated_prob <- isotonic_calibration(y = as.vector(unlist(label_calibration[, 1])),
                                          p = as.vector(unlist(pred_calibration[, 1])),
                                          p_test = as.vector(unlist(pred_test[, 1])))
  
  
  # # Using the function to compute values
  # predicted_probs <- runif(1000)  # Random probabilities for example
  
  df1 <- data_reliability_diagram(as.vector(unlist(pred_test[, 1])), 
                                  as.vector(unlist(label_test[, 1])), 
                                  n_bins=7)
  df2 <- data_reliability_diagram(calibrated_prob, 
                                  as.vector(unlist(label_test[, 1])), 
                                  n_bins=7)
  
  cols <- c("Before Calibration"="dodgerblue","After Calibration"="coral")
  p <- ggplot(df1, aes(x = bin_accuracies, y = bin_confs)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", alpha = 0.4) +
    geom_line(aes(color = "Before Calibration")) +
    geom_point(size=2, color = "navy") +  # Plot actual accuracies based on observation count
    geom_line(data = df2, aes(x = bin_accuracies, y = bin_confs, color = "After Calibration")) +
    geom_point(data = df2, aes(x = bin_accuracies, y = bin_confs), size=2, color = "darkred") +
    labs(x = "Bin Frequency", y = "Average Predicted Probability",
         title = sprintf("Reliability Diagram for %s-%s Classification", class_a_u, class_b_u)) +
    scale_colour_manual(name = element_blank(), values=cols) +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.25,0.85),
          legend.background = element_rect(color = 1),
          plot.title = element_text(size = 12, hjust = 0.5))
  
  print (p)
  
  ggsave(sprintf("Figures/%s_%smiscalibration.pdf",
                 class_a, class_b),
         plot = p,
         height = 4,
         width = 5)
  
}

