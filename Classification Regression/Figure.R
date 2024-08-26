# This is a Rscript for regression calibration
library(ggplot2)
library(tibble)
library(dplyr)
library(latex2exp)
n_list <- c(50, 75)
alpha <- 0.05


for (n in n_list) {

  # load the result
  data <- readRDS(sprintf("Power%d.rds", n))

  # summarise the type-I error or rejection rate
  data_to_plot <- as_tibble(as.table(data)) |>
    mutate(p_value = n) |>
    dplyr::select(-n) |>
    group_by(method, signal) |>
    summarise(rejection_rate = mean(p_value <= alpha)) |>
    ungroup() |>
    mutate(method = factor(as.factor(method),
                           levels = c("SKCE", "asym_NN2", "asym_NN4", 
                                      "derandomized_NN2", "derandomized_NN4")))


  # plot
  my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=11))
  pannel_cols <- c("coral", "darkorchid4", "dodgerblue4", "dodgerblue3",
                   "dodgerblue1", "deepskyblue")
  title_string <- sprintf('Calibration test for regression model with $n=%d$', n)

  # ggplot
  plot_to_save <- data_to_plot |>
    ggplot(aes(x = signal, y = rejection_rate, color = method, group = method)) +
    geom_line() +
    geom_point(size = 2) +
    labs(x = TeX(r"($\rho$)"),
         y = "Empirical Power") +
    my_theme +
    theme(legend.background = element_rect(color = 1),
          legend.position=c(0.8, 0.3),
          legend.title = element_blank()) +
    scale_color_manual(labels = c("SKCE", "2 NN (asymp)","4 NN (asymp)",
                                  "2 NN (derandom)","4 NN (derandom)"), values = pannel_cols) +
    guides(color = guide_legend(
      keywidth = 0.0,
      keyheight = 0.2,
      default.unit = "inch",
      override.aes = list(size = 2.5))) +
    ggtitle(TeX(title_string))

  # save the plot
  ggsave(sprintf("Power%d.pdf", n),
         plot = plot_to_save,
         width = 5,
         height = 4)

}
