# This is a Rscript for classification calibration
library(ggplot2)
library(tibble)
library(dplyr)
library(latex2exp)
library(kableExtra)
library(tidyr)
hypothesis_list <- c("Power", "TypeI")
n_list <- c(75, 100)
alpha <- 0.05

for (hypothesis in hypothesis_list) {
  for (n in n_list) {
    # load the result
    data <- readRDS(sprintf("%s%d.rds",hypothesis, n))

    # summarise the type-I error or rejection rate
    data_to_plot <- as_tibble(as.table(data)) |>
      mutate(p_value = n) |>
      dplyr::select(-n) |>
      group_by(method, signal) |>
      summarise(rejection_rate = mean(p_value <= alpha, na.rm = TRUE)) |>
      ungroup() |>
      mutate(method = factor(as.factor(method),
                             levels = c("SKCE", "asym_NN15", "asym_NN25", 
                                        "derandomized_NN15", "derandomized_NN25"))) |>
      arrange(method)


    # plot
    my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=11))
    pannel_cols <- c("coral", "darkorchid4", "dodgerblue4", "dodgerblue3",
                     "dodgerblue1", "deepskyblue")
    title_string <- sprintf('Calibration Test for Classification with $n=%d$', n)

    # table and ggplot
    if(hypothesis == "TypeI"){
      # write a table 
      data_to_table <- data_to_plot |>
        mutate(rejection_rate = round(rejection_rate, 3)) |>
        pivot_wider(names_from = method,
                    values_from = rejection_rate) 
      
      # write_csv(results, "Figures/Classification-calibration/TypeI_error_75_classification.csv")
      
      kable_output <- kable(data_to_table, "latex", booktabs = T, escape = FALSE,
                            align = c('c', 'c', 'c', 'c', 'c', 'c')) %>%
        kable_styling(font_size = 5, latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
        add_header_above(c(" " = 1, "Test" = 2)) %>%
        add_header_above("Type-I Error") %>%
        row_spec(nrow(data_to_table))
      
      # save the table
      writeLines(kable_output, sprintf("TypeI%d.tex", n))
      
    }else{
      plot_to_save <- data_to_plot |>
        ggplot(aes(x = signal, y = rejection_rate, color = method, group = method)) +
        geom_line() +
        geom_point(size = 2) +
        labs(x = TeX(r"($\rho$)"),
             y = "Empirical Power") +
        my_theme +
        theme(legend.background = element_rect(color = 1),
              legend.position=c(0.8, 0.5),
              legend.title = element_blank()) +
        scale_color_manual(labels = c("SKCE", "15 NN (asymp)","25 NN (asymp)",
                                      "15 NN (derandom)","25 NN (derandom)"), values = pannel_cols) +
        guides(color = guide_legend(
          keywidth = 0.0,
          keyheight = 0.2,
          default.unit = "inch",
          override.aes = list(size = 2.5))) +
        ggtitle(TeX(title_string))
      
      # save the plot
      ggsave(sprintf("%s%d.pdf", hypothesis, n),
             plot = plot_to_save,
             width = 5,
             height = 4)
    }
  }
}
