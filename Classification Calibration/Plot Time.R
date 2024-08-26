# This is a Rscript for classification calibration
library(ggplot2)
library(tibble)
library(dplyr)
library(latex2exp)
n <- c(100)
alpha <- 0.05


# load the result
data <- readRDS(sprintf("Time%d.rds", n))

# summarise the type-I error or rejection rate
data_to_plot <- as_tibble(as.table(data)) |>
  mutate(time = n) |>
  dplyr::select(-n) |>
  mutate(method = factor(as.factor(method),
                         levels = c("SKCE", "asym_NN15", "asym_NN25", 
                                    "derandomized_NN15", "derandomized_NN25")))


# plot
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=11))
pannel_cols <- c("coral", "darkorchid4", "dodgerblue4", "dodgerblue3",
                 "dodgerblue1", "deepskyblue")

# ggplot
plot_to_save <- data_to_plot |>
  ggplot(aes(x = method, y = time, color = method)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "method",
       y = "Time (seconds)") +
  my_theme +
  theme(legend.background = element_rect(color = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),  # Adjust the size if needed
        legend.key.size = unit(0.5, "lines"), # Adjust the size if needed
        legend.spacing.x = unit(0.1, "cm"),   # Adjust horizontal spacing
        legend.spacing.y = unit(0.1, "cm")    # Adjust vertical spacing
  ) +
  scale_color_manual(labels = c("SKCE", "15 NN (asymp)","25 NN (asymp)",
                                "15 NN (derandom)","25 NN (derandom)"), values = pannel_cols) +
  scale_x_discrete(labels = c("SKCE", "NN15 (asymp)", "NN25 (asymp)", "NN15 (derandom)", "NN25 (derandom)")) +
  guides(color = guide_legend(
    ncol = 3,  # Set the legend to 2 columns
    byrow = TRUE, # Ensure items are filled by row
    keywidth = 0.0,
    keyheight = 0.2,
    default.unit = "inch",
    override.aes = list(size = 1.6)))

# save the plot
ggsave(sprintf("Time%d.pdf", n),
       plot = plot_to_save,
       width = 6,
       height = 4)
