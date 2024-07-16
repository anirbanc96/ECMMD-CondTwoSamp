require(tidyverse)
require(reshape2)
library(scales)
require(latex2exp)

SLCP_mdn <- cbind(c("MDN 5", "MDN 10", "MDN 15", "MDN 25"),
                      read_csv("mdn_numsim_power.csv")[,-1])
colnames(SLCP_mdn) <- c("Estimator", "10000",
                            "25000", "50000", "75000", "100000")
SLCP_nsf <- cbind(c("NSF 5"), read_csv("nsf_numsim_power.csv")[,-1])
colnames(SLCP_nsf) <- c("Estimator", "10000",
                        "25000", "50000", "75000", "100000")
SLCP_data <- rbind(SLCP_mdn, SLCP_nsf)
write_csv(SLCP_data, "SLCP_data.csv")
data <- melt(SLCP_data, id = 1)
colnames(data) <- c("est", "samp_size", "power") 

mdn_colors = c("darkorchid4", "dodgerblue3", "dodgerblue1", "deepskyblue",
               "coral")
data <- data %>%
  mutate(est_factor = factor(est, levels = c("MDN 5", "MDN 10", "MDN 15",
                                             "MDN 25", "NSF 5")))
SLCP_mdn_plot <- data %>%
  ggplot(aes(x = samp_size, y = power, group = est_factor)) +
  geom_point(aes(color=est_factor), size = 2) + 
  geom_line(aes(color=est_factor)) +
  scale_color_manual(values = mdn_colors) +
  scale_x_discrete(labels = c(TeX(r'($10^4$)'),
                              TeX(r'($2.5\times 10^4$)'),
                              TeX(r'($5\times 10^4$)'),
                              TeX(r'($7.5\times 10^4$)'),
                              TeX(r'($10^5$)'))) +
  labs(title = TeX(r'(Performance of ECMMD Test)'), 
       x = TeX(r'(Training Sample Size)'),
       y = TeX(r'(Proportion of rejections)'), color = "Legend Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8, 0.77),
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title=element_blank(), 
        legend.text=element_text(size=7),
        legend.key.size = unit(0.4, "cm"))

SLCP_mdn_plot

ggsave(plot = SLCP_mdn_plot, filename = "SLCPplot.pdf", device = "pdf",
       width = 5, height = 4)

