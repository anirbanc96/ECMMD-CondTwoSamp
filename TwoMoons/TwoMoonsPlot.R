require(tidyverse)
require(reshape2)
library(scales)
require(latex2exp)

twomoons_mdn <- cbind(c("MDN 1", "MDN 3", "MDN 5", "MDN 7"),
                      read_csv("Data/mdn_numsim_power_twomoons.csv")[,-1])
colnames(twomoons_mdn) <- c("Estimator", "100", "500", "1000", "5000",
                            "10000", "50000", "100000")
twomoons_nsf <- cbind(c("NSF 5"), read_csv("Data/nsf_numsim_power_twomoons.csv")[,-1])
colnames(twomoons_nsf) <- c("Estimator", "100", "500", "1000", "5000",
                            "10000", "50000", "100000")
twomoons_data <- rbind(twomoons_mdn, twomoons_nsf)
write_csv(twomoons_data, "Data/twomoons_data.csv")
data <- melt(twomoons_data, id = 1)
colnames(data) <- c("est", "samp_size", "power") 

mdn_colors = c("darkorchid4", "dodgerblue3", "dodgerblue1", "deepskyblue",
               "coral")

twomoons_mdn_plot <- data %>%
  ggplot(aes(x = samp_size, y = power, group = factor(est))) +
  geom_point(aes(color=factor(est)), size = 2) + 
  geom_line(aes(color=factor(est))) +
  scale_color_manual(values = mdn_colors) +
  scale_x_discrete(labels = c(TeX(r'($10^2$)'), TeX(r'($5\times 10^2$)'),
                              TeX(r'($10^3$)'), TeX(r'($5\times 10^3$)'),
                              TeX(r'($10^4$)'), TeX(r'($5\times 10^4$)'),
                              TeX(r'($10^5$)'))) +
  labs(title = TeX(r'(Performance of ECMMD Test)'), 
       x = TeX(r'(Training Sample Size)'),
       y = TeX(r'(Proportion of rejections)'), color = "Legend Title") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.1, 0.2),
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title=element_blank(), 
        legend.text=element_text(size=7),
        legend.key.size = unit(0.5, "cm"))

twomoons_mdn_plot

ggsave(plot = twomoons_mdn_plot, filename = "Figures/TwoMoonsplot.pdf", device = "pdf",
       width = 5, height = 4)

