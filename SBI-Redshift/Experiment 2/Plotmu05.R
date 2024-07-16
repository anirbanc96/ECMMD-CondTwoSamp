library(tidyverse)
library(reshape2)
library(latex2exp)

power.data <- read_csv("powermu2205_n1000_k50.csv") %>%
  cbind(read_csv("powermu2205_n1000_k100.csv")) %>%
  cbind(read_csv("powermu2205.csv")) %>%
  cbind(read_csv("powermu2205_n2500_k50.csv"))

colnames(power.data) <- c("n1k1", "n1k2", "n2k2", "n2k1")

angles <- c(-pi/3, -pi/4, -pi/6, -pi/12, 0, pi/12, pi/6, pi/4, pi/3)
angle.labels <- c(expression(paste(-pi,"/3")), expression(paste(-pi,"/4")),
                  expression(paste(-pi,"/6")),
                  expression(paste(-pi,"/12")), "0",
                  expression(paste(pi,"/12")), expression(paste(pi,"/6")),
                  expression(paste(pi,"/4")), expression(paste(pi,"/3")))

power.data <- power.data %>%
  mutate(angles = angles, .before = n1k1)

# power.plot <- power.data %>%
#   ggplot(aes(x = angles, y = n1k1)) +
#   geom_line(aes(x = angles, y = n1k1, col = "n1k1"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = n1k1), col = "navy") +
#   geom_line(aes(x = angles, y = n1k2, col = "n1k2"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = n1k2), col = "darkred") +
#   geom_line(aes(x = angles, y = n2k1, col = "n2k1"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = n2k1), col = "darkgreen") +
#   geom_line(aes(x = angles, y = n2k2, col = "n2k2"), linetype = "longdash")  +
#   geom_point(aes(x = angles, y = n2k2), col = "darkmagenta") +
#   scale_colour_manual(NULL,
#                       breaks = c("n1k1", "n1k2", "n2k1", "n2k2"),
#                       values = c("cornflowerblue", "red", "green3", "plum"),
#                       labels = c("n = 500, k = 25", "n = 500, k = 50",
#                                  "n = 1000, k = 25", "n = 1000, k = 50")) +
#   scale_x_continuous(breaks = angles, labels = angle.labels) +
#   labs(title = TeX(r'(Performance of ECMMD Test with $\mu = 1.5$)'), x = TeX(r'($\theta$)'),
#        y = TeX(r'(Proportion of Rejection)'), color = "Legend Title\n") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = c(0.2,0.78),
#         legend.text.align = 0,
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black"))


power.plot <- power.data %>%
  ggplot(aes(x = angles, y = n1k1)) +
  geom_line(aes(x = angles, y = n1k1, col = "n1k1")) +
  geom_point(aes(x = angles, y = n1k1), size = 2, col = "deepskyblue") +
  geom_line(aes(x = angles, y = n1k2, col = "n1k2")) +
  geom_point(aes(x = angles, y = n1k2), size = 2, col = "dodgerblue1") +
  geom_line(aes(x = angles, y = n2k1, col = "n2k1")) +
  geom_point(aes(x = angles, y = n2k1), size = 2, col = "dodgerblue3") +
  geom_line(aes(x = angles, y = n2k2, col = "n2k2"))  +
  geom_point(aes(x = angles, y = n2k2), size = 2, col = "darkorchid4") +
  scale_colour_manual(NULL,
                      breaks = c("n1k1", "n1k2", "n2k1", "n2k2"),
                      values = c("deepskyblue", "dodgerblue1", "dodgerblue3",
                                 "darkorchid4"),
                      labels = c("n = 1000, k = 50", "n = 1000, k = 100",
                                 "n = 2500, k = 50", "n = 2500, k = 100")) +
  scale_x_continuous(breaks = angles, labels = angle.labels) +
  labs(title = TeX(r'(Performance of ECMMD Test with $\mu = 0.5$)'), x = TeX(r'($\theta$)'),
       y = TeX(r'(Proportion of Rejection)'), color = "Legend Title\n") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.2,0.8),
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  guides(colour = guide_legend(reverse=T))


power.plot

ggsave(plot = power.plot, filename = "Powerformu05.pdf", device = "pdf",
       width = 5, height = 4)
