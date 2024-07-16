library(tidyverse)
library(reshape2)
library(latex2exp)

power.data <- read_csv("powermu220.csv") %>%
  cbind(read_csv("powermu2205.csv")) %>%
  cbind(read_csv("powermu221.csv")) %>%
  cbind(read_csv("powermu2215.csv"))

colnames(power.data) <- c("mu1", "mu2", "mu3", "mu4")

angles <- c(-pi/3, -pi/4, -pi/6, -pi/12, 0, pi/12, pi/6, pi/4, pi/3)
angle.labels <- c(expression(paste(-pi,"/3")), expression(paste(-pi,"/4")),
                  expression(paste(-pi,"/6")), expression(paste(-pi,"/12")), "0",
                  expression(paste(pi,"/12")), expression(paste(pi,"/6")),
                  expression(paste(pi,"/4")), expression(paste(-pi,"/3")))

power.data <- power.data %>%
  mutate(angles = angles, .before = mu1)

# power.plot <- power.data %>%
#   ggplot(aes(x = angles, y = mu1)) +
#   geom_line(aes(x = angles, y = mu1, col = "mu1"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = mu1), col = "navy") +
#   geom_line(aes(x = angles, y = mu2, col = "mu2"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = mu2), col = "darkorange") +
#   geom_line(aes(x = angles, y = mu3, col = "mu3"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = mu3), col = "darkred") +
#   geom_line(aes(x = angles, y = mu4, col = "mu4"), linetype = "longdash") +
#   geom_point(aes(x = angles, y = mu4), col = "darkgreen") +
#   scale_colour_manual(NULL,
#                       breaks = c("mu1", "mu2", "mu3", "mu4"),
#                       values = c("cornflowerblue", "orange", "red", "green3"),
#                       labels = c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
#                                  TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))) +
#   scale_x_continuous(breaks = angles, labels = angle.labels) +
#   labs(title = TeX(r'(Performance of ECMMD Test)'), x = TeX(r'($\theta$)'),
#        y = TeX(r'(Proportion of rejections)'), color = "Legend Title\n") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = c(0.12,0.79),
#         legend.text.align = 0,
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black"))
  

power.plot <- power.data %>%
  ggplot(aes(x = angles, y = mu1)) +
  geom_line(aes(x = angles, y = mu1, col = "mu1")) +
  geom_point(aes(x = angles, y = mu1), size = 2, col = "darkorchid4") +
  geom_line(aes(x = angles, y = mu2, col = "mu2")) +
  geom_point(aes(x = angles, y = mu2), size = 2, col = "dodgerblue3") +
  geom_line(aes(x = angles, y = mu3, col = "mu3")) +
  geom_point(aes(x = angles, y = mu3), size = 2, col = "dodgerblue1") +
  geom_line(aes(x = angles, y = mu4, col = "mu4")) +
  geom_point(aes(x = angles, y = mu4), size = 2, col = "deepskyblue") +
  scale_colour_manual(NULL,
                      breaks = c("mu1", "mu2", "mu3", "mu4"),
                      values = c("darkorchid4", "dodgerblue3", "dodgerblue1",
                                 "deepskyblue"),
                      labels = c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
                                 TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))) +
  scale_x_continuous(breaks = angles, labels = angle.labels) +
  labs(title = TeX(r'(Performance of ECMMD Test)'), x = TeX(r'($\theta$)'),
       y = TeX(r'(Proportion of rejections)'), color = "Legend Title\n") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.87,0.32),
        legend.text.align = 0,
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

power.plot

ggsave(plot = power.plot, filename = "PowerVsThetaCutoff.pdf", device = "pdf",
       width = 5, height = 4)
