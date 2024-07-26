require(tidyverse)
require(kableExtra)
require(latex2exp)

KCSD_TypeI_Power <- read.csv("KCSD_results.csv")
ECMMD_TypeI <- as_tibble(t(read.csv("CGOF_ECMMD_TypeI.csv")[, -1]))

TypeI <- KCSD_TypeI_Power %>%
  mutate(NN10 = ECMMD_TypeI$V1[-1],
         NN20 = ECMMD_TypeI$V2[-1],
         NN30 = ECMMD_TypeI$V3[-1],
         NN40 = ECMMD_TypeI$V4[-1],
         KCSD = TypeI_error) %>%
  select(-Power) %>% select(-TypeI_error)

colnames(TypeI) <- c("Sample Size", " 10 NN", "20 NN", "30 NN", "40 NN", "KCSD")

kable_output <- kable(TypeI, "latex", booktabs = T, escape = FALSE,
                      align = c('c', 'c', 'c', 'c', 'c', 'c')) %>%
  kable_styling(font_size = 5, latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  add_header_above("Type-I Error") %>%
  row_spec(nrow(TypeI))

writeLines(kable_output, "CGOF_TypeIError.tex")

ECMMD_Power <- as_tibble(t(read.csv("CGOF_ECMMD_Power.csv")[, -1]))

Power <- KCSD_TypeI_Power %>%
  mutate(NN10 = ECMMD_Power$V1[-1],
         NN20 = ECMMD_Power$V2[-1],
         NN30 = ECMMD_Power$V3[-1],
         NN40 = ECMMD_Power$V4[-1],
         KCSD = Power) %>%
  select(-TypeI_error) %>% select(-Power)

# plotting using ggplot
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 11))
pannel_cols <- c("10 NN" = "darkorchid4",
                 "20 NN" = "dodgerblue4", "30 NN" = "dodgerblue3",
                 "40 NN" = "deepskyblue", "KCSD" = "coral")

Power_Plot <- Power %>%
  ggplot(aes(x = sample_size, y = KCSD)) +
  geom_line(aes(x = sample_size, y = KCSD, color = "KCSD")) + 
  geom_point(aes(x = sample_size, y = KCSD, color = "KCSD"), size = 2) + 
  geom_line(aes(x = sample_size, y = NN10, color = "10 NN")) + 
  geom_point(aes(x = sample_size, y = NN10, color = "10 NN"), size = 2) + 
  geom_line(aes(x = sample_size, y = NN20, color = "20 NN")) + 
  geom_point(aes(x = sample_size, y = NN20, color = "20 NN"), size = 2) + 
  geom_line(aes(x = sample_size, y = NN30, color = "30 NN")) + 
  geom_point(aes(x = sample_size, y = NN30, color = "30 NN"), size = 2) + 
  geom_line(aes(x = sample_size, y = NN40, color = "40 NN")) + 
  geom_point(aes(x = sample_size, y = NN40, color = "40 NN"), size = 2) + 
  labs(x = TeX(r'($n$)'),
       y = "Empirical Power") +
  my_theme +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.background = element_rect(color = 1),
        legend.position=c(0.15, 0.7),
        legend.title = element_blank()) + 
  scale_color_manual(values = pannel_cols, breaks = c("10 NN", "20 NN", "30 NN",
                                                      "40 NN", "KCSD")) +
  guides(color = guide_legend(
    keywidth = 0.0,
    keyheight = 0.2,
    default.unit = "inch",
    override.aes = list(size = 2.5))) +
  ggtitle(TeX(r'(Conditional Goodness of Fit Test)'))

Power_Plot

ggsave("CGOF_PowerPlot.pdf",
       plot = Power_Plot, 
       width = 4,
       height = 3)