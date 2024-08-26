# This is Rscript working on wind dataset (regression-curve example)
library(readr)
library(tibble)
library(dplyr)
library(lubridate)
library(ggplot2)
library(NNCDT)


# load data set
wind_energy_WT3 <- read.csv("Inland_Offshore_Wind/Offshore\ Wind\ Farm\ Dataset2(WT3).csv")
wind_energy_WT4 <- read.csv("Inland_Offshore_Wind/Offshore\ Wind\ Farm\ Dataset2(WT4).csv")

# average over the day
# Convert datetime to date
wind_energy_WT3$date <- as.Date(wind_energy_WT3$time)
wind_energy_WT4$date <- as.Date(wind_energy_WT4$time)

# Group by date and calculate mean for each column
daily_averages_WT3 <- wind_energy_WT3 %>%
  distinct(time, .keep_all = TRUE) |>
  dplyr::select(-time) |>
  group_by(date) |>
  summarize(across(everything(), mean, na.rm = TRUE)) |>
  dplyr::rename(V1 = V,
                Vadj1 = Vadj,
                D1 = D,
                rho1 = rho,
                I1 = I,
                H1 = H,
                PW1 = normPW)

daily_averages_WT4 <- wind_energy_WT4 %>%
  distinct(time, .keep_all = TRUE) |>
  dplyr::select(-time) |>
  group_by(date) |>
  summarize(across(everything(), mean, na.rm = FALSE)) |>
  dplyr::rename(V2 = V,
                Vadj2 = Vadj,
                D2 = D,
                rho2 = rho,
                H2 = H,
                I2 = I,
                PW2 = normPW)

# combine two datasets into one
daily_averages_commbined <- daily_averages_WT3 |>
  dplyr::left_join(daily_averages_WT4, by = "date")

# remove
daily_averages_commbined <- daily_averages_commbined %>%
  dplyr::filter(complete.cases(daily_averages_commbined))


# restrict to year 2007 and 2008
K <- seq(20, 45, 5)
daily_averages_year12 <- daily_averages_commbined |>
  dplyr::filter(year(date) %in% c(2007, 2008))

data <- list(X = daily_averages_year12$PW1,
             Y = daily_averages_year12$PW2,
             Z = daily_averages_year12 |>
               dplyr::select(-c(PW1, PW2, date))|>
               as.data.frame())

mean_test_result <- sapply(K, function(k){
  asy_test(data, k = k, hyper = NULL, kernel_choice = "Linear")
})

print(mean_test_result)

distribution_test_result <- sapply(K, function(k){
  asy_test(data, k = k, hyper = "median_diff", kernel_choice = "Gaussian")
})

print(distribution_test_result)


# draw the frequency plot
my_theme <- theme_bw() + theme(axis.line = element_line(color = "black"),
                               plot.title = element_text(hjust = 0.5, size=11))

year12_frequency <- daily_averages_year12 |>
  dplyr::select(c(PW1,PW2)) |>
  dplyr::filter(PW1 >= 10 & PW1 <= 50 & PW2 >= 10 & PW2 <= 50) |>
  dplyr::rename(WT3 = PW1, WT4 = PW2) |>
  tidyr::pivot_longer(cols = c("WT3", "WT4"),
                      names_to = "Turbine",
                      values_to = "wind_power") |>
  ggplot(aes(x = wind_power, color = Turbine)) +
  geom_freqpoly(binwidth = 5) +
  labs(x = "Normalized wind power", y = "Count") +
  my_theme +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.background = element_rect(color = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.8),
        legend.title = element_text(hjust = 0.5))

ggsave("Year12_Frequency.pdf", plot = year12_frequency, height = 4, width = 4)



# restrict to year 2009 and 2010
K <- seq(20, 45, 5)
daily_averages_year34 <- daily_averages_commbined |>
  dplyr::filter(year(date) %in% c(2009, 2010))

data <- list(X = daily_averages_year34$PW1,
             Y = daily_averages_year34$PW2,
             Z = daily_averages_year34 |>
               dplyr::select(-c(PW1, PW2, date))|>
               as.data.frame())

mean_test_result <- sapply(K, function(k){
  asy_test(data, k = k, hyper = NULL, kernel_choice = "Linear")
})

print(mean_test_result)

distribution_test_result <- sapply(K, function(k){
  asy_test(data, k = k, hyper = "median_diff", kernel_choice = "Gaussian")
})

print(distribution_test_result)

# draw the frequency plot for year 3 and 4
year34_frequency <- daily_averages_year34 |>
  dplyr::select(c(PW1,PW2)) |>
  dplyr::filter(PW1 >= 10 & PW1 <= 50 & PW2 >= 10 & PW2 <= 50) |>
  dplyr::rename(WT3 = PW1, WT4 = PW2) |>
  tidyr::pivot_longer(cols = c("WT3", "WT4"),
                      names_to = "Turbine",
                      values_to = "wind_power") |>
  ggplot(aes(x = wind_power, color = Turbine)) +
  geom_freqpoly(binwidth = 5) +
  labs(x = "Normalized wind power", y = "Count") +
  my_theme +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.background = element_rect(color = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8,0.8),
        legend.title = element_text(hjust = 0.5))

ggsave("Year34_Frequency.pdf", plot = year34_frequency, height = 4, width = 4)

