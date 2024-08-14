f.zsamples <- function(n, mu, theta_cutoff, sigma2 = 0.25){
  
  U_ind <- rbinom(n, 1, 0.5); V_ind <- rbinom(n, 1, 0.5)
  z_ind <- rbinom(n, 1, 0.5)
  
  z <- rep(0, n)
  
  theta_sample <- runif(n, min = -pi/2, max = pi/2)
  
  for (i in 1:n){
    
    if (theta_sample[i] > theta_cutoff){
      
      U <- rnorm(1, 2, 1)
      V <- rnorm(1, -2, 1)
      
    }
    
    else{
      
      U <- U_ind[i] * rnorm(1, 3, sigma2) + (1-U_ind[i]) * rnorm(1, mu, sigma2)
      V <- V_ind[i] * rnorm(1, -3, sigma2) + (1-V_ind[i]) * rnorm(1, -mu, sigma2)
      
    }
    
    z[i] <- z_ind[i] * U + (1-z_ind[i]) * V
    
  }
  
  return (z)
  
}

require(tidyverse)
library(reshape2)
require(latex2exp)
set.seed(1)

sigma2 = 0.25

n <- 10000
theta_cutoff = -pi/3

zsample0 <- as_tibble(f.zsamples(n, 0, theta_cutoff, sigma2 = sigma2))
zsample05 <- as_tibble(f.zsamples(n, 0.5, theta_cutoff, sigma2 = sigma2))
zsample1 <- as_tibble(f.zsamples(n, 1, theta_cutoff, sigma2 = sigma2))
zsample15 <- as_tibble(f.zsamples(n, 1.5, theta_cutoff, sigma2 = sigma2))

zsample <- cbind(zsample0, zsample05, zsample1, zsample15)

colnames(zsample) <- c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
                       TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))

zsample <- zsample %>% melt()

# variable.names <- c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
#                     TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))

z.density.plot <- ggplot(zsample, aes(x=value)) + 
  geom_histogram(aes(y=..density..), colour="navy", fill="white", bins = 20)+
  geom_density(alpha=.6, fill="cornflowerblue") +
  facet_wrap(~ variable, scales = 'free', nrow = 2,
             labeller = "label_parsed") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  
z.density.plot

ggsave(plot = z.density.plot, filename = "zdensity_theta-pi2.pdf", device = "pdf",
       width = 4, height = 4)

set.seed(1)

n <- 10000
theta_cutoff = 0
zsample0 <- as_tibble(f.zsamples(n, 0, theta_cutoff, sigma2 = sigma2))
zsample05 <- as_tibble(f.zsamples(n, 0.5, theta_cutoff, sigma2 = sigma2))
zsample1 <- as_tibble(f.zsamples(n, 1, theta_cutoff, sigma2 = sigma2))
zsample15 <- as_tibble(f.zsamples(n, 1.5, theta_cutoff, sigma2 = sigma2))

zsample <- cbind(zsample0, zsample05, zsample1, zsample15)

colnames(zsample) <- c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
                       TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))

zsample <- zsample %>% melt()

# variable.names <- c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
#                     TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))

z.density.plot <- ggplot(zsample, aes(x=value)) + 
  geom_histogram(aes(y=..density..), colour="navy", fill="white", bins = 20)+
  geom_density(alpha=.6, fill="cornflowerblue") +
  facet_wrap(~ variable, scales = 'free', nrow = 2,
             labeller = "label_parsed") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

z.density.plot

ggsave(plot = z.density.plot, filename = "zdensity_theta0.pdf", device = "pdf",
       width = 4, height = 4)

set.seed(1)

n <- 10000
theta_cutoff = pi/3
zsample0 <- as_tibble(f.zsamples(n, 0, theta_cutoff, sigma2 = sigma2))
zsample05 <- as_tibble(f.zsamples(n, 0.5, theta_cutoff, sigma2 = sigma2))
zsample1 <- as_tibble(f.zsamples(n, 1, theta_cutoff, sigma2 = sigma2))
zsample15 <- as_tibble(f.zsamples(n, 1.5, theta_cutoff, sigma2 = sigma2))

zsample <- cbind(zsample0, zsample05, zsample1, zsample15)

colnames(zsample) <- c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
                       TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))

zsample <- zsample %>% melt()

# variable.names <- c(TeX(r'($\mu = 0$)'), TeX(r'($\mu = 0.5$)'),
#                     TeX(r'($\mu = 1$)'), TeX(r'($\mu = 1.5$)'))

z.density.plot <- ggplot(zsample, aes(x=value)) + 
  geom_histogram(aes(y=..density..), colour="navy", fill="white", bins = 20)+
  geom_density(alpha=.6, fill="cornflowerblue") +
  facet_wrap(~ variable, scales = 'free', nrow = 2,
             labeller = "label_parsed") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

z.density.plot

ggsave(plot = z.density.plot, filename = "zdensity_thetapi2.pdf", device = "pdf",
       width = 4, height = 4)
