library(brms)
library(tidyverse)


# Simulated data set ------------------------------------------------------


# normal linear 
set.seed(10101) # Omit or change this if you like

N <- 25

x_1 <- rnorm(N)
x_2 <- rnorm(N)

beta_0 <- 1.25
beta_1 <- 1.75
beta_2 <- 2.25

mu <- beta_0 + beta_1 * x_1 + beta_2 * x_2

y <- mu + rnorm(N, mean=0, sd=1.75)

data_df1 <- tibble(x_1, x_2, y)



# Classical / frequentist linear regression --------------------------------

M_freq_1 <- lm(y ~ x_1 + x_2, data = data_df1)

summary(M_freq_1)
confint(M_freq_1)

M_bayes_1 <- brm(y ~ x_1 + x_2, data = data_df1)
summary(M_bayes_1)
plot(M_bayes_1)

mcmc_plot(M_bayes_1, type = 'hist', binwidth = 0.05)
mcmc_plot(M_bayes_1, 
          type = 'hist', 
          binwidth = 0.05, variable = 'sigma')

mcmc_plot(M_bayes_1, 
          type = 'areas', variable = 'sigma')

# get the samples
prepare_predictions(M_bayes_1)$dpars$mu$fe$b

# where are the priors?
prior_summary(M_bayes_1)
