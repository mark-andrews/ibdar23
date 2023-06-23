library(tidyverse)
library(brms)

M_freq_2 <- lm(weight ~ height + age + gender, data = weight_df)
M_bayes_2 <- brm(weight ~ height + age + gender, data = weight_df)

M_bayes_3 <- brm(weight ~ height + age + gender, 
                 iter = 5000,
                 warmup = 500,
                 chains = 6,
                 cores = 6,
                 data = weight_df)


M_bayes_4 <- brm(weight ~ height + age + gender, 
                 iter = 5000,
                 warmup = 500,
                 chains = 6,
                 cores = 6,
                 prior = set_prior('normal(0, 10)'),
                 data = weight_df)

prior_summary(M_bayes_3)
prior_summary(M_bayes_4)

brms::fixef(M_bayes_3)
brms::fixef(M_bayes_4)

# create a vector (or list) of priors

new_priors <- c(
  set_prior(class = 'b', coef = 'height', prior = 'normal(0, 10)'),
  set_prior(class = 'b', coef = 'gendermale', prior = 'normal(0, 5)'),
  set_prior(class = 'b', coef = 'age', prior = 'normal(0, 1)'),
  set_prior(class = 'Intercept', prior = 'normal(0, 100)'),
  set_prior(class = 'sigma', prior = 'student_t(1, 0, 10)')
)
  


M_bayes_5 <- brm(weight ~ height + age + gender, 
                 iter = 5000,
                 warmup = 500,
                 chains = 6,
                 cores = 6,
                 prior = new_priors,
                 data = weight_df)

