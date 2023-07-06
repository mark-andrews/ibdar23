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


new_priors_2 <- c(
  set_prior(class = 'b', prior = 'normal(0, 10)'),
  set_prior(class = 'Intercept', prior = 'normal(0, 100)'),
  set_prior(class = 'sigma', prior = 'student_t(1, 0, 10)')
)

M_bayes_6 <- brm(weight ~ height + age + gender, 
                 iter = 5000,
                 warmup = 500,
                 chains = 6,
                 cores = 6,
                 prior = new_priors_2,
                 data = weight_df)

# Not a normal linear model but a t linear model

M_bayes_7 <- brm(weight ~ height + age + gender, 
                 cores = 4,
                 family = student(),
                 save_pars = save_pars(all = TRUE),
                 data = weight_df)

M_bayes_8 <- brm(weight ~ height + age + gender, 
                 cores = 4,
                 save_pars = save_pars(all = TRUE),
                 data = weight_df)

M_bayes_9 <- brm(weight ~ height + age + gender + race, 
                 cores = 4,
                 save_pars = save_pars(all = TRUE),
                 data = weight_df)

# Model comparison: WAIC
waic(M_bayes_7, M_bayes_8)

waic(M_bayes_9, M_bayes_8)

loo(M_bayes_9, M_bayes_8)

bayes_factor(M_bayes_9, M_bayes_8, log = T)



# Generalized linear models -----------------------------------------------

smoking_df <- mutate(smoking_df, smoker = cigs > 0)

# logistic regression predicting smoking as a function of 
# educ, cigpric, age, restaurn, lincome

M_freq_10 <- glm(smoker ~ educ + cigpric + age + restaurn + lincome, 
                 data = smoking_df,
                 family = binomial(link = 'logit'))


M_bayes_10 <- brm(smoker ~ educ + cigpric + age + restaurn + lincome, 
                  data = smoking_df,
                  family = bernoulli(link = 'logit'))

M_bayes_11 <- brm(smoker ~ educ, 
                  data = smoking_df,
                  family = bernoulli(link = 'logit'))

M_freq_11 <- glm(smoker ~ educ, 
                 data = smoking_df,
                 family = binomial(link = 'logit'))

smoking_educ <- tibble(educ = seq(5, 20))
predict(M_freq_11, newdata = smoking_educ, type = 'response')

posterior_linpred(M_bayes_11, newdata = smoking_educ, transform = T) %>% 
  apply(2, mean)

summary(M_freq_10)$coefficients
confint.default(M_freq_10)
brms::fixef(M_bayes_10)

prior_summary(M_bayes_10)

plot(M_bayes_10)

mcmc_plot(M_bayes_10, type = 'hist', variable = 'b_Intercept')

# Mixed effects / multilevel models ---------------------------------------

library(lme4)

ggplot(data = sleepstudy, aes(x=Days,y=Reaction,colour=Subject)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = F) +
  facet_wrap(~Subject)

M_freq_11 <- lmer(Reaction ~ Days + (Days|Subject),
                  data = sleepstudy)

M_bayes_11 <- brm(Reaction ~ Days + (Days|Subject),
                 data = sleepstudy)

M_bayes_11
plot(M_bayes_11)

prior_summary(M_bayes_11)

M_freq_12 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|classid), data = classroom_df)
M_bayes_12 <- brm(mathscore ~ ses + (ses|schoolid) + (ses|classid), data = classroom_df)
