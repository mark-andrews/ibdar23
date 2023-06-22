library(priorexposure)

n <- 250 # number of coin flips
m <- 139 # number of Heads in the n flips

bernoulli_likelihood(n, m)

n_prime <- 25
m_prime <- 14

bernoulli_likelihood(n_prime, m_prime)

beta_plot(3, 5)
beta_plot(10, 10)
beta_plot(10, 20)
beta_plot(20, 10)

beta_plot(4, 2)

beta_plot(10, 10)
beta_plot(139 + 10, 111 + 10)

bernoulli_posterior_plot(n, m, 10, 10)


beta_plot(2, 2)
bernoulli_posterior_plot(n_prime, m_prime, 2, 2)

beta_plot(3, 2)
bernoulli_posterior_plot(n_prime, m_prime, 3, 2)

bernoulli_posterior_plot(n, m, 2, 2)
bernoulli_posterior_plot(n, m, 3, 2)

beta_plot(10, 5)
bernoulli_posterior_plot(n, m, 10, 5)

beta_plot(0.5, 0.5)

bernoulli_posterior_plot(n, m, 0.5, 0.5)

bernoulli_posterior_summary(n, m, 2, 2)
bernoulli_posterior_plot(n, m, 2, 2)
get_beta_hpd(m + 2, n -m  + 2)

# Below which point has 2.5% of the area under curve in the posterior 
qbeta(0.025, m+2, n-m+2)
# Above which point has 2.5% of the area under curve in the posterior 
# Below which point has 97.5% of the area under curve in the posterior 
qbeta(0.975, m+2, n-m+2)
