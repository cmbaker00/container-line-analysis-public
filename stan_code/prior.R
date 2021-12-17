N = 1000000
set.seed(147)
pint = rnorm(N, -4, 4)
beta_doc = rnorm(N, 0, 0.5)
country_effect = rnorm(N, 0, 0.5)
sigma_entry = runif(N, 0, 0.5)
entry_effect = sapply(sigma_entry, function(sigma) rnorm(1, 0, sigma))
zeros = rep(0, N)

# 4 possible combinations: beta doc or not, entry effect or not.
prior_df = data.frame(
  pint = rep(pint, 4),
  country_effect = rep(country_effect, 4),
  beta_doc = rep(c(beta_doc, zeros), 2),
  entry_effect = c(rep(entry_effect, 2), rep(zeros, 2))
  ) %>%
  mutate(
    p_logit = pint + country_effect + beta_doc + entry_effect,
    p = inv.logit(p_logit)
  )

p1 = prior_df %>%
  filter(entry_effect > 0) %>%
ggplot(aes(x = p)) +
  geom_histogram(breaks = seq(0,1,0.0025)) +
    geom_vline(xintercept = c(0.001, 0.01, 0.02, 0.05, .2))+
  labs(title = "Prior with entry effect") +
  xlim(0.0005, 0.3)

p2 = prior_df %>%
  filter(entry_effect > 0) %>%
ggplot(aes(x = p_logit)) +
  geom_histogram(breaks = seq(-10,10,0.0025)) +
    geom_vline(xintercept = qlogis(c(0.001, 0.01, 0.02, 0.05, .2)))+
  labs(title = "Prior with entry effect") +
  xlim(-10, 10)

 p1 + p2