library(dplyr)

data <- readRDS('simulation_study/simulation_stan_results/test_run_fit_summary.Rda')
data %>% select(summary.mean, summary.sd)