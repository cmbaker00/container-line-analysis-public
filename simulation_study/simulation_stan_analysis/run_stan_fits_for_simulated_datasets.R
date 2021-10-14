
source('simulation_study/simulation_stan_analysis/fit_simulated_data_full_model.R')


data_filename <- 'line_data_2_5_no_corr_1000'
load_sim_data <- read.csv(paste0('simulation_study/simulation_data/container_line_simulated_data_entrysize_2_5_entry_corr0.25_rows_1e+05.csv'))
load_sim_data <- load_sim_data %>% filter(Mode == 'Line')
load_sim_data <- load_sim_data[1:1000,]
#load_sim_data <- load_sim_data %>% mutate(Country=1)
#load_sim_data <- load_sim_data %>% mutate(Entry=1)

run_stan_simulated_data(load_sim_data,
                        data_filename,
                        entry_correlation_flag = FALSE
)