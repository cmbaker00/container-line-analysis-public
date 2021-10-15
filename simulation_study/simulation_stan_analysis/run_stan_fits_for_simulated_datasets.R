
source('simulation_study/simulation_stan_analysis/fit_simulated_data_full_model.R')
source('simulation_study/simulation_naming_functions.R')

min_entry_size <- 2
max_entry_size <- 5
sd <- 0
target_num_rows_data <- '1e+05'

data_filename <- gen_filename_simulate_data(min_entry_size,max_entry_size,sd,1e5)
load_sim_data <- read.csv(data_filename)
load_sim_data <- load_sim_data %>% filter(Mode == 'Line')
load_sim_data <- load_sim_data[1:1000,]

#load_sim_data <- load_sim_data %>% mutate(Country=1)
#load_sim_data <- load_sim_data %>% mutate(Entry=1)

fit_summary_df <- run_stan_simulated_data(load_sim_data,
                        data_filename,
                        entry_correlation_flag = FALSE
)
num_data_rows <- load_sim_data %>% group_by(Mode) %>% summarise(n=n())

num_line_data <- num_data_rows[num_data_rows$Mode=='Line',]$n
num_container_data <- num_data_rows[num_data_rows$Mode=='Container',]$n

gen_filename_stanfit_simulated_data(min_entry_size, max_entry_size,sd,num_line_data, num_container_data)

saveRDS(fit_summary_df, paste0('simulation_study/simulation_stan_results/',data_filename,"_fit_summary.Rda"))
saveRDS(fit_summary_df, data_filename)