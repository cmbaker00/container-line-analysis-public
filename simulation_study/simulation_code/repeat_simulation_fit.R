# new simulated data

library(tidyverse)

source('simulation_study/simulation_code/simulate_data.R')
source('simulation_study/simulation_stan_analysis/fit_simulated_data_full_model.R')


num_fits <- 20

min_entry_size <- 1
max_entry_size <- 20
entry_corr_sd <- 0.25
num_rows <- 1e+05

# for each of container and line
# possibly could do fewer different values?
num_lines_vals <- c(1000, 2000, 3000, 4000, 5000, 10000, 15000, 20000, 25000)

base_path_data <- 'simulation_study/new_sim_data/new_sim_data_no'
base_path_output <- 'simulation_study/new_sim_results/fit_summary'

use_cmdstan <- FALSE

# generate separate simulation files for each fit
generate_data <- function (min_entry_size, max_entry_size, entry_corr_sd, num_rows, num_batches, base_data_path) {

    for (i in 1:num_batches) {

        sim_fname <- paste0(base_data_path, i, '.csv')
        simulate_data_function(min_entry_size = min_entry_size, max_entry_size = max_entry_size, 
            entry_correlation_sd = entry_corr_sd, target_num_rows_data = num_rows, fname_override=sim_fname)
        
    }

}

# fit model to a file, given numbers of line & container
fit_model <- function (base_data_path, base_output_path, index, num_line, num_container) {

    data_fname <- paste0(base_data_path, index, '.csv')
    output_fname <- paste0(base_output_path, '_line_', num_line, '_container_', num_container, '_no', index, '.Rda')

    data <- read.csv(data_fname)

    line_data <- data %>% filter(Mode == 'Line') %>% head(num_line)
    container_data <- data %>% filter(Mode == 'Container') %>% head(num_container)
    data_for_fit <- rbind(line_data, container_data)

    fit_summary_df <- run_stan_simulated_data(data_for_fit,
                                              data_fname,
                                              entry_correlation_flag = FALSE, # ??
                                              use_cmdstan = use_cmdstan
    )
    
    saveRDS(fit_summary_df, output_fname)

} 

# run generating code
# generate_data (min_entry_size, max_entry_size, entry_corr_sd, num_rows, num_fits, base_path_data)

# run fit code
for (i in 1:num_fits) {
    for (n in num_lines_vals) {
        fit_model (base_path_data, base_path_output, i, n, n) # equal mix container and line
    }
}
