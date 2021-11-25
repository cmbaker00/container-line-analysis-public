source('simulation_study/simulation_code/simulate_data.R')

simulate_data_function(min_entry_size = 1, max_entry_size = 20, entry_correlation_sd = 0, target_num_rows_data = 1e5)
simulate_data_function(min_entry_size = 1, max_entry_size = 20, entry_correlation_sd = 0.25, target_num_rows_data = 1e5)

single_entry_size <- 2
simulate_data_function(min_entry_size = single_entry_size, max_entry_size = single_entry_size, entry_correlation_sd = 0, target_num_rows_data = 1e5)

single_entry_size <- 5
simulate_data_function(min_entry_size = single_entry_size, max_entry_size = single_entry_size, entry_correlation_sd = 0, target_num_rows_data = 1e5)

single_entry_size <- 10
simulate_data_function(min_entry_size = single_entry_size, max_entry_size = single_entry_size, entry_correlation_sd = 0, target_num_rows_data = 1e5)

single_entry_size <- 20
simulate_data_function(min_entry_size = single_entry_size, max_entry_size = single_entry_size, entry_correlation_sd = 0, target_num_rows_data = 1e5)