source(file.path('simulation_study', 'simulation_stan_analysis', 'subset_simulated_data.R'))

# set parameters
input_params = data.frame(
  min_entry_size = rep(2, 3),
  max_entry_size = rep(5, 3),
  sd = c(0, 0.25, 0.25),
  target_num_rows_data = c(rep('1e+05', 2), '1000'),
  num_line_data = c(10, 20, 5),
  num_container_data = c(5, 10, 20)
)

# load and subset data
for(i in 1:nrow(input_params)){
  param = input_params[i, ]
  print(load_and_subset_sim_data(param$min_entry_size, 
                                 param$max_entry_size,
                                 param$sd,
                                 param$target_num_rows_data,
                                 param$num_line_data,
                                 param$num_container_data))
}

