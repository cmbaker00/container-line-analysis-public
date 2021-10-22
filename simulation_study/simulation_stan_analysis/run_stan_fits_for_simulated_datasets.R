source(file.path('simulation_study', 'simulation_stan_analysis', 'subset_simulated_data.R'))


loop_over_input_params_fit_stan <- function (input_params) {
  # load and subset data
  for (i in 1:nrow(input_params)) {
    param = input_params[i,]
    print(load_and_subset_sim_data(param$min_entry_size,
                                   param$max_entry_size,
                                   param$sd,
                                   param$target_num_rows_data,
                                   param$num_line_data,
                                   param$num_container_data,
                                   param$entry_random_effect
    )
    )
  }

}


# set parameters


num_scenarios <- 5
input_params_line_only_no_sd = data.frame(
  min_entry_size = rep(2, num_scenarios),
  max_entry_size = rep(5, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = c(1000, 2000, 3000, 4000, 5000),
  num_container_data = rep(0, num_scenarios),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_line_only_no_sd)

num_scenarios <- 5
input_params_container_only_no_sd = data.frame(
  min_entry_size = rep(2, num_scenarios),
  max_entry_size = rep(5, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = rep(0, num_scenarios),
  num_container_data = c(1000, 2000, 3000, 4000, 5000),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_container_only_no_sd)

num_scenarios <- 5
input_params_all_no_sd = data.frame(
  min_entry_size = rep(2, num_scenarios),
  max_entry_size = rep(5, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = c(1000, 2000, 3000, 4000, 5000),
  num_container_data = c(1000, 2000, 3000, 4000, 5000),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_all_no_sd)