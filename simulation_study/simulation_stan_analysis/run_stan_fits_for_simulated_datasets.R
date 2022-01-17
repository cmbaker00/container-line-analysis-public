source(file.path('simulation_study', 'simulation_stan_analysis', 'subset_simulated_data.R'))

entry_size_list <- c(1,2,5,10,20)
entry_size_min <- entry_size_list[1]
entry_size_max <- entry_size_list[length(entry_size_list)]

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


## Line data only, no SD, varying amount of data
num_scenarios <- 5
input_params_line_only_no_sd = data.frame(
  min_entry_size = rep(entry_size_min, num_scenarios),
  max_entry_size = rep(entry_size_max, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = c(1000, 2000, 3000, 4000, 5000),
  num_container_data = rep(0, num_scenarios),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_line_only_no_sd)


## Container data only, no SD, varying amount of data
num_scenarios <- 5
input_params_container_only_no_sd = data.frame(
  min_entry_size = rep(entry_size_min, num_scenarios),
  max_entry_size = rep(entry_size_min, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = rep(0, num_scenarios),
  num_container_data = c(1000, 2000, 3000, 4000, 5000),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_container_only_no_sd)


# Equal mix of container and line, no SD, varying amount of data
num_scenarios <- 5
input_params_all_no_sd = data.frame(
  min_entry_size = rep(entry_size_min, num_scenarios),
  max_entry_size = rep(entry_size_max, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = c(1000, 2000, 3000, 4000, 5000),
  num_container_data = c(1000, 2000, 3000, 4000, 5000),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_all_no_sd)


# Equal mix of container and line, no SD, varying amount of data - 10k + ROWS


num_rows <- c(10000, 15000, 20000, 25000)
num_scenarios <- length(num_rows)
input_params_all_no_sd = data.frame(
  min_entry_size = rep(entry_size_min, num_scenarios),
  max_entry_size = rep(entry_size_max, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = num_rows,
  num_container_data = num_rows,
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_all_no_sd)


# Container only, no SD, varying amount of data


num_rows <- c(5000, 10000, 15000, 20000, 25000)
num_scenarios <- length(num_rows)
input_params_all_no_sd = data.frame(
  min_entry_size = rep(entry_size_min, num_scenarios),
  max_entry_size = rep(entry_size_max, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data = 0,
  num_container_data = num_rows,
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_all_no_sd)


# Constant 1k rows of data, no SD, varying proportion of line/container data
num_scenarios <- 9
input_params_all_no_sd_change_combination = data.frame(
  min_entry_size = rep(entry_size_min, num_scenarios),
  max_entry_size = rep(entry_size_max, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data =      c(900, 800, 700, 600, 500, 400, 300, 200, 100),
  num_container_data = c(100, 200, 300, 400, 500, 600, 700, 800, 900),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_all_no_sd_change_combination)




# Chanign entry sizes, runnign with equal mix of line and container, and contaienr only. 500 to 2500 of each in steps of 500
for (fixed_entry_size in entry_size_list){
num_scenarios <- 10
input_params_all_no_sd_change_combination = data.frame(
  min_entry_size = rep(fixed_entry_size, num_scenarios),
  max_entry_size = rep(fixed_entry_size, num_scenarios),
  sd = rep(0, num_scenarios),
  target_num_rows_data = rep('1e+05', num_scenarios),
  num_line_data =      c(seq(250,1250,by=250),rep(0,5)),
  num_container_data = c(seq(250,1250,by=250),seq(500,2500,by=500)),
  entry_random_effect = rep(FALSE, num_scenarios)
)

loop_over_input_params_fit_stan(input_params_all_no_sd_change_combination)

}





# Chanign entry sizes, contaienr only. 3000 to 5000 of each in steps of 500
for (fixed_entry_size in entry_size_list){
  num_scenarios <- 5
  input_params_all_no_sd_change_combination = data.frame(
    min_entry_size = rep(fixed_entry_size, num_scenarios),
    max_entry_size = rep(fixed_entry_size, num_scenarios),
    sd = rep(0, num_scenarios),
    target_num_rows_data = rep('1e+05', num_scenarios),
    num_line_data =      c(rep(0,5)),
    num_container_data = c(seq(3000,5000,by=500)),
    entry_random_effect = rep(FALSE, num_scenarios)
  )
  
  loop_over_input_params_fit_stan(input_params_all_no_sd_change_combination)
  
}





# Chanign entry sizes, only line 2000 to 5000 of each in steps of 1000
for (fixed_entry_size in entry_size_list){
  if (fixed_entry_size == 2) {
    num_scenarios <- 4
    input_params_all_no_sd_change_combination = data.frame(
      min_entry_size = rep(fixed_entry_size, num_scenarios),
      max_entry_size = rep(fixed_entry_size, num_scenarios),
      sd = rep(0, num_scenarios),
      target_num_rows_data = rep('1e+05', num_scenarios),
      num_line_data = c(seq(2000, 5000, by = 1000)),
      num_container_data = c(rep(0, 4)),
      entry_random_effect = rep(FALSE, num_scenarios)
    )
    
    loop_over_input_params_fit_stan(input_params_all_no_sd_change_combination)
  }
}
# Chanign entry sizes, only line 500 to 2500 of each in steps of 500
for (fixed_entry_size in entry_size_list){
  if (fixed_entry_size == 2) {
    num_scenarios <- 5
    input_params_all_no_sd_change_combination = data.frame(
      min_entry_size = rep(fixed_entry_size, num_scenarios),
      max_entry_size = rep(fixed_entry_size, num_scenarios),
      sd = rep(0, num_scenarios),
      target_num_rows_data = rep('1e+05', num_scenarios),
      num_line_data = c(seq(500, 2500, by = 500)),
      num_container_data = c(rep(0, num_scenarios)),
      entry_random_effect = rep(FALSE, num_scenarios)
    )
    
    loop_over_input_params_fit_stan(input_params_all_no_sd_change_combination)
  }
}
