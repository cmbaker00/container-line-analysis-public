source(file.path('simulation_study', 'simulation_stan_analysis', 'fit_simulated_data_full_model.R'))
source(file.path('simulation_study', 'simulation_naming_functions.R'))

#' Load and subset simulation data
#' 
#' Takes input parameters to specify which input data to load and how much data
#' to extract from it, and outputs an RDA file with that subset of data
#' @param min_entry_size, int
#' @param max_entry_size, int
#' @param sd, 
#' @param target_num_rows_data
#' @param num_line_data, int, number of line mode rows to extract
#' @param num_container_data, int, number of container mode rows to extract
#' @return file name of output written
load_and_subset_sim_data = function(min_entry_size, 
                                    max_entry_size,
                                    sd,
                                    target_num_rows_data,
                                    num_line_data,
                                    num_container_data) {
  
  # open full data frame
  data_filename <- gen_filename_simulate_data(min_entry_size,max_entry_size,sd,target_num_rows_data)
  load_sim_data <- read.csv(data_filename)
  
  # check that the number of rows for both line and contianer don't exceed the max
  max_line = length(load_sim_data %>% filter(Mode == 'Line') %>% pull(Mode))
  max_container <- length(load_sim_data %>% filter(Mode == 'Container') %>% pull(Mode))
  
  if(num_line_data > max_line){
    message(paste0("Input number of lines ", num_line_data, " > max=", max_line, ". Setting to max..." ))
    num_line_data <- max_line
  }
  if(num_container_data > max_container){
    message(paste0("Input number of containers ", num_container_data, " > max=", max_container, ". Setting to max..." ))
    num_container_data <- max_container
  }
  
  # extract line_rows of line data and container_rows of container data
  line_data <- load_sim_data %>% filter(Mode == 'Line') %>% head(num_line_data)
  container_data <- load_sim_data %>% filter(Mode == 'Container') %>% head(num_container_data)
  load_sim_data <- rbind(line_data, container_data)

  #load_sim_data <- load_sim_data %>% mutate(Country=1)
  #load_sim_data <- load_sim_data %>% mutate(Entry=1)
  
  fit_summary_df <- run_stan_simulated_data(load_sim_data,
                                            data_filename,
                                            entry_correlation_flag = FALSE
  )

  output_filename = gen_filename_stanfit_simulated_data(min_entry_size, max_entry_size,sd,num_line_data, num_container_data)
  
  saveRDS(fit_summary_df, output_filename)
  output_filename
}