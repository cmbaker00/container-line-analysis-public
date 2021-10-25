#' Combine simulation data
#'
#' Combine all the simulation data in simulation_study/simulation_stan_results
#' 

library(tidyverse)
library(janitor)
source(file.path("simulation_study", "simulation_naming_functions.R"))

DATA_DIR = file.path("simulation_study", "simulation_stan_results")
OUT_NAME = "combined_simulated_data.Rda"

summary_data = data.frame(matrix(nrow = 0, ncol = 0)) 

for(file in list.files(DATA_DIR)){
  params = extract_params_stanfit_simulated_data(file)
  if(!is.null(params)){
    curr = readRDS(file.path(DATA_DIR, file)) %>%
      rownames_to_column(var = "param_name") %>%
      janitor::clean_names() %>%
      select(param_name, summary_mean, summary_sd, summary_rhat) %>%
      mutate(
        min_entry_size = params$min_entry_size,
        max_entry_size = params$max_entry_size,
        entry_correlation_sd = params$entry_correlation_sd,
        num_line_rows = params$num_line_rows,
        num_container_rows = params$num_container_rows
      ) %>%
      relocate(param_name, min_entry_size, max_entry_size, entry_correlation_sd, num_line_rows, num_container_rows)
    
    summary_data = rbind(summary_data, curr)
  }
}

saveRDS(summary_data, file.path(DATA_DIR, OUT_NAME))

# Read in output to check
# rm(summary_data)
# summary_data = readRDS(file.path(DATA_DIR, OUT_NAME))

