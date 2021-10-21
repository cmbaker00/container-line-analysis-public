library(tidyverse)

gen_filename_simulate_data <- function (min_entry_size, max_entry_size,
                                        entry_correlation_sd,target_num_rows_data,
                                        include_path = TRUE, include_extension = TRUE
){
  if (include_path){rel_path <- 'simulation_study/simulation_data/'} else {rel_path <- ''}
  if (include_extension){end_str <- '.csv'} else {end_str <- ''}
  paste0(rel_path, 'entrysize_', min_entry_size, '_', max_entry_size,
         '_entry_corr', entry_correlation_sd,'_rows_', target_num_rows_data, end_str
  )
}

#' Generate file name based on Stan fit parameters
#' 
#' @param min_entry_size, int
#' @param max_entry_size, int
#' @param entry_correlation_sd, int
#' @param num_line_rows, int or empty
#' @param num_container_rows, int or empty
#' @param include_path, bool, true (default) sets file path to simulation_study/simulation_stan_results
#' @param include_extension, bool, true (default) sets file extension to ".Rda", otherwise no extension
#' @return file path or file name based on input parameters
gen_filename_stanfit_simulated_data <- function (min_entry_size, max_entry_size,
                                                 entry_correlation_sd, num_line_rows, num_container_rows,
                                                 include_path = TRUE, include_extension = TRUE){
  
  # set file path
  rel_path = ifelse(include_path, file.path("simulation_study", "simulation_stan_results"), "")
  # set extension
  end_str = ifelse(include_extension, ".Rda", "")
  # if number of line or container rows are empty, set to zero
  num_line_rows = ifelse(is_empty(num_line_rows), 0, num_line_rows)
  num_container_rows = ifelse(is_empty(num_container_rows), 0, num_container_rows)
  
  # return file name
  paste0(rel_path, "entrysize_", min_entry_size,"_",max_entry_size,
         "_corr_sd_",entry_correlation_sd,"_num_line_",num_line_rows,"_num_container_",num_container_rows,
         "_fit_summary",end_str)
}

