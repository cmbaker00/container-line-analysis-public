

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

gen_filename_stanfit_simulated_data <- function (min_entry_size, max_entry_size,
                                        entry_correlation_sd, num_line_row, num_container_rows,
                                                 include_path = TRUE, include_extension = TRUE){
  if (include_path){rel_path <- 'simulation_study/simulation_stan_results/'} else {rel_path <- ''}
  if (include_extension){end_str <- '.Rda'} else {end_str <- ''}

  paste0(rel_path,"entrysize_", min_entry_size,"_",max_entry_size,
    "_corr_sd_",entry_correlation_sd,"_num_line_",num_line_row,"_num_container_",num_container_rows,
         "_fit_summary",end_str)
}

