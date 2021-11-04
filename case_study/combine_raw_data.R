#' Combine raw data
#'
#' Combine all the raw data in case_study/stan_fit_to_data
#' 

library(tidyverse)
library(janitor)

DATA_DIR = here::here("case_study", "stan_fit_to_data")

# ------- Functions ---------------

#' Extract parameters from file name
#' 
#' Extract parameters from file name of raw data e.g. furniture_data_2020_week_2_inspected_fit_All_summary.Rda
#' @param filename, string with the filename (with or without file path and extension)
#' @return list of parameter values
extract_params_stanfit_raw_data = function(filename){
  params = basename(filename)
  params = str_remove(params, "furniture_data_2020_week_")
  params = str_remove(params, "inspected_fit_")
  params = str_remove(params, "_summary.Rda")
  
  params = str_split(params, pattern = "_")[[1]]
  
  if(length(params)!= 2){
    message(paste(filename, "not in correct format"))
    return()
  }
  
  
  list(
    week = as.numeric(params[1]),
    type = tolower(params[2])
  )
}


# --------- Script ---------------

raw_data = data.frame(matrix(nrow = 0, ncol = 0)) 

for(file in list.files(DATA_DIR)){
  params = extract_params_stanfit_raw_data(file)
  if(!is.null(params)){
    curr = readRDS(file.path(DATA_DIR, file))$summary %>%
      as.data.frame %>%
      rownames_to_column(var = "param_name") %>%
      janitor::clean_names() %>%
      select(param_name, mean, sd, rhat) %>%
      mutate(
        week = params$week,
        type = params$type
      ) %>%
      relocate(param_name, week, type) 
    
      raw_data = rbind(raw_data, curr)
  }
}
rm(curr, file, params)


raw_data = raw_data %>%
  arrange(week)


