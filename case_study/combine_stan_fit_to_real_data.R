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
#' Extract parameters from file name of raw data e.g. furniture_data_2020_week_2_inspected_fit_All.RDA
#' @param filename, string with the filename (with or without file path and extension)
#' @return list of parameter values
extract_params_stanfit_raw_data = function(filename, end_string){
  start_string = "furniture_data_2020_week_"
  middle_string = "inspected_fit_"
  if(end_string %in% c("line.RDA", "All.RDA")){
    end_string = ".RDA"
  }
  format_check = all(sapply(c(start_string, middle_string, end_string), function (x) str_detect(filename, x)))
  
  params = basename(filename)
  params = str_remove(params, start_string)
  params = str_remove(params, middle_string)
  params = str_remove(params, end_string)
  
  params = str_split(params, pattern = "_")[[1]]
  params = params[params != ""]
  
  if((length(params)!= 2) | !format_check){
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
old_raw_data = data.frame(matrix(nrow = 0, ncol = 0)) 

for(file in list.files(DATA_DIR)){
  tail_string = tail(str_split(file, "_")[[1]], n=1)
  params = extract_params_stanfit_raw_data(file, tail_string)
  if(!is.null(params)){
    curr = readRDS(file.path(DATA_DIR, file)) %>%
      as.data.frame %>%
      rownames_to_column(var = "param_name") %>%
      janitor::clean_names() %>%
      select(param_name, summary_mean, summary_sd, summary_rhat) %>%
      mutate(
        week = params$week,
        type = params$type
      ) %>%
      relocate(param_name, week, type) 
    
    if(tail_string == "old.Rda"){
      old_raw_data = rbind(old_raw_data, curr)
    } else if(tail_string %in% c("line.RDA", "All.RDA")){
      raw_data = rbind(raw_data, curr)
    } else{
      message(paste("Unexpected value for end of string:", tail_string))
    }
      
    
  }
}
rm(curr, file, params)


raw_data = raw_data %>%
  arrange(week)


