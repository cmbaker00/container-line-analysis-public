
source('simulation_study/simulation_stan_analysis/fit_simulated_data_full_model.R')
source('simulation_study/simulation_naming_functions.R')

# set parameters
min_entry_size <- 2
max_entry_size <- 5
sd <- 0
target_num_rows_data <- '1e+05'
line_rows <- 10
container_rows <- 5  

# open full data frame
data_filename <- gen_filename_simulate_data(min_entry_size,max_entry_size,sd,1e5)
load_sim_data <- read.csv(data_filename)

# check that the number of rows for both line and contianer don't exceed the max
max_line = length(load_sim_data %>% filter(Mode == 'Line') %>% pull(Mode))
max_container <- length(load_sim_data %>% filter(Mode == 'Container') %>% pull(Mode))

if(line_rows > max_line){
  message(paste0("Input number of lines ", line_rows, " > max=", max_line, ". Setting to max..." ))
  line_rows <- max_line
}
if(container_rows > max_container){
  message(paste0("Input number of containers ", container_rows, " > max=", max_container, ". Setting to max..." ))
  container_rows <- max_container
}

# extract line_rows of line data and container_rows of container data
line_data <- load_sim_data %>% filter(Mode == 'Line') %>% head(line_rows)
container_data <- load_sim_data %>% filter(Mode == 'Container') %>% head(container_rows)
load_sim_data <- rbind(line_data, container_data)

#load_sim_data <- load_sim_data %>% mutate(Country=1)
#load_sim_data <- load_sim_data %>% mutate(Entry=1)

fit_summary_df <- run_stan_simulated_data(load_sim_data,
                        data_filename,
                        entry_correlation_flag = FALSE
)
num_data_rows <- load_sim_data %>% group_by(Mode) %>% summarise(n=n())

num_line_data <- num_data_rows[num_data_rows$Mode=='Line',]$n
num_container_data <- num_data_rows[num_data_rows$Mode=='Container',]$n

output_filename = gen_filename_stanfit_simulated_data(min_entry_size, max_entry_size,sd,num_line_data, num_container_data)

# saveRDS(fit_summary_df, output_filename)
print(output_filename)
