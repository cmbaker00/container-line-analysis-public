# Load packages
library(rstan)
library(dplyr)

data_filename <- 'test_run'
load_sim_data <- read.csv(paste0('simulation_study/simulation_data/container_line_simulated_data_correlations_country2_5_rows_10000.csv'))
load_sim_data <- load_sim_data %>% mutate(Country=1)
data <- load_sim_data

data <- data %>% mutate(Documentation = as.numeric(Documentation)) # Ensure Documentation is numeric

# Change container mode data that is not intercepted to be line mode for analsysis
data <- data %>% mutate(Mode = ifelse(Mode == 'Line' | RecordIntercept == 0, 'Line', 'Container'))

# Store row number of each row to recover original order later
data$row_ID <- seq.int(nrow(data))

### Process data for analysis ###

# Split data into container and line
container_data <- data %>% filter(Mode == 'Container')
line_data <- data %>% filter(Mode == 'Line')

# Record_group is groupings of lines, where inteceptions are recorded against the group.
# For line mode, every row of data is it's own unique record group
# For container mode data, each entry is its own record group
# This code gives each container-mode entry a Record_group equal to its entry number, and then the line
# mode data is given numbers, starting after the container mode data.
container_data <- container_data %>% mutate(Record_group = Entry)
if (nrow(container_data) == 0){starting_record_index <- 0} else {starting_record_index <- max(container_data$Record_group)}
line_data$Record_group <-seq.int(nrow(line_data)) + starting_record_index

data <- rbind(container_data, line_data)

# Arrange data in original order, rather than container data then line data.
# The reason for this is to allow removing the last x rows of data without only removing line data.
data <- data %>% arrange(row_ID)

# Creates a vector of interception data, corresponding to the record groups
intercept_data <- data %>% group_by(Record_group) %>% summarise(Record_intercept=max(RecordIntercept))

# This code gets the start/finish for each record group.
n_records <- length(unique(intercept_data$Record_group)) #number of groups
start_indices <- integer(n_records) # To store the start indices
end_indices <- integer(n_records) # To store the end indices

for (i in seq_along(intercept_data$Record_group)){
  current_record_group <- intercept_data$Record_group[i]
  ind_opts <- which(data$Record_group %in% current_record_group) # Stores the rows in the record group
  opts_len <- length(ind_opts) # The number
  if (opts_len == 1){
    start_indices[i] <- ind_opts
    end_indices[i] <- ind_opts
  } else {
    start_indices[i] <- ind_opts[1] # Start index is the first in ind_opts
    end_indices[i] <- ind_opts[opts_len] # End index is the last in ind_opts
  }
}

# This code stores a unique entry ID for each entry, from 1 to #entries.
# It makes sure they're number 1, 2, 3 .. etc
entry_data <- data$Entry
prev_entry <- entry_data[1]
for (i in 2:length(entry_data)){
  c_entry <- entry_data[i]
  if (c_entry > prev_entry + 1){
    entry_data[entry_data==c_entry] <- prev_entry + 1
  }
  prev_entry <- entry_data[i]
}

container_true_false <- (end_indices - start_indices) > 0
entry_size <- end_indices - start_indices + 1
all_nums <- seq(length(container_true_false))

container_mode_indices_nums <- all_nums[container_true_false]
line_mode_indices_nums <- all_nums[!container_true_false]

num_containers <- length(container_mode_indices_nums)
num_lines <- length(line_mode_indices_nums)


num_entries <- length(unique(entry_data))
stan_data <- list(num_rows = nrow(data),
                  num_records = n_records,
                  Item_class = data$Type,
                  Num_item_classes = length(unique(data$Type)),
                  Document = data$Documentation,
                  num_unique_Entry = num_entries,
                  Entry = entry_data,
                  num_countries = length(unique(data$Country)),
                  country = data$Country,
                  Record_intercept = intercept_data$Record_intercept,
                  Record_index_start = start_indices,
                  Record_index_end = end_indices,
                  num_containers = num_containers,
                  num_lines = num_lines,
                  container_record_index = container_mode_indices_nums,
                  line_record_index = line_mode_indices_nums,
                  entry_size = entry_size)

# init_fun <- function(...) list(p=runif(n=8,0.01,.4),beta_doc=0, sigma_entry=0.1, entry_effect=integer(num_entries))

fit <- stan(
  file = "stan_code/full_container_line_model.stan",
  data = stan_data,
  chains = 4,
  warmup = 1000,
  iter = 10000,
  cores = 4,
  refresh = 500, 
  control = list(adapt_delta = .8),
  init_r = .1,
  # init = init_fun,
)
print(fit)
fit_summary <- summary(fit)
fit_summary_df <- data.frame(fit_summary)
saveRDS(fit_summary_df, paste0('simulation_study/simulation_stan_results/',data_filename,"_fit_summary.Rda"))
