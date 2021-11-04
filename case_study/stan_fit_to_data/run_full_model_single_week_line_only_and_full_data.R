# Load packages
library(shinystan)
library(rstan)
library(dplyr)

run_stanfit_single_week <- function (week){
full_model_iter <- 20000
line_model_iter <- 20000
warmup_iter <- 5000
data_filename <- paste0('furniture_data_2020_week_', week, '_inspected')
data <- data %>% mutate(Documentation = as.numeric(Documentation))




##### LINE MODE ANALYSIS ######

data_for_analysis <- data

data <- data_for_analysis %>% filter(Mode == 'Line')

intercept_data <- data$RecordIntercept

# This code gives every entry a unique number between 1 and number of unique entries
entry_data <- data$Entry
prev_entry <- entry_data[1]
for (i in 2:length(entry_data)){
  c_entry <- entry_data[i]
  if (c_entry > prev_entry + 1){
    entry_data[entry_data==c_entry] <- prev_entry + 1
  }
  prev_entry <- entry_data[i]
}

num_entries <- length(unique(entry_data))
stan_data <- list(num_rows = nrow(data),
                  Item_class = data$Type,
                  Num_item_classes = max(unique(data$Type)),
                  Document = data$Documentation,
                  num_unique_Entry = num_entries,
                  Entry = entry_data,
                  num_countries = length(unique(data$Country)),
                  country = data$Country,
                  Record_intercept = intercept_data)


mean_intercept_naive_freq <- qlogis(mean(data$RecordIntercept))
num_item_types <- max(unique(data$Type))
num_countries <- length(unique(data$Country))

init_fun <- function(...) list(p=rnorm(n=num_item_types, mean_intercept_naive_freq,.3),
                               country_effect = rnorm(n=num_countries, 0, .1),
                               beta_doc=rnorm(n=1, 0,.1),
                               sigma_entry=runif(n=1, min = .01, max = .05),
                               entry_effect=integer(num_entries))

fit_line <- stan(
  file = "stan_code/full_line_only_model.stan",
  data = stan_data,
  chains = 8,
  warmup = warmup_iter,
  iter = full_model_iter,
  cores = 8,
  refresh = 100,
  control = list(adapt_delta = .99, max_treedepth = 10),
  # init_r = .1,
  init = init_fun,
)
print(fit_line)
saveRDS(fit_line, paste0('case_study/stan_fit_to_data/',data_filename,"_fit_line.rds"))



##### ALL DATA ANALYSIS ######

data <- data_for_analysis %>% mutate(Mode = ifelse(Mode == 'Line' | RecordIntercept == 0, 'Line', 'Container'))
# Store row number of each row to recover original order later
data$row_ID <- seq.int(nrow(data))

# Split data into container and line
container_data <- data %>% filter(Mode == 'Container')
line_data <- data %>% filter(Mode == 'Line')

container_data <- container_data %>% mutate(Record_group = Entry)
if (nrow(container_data) == 0){starting_record_index <- 0} else {starting_record_index <- max(container_data$Record_group)}
line_data$Record_group <-seq.int(nrow(line_data)) + starting_record_index

data <- rbind(container_data, line_data)

# Arrange data in original order and
data <- data %>% arrange(row_ID)

intercept_data <- data %>% group_by(Record_group) %>% summarise(Record_intercept=max(RecordIntercept))

# This code calcultes out the start/finish for each container.
n_records <- length(unique(intercept_data$Record_group))
start_indices <- integer(n_records)
end_indices <- integer(n_records)

for (i in seq_along(intercept_data$Record_group)){
  current_record_group <- intercept_data$Record_group[i]
  ind_opts <- which(data$Record_group %in% current_record_group)
  opts_len <- length(ind_opts)
  if (opts_len == 1){
    start_indices[i] <- ind_opts
    end_indices[i] <- ind_opts
  } else {
    start_indices[i] <- ind_opts[1]
    end_indices[i] <- ind_opts[opts_len]
  }
}

# This code gives every entry a unique number between 1 and number of unique entries
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
                  Num_item_classes = max(unique(data$Type)),
                  Document = data$Documentation,
                  num_unique_Entry = num_entries,
                  Entry = entry_data,
                  num_countries = max(unique(data$Country)),
                  country = data$Country,
                  #Record_groups = intercept_data$Record_group,
                  Record_intercept = intercept_data$Record_intercept,
                  Record_index_start = start_indices,
                  Record_index_end = end_indices,
                  num_containers = num_containers,
                  num_lines = num_lines,
                  container_record_index = container_mode_indices_nums,
                  line_record_index = line_mode_indices_nums,
                  entry_size = entry_size)

mean_intercept_naive_freq <- qlogis(mean(data$RecordIntercept))
num_item_types <- length(unique(data$Type))
num_countries <- length(unique(data$Country))

init_fun <- function(...) list(p=rnorm(n=num_item_types, mean_intercept_naive_freq,.3),
                               country_effect = rnorm(n=num_countries, 0, .1),
                               beta_doc=rnorm(n=1, 0,.1),
                               sigma_entry=runif(n=1, min = .01, max = .05),
                               entry_effect=integer(num_entries))

fit <- stan(
  file = "stan_code/full_container_line_model.stan",
  data = stan_data,
  chains = 8,
  warmup = warmup_iter,
  iter = line_model_iter,
  cores = 8,
  refresh = 100,
  control = list(adapt_delta = .99, max_treedepth = 10),
  # init_r = .1,
  init = init_fun,
)

print(fit)
saveRDS(fit, paste0('case_study/stan_fit_to_data/',data_filename,"_fit_All.rds"))
# fit1000 <- readRDS("fit_1000_correlation.rds")
}

for (i in c(1,2,3)){
  run_stanfit_single_week(i)
}