# Load packages
library(shinystan)
library(rstan)
library(dplyr)

#set working directory to file 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load_sim_data <- read.csv(paste0('../data/container_line_simulated_data_correlations2_5_rows_1000.csv'))
# load_sim_data <- read.csv(paste0('../data/container_line_simulated_data_p05_container_size4_4_rows_1e+05.csv'))
load_sim_data <- load_sim_data %>% rename(Entry = Container)

# load_sim_data <- read.csv(paste0('../data/container_line_simulated_data_correlations2_5_rows_1e+05.csv'))
# load_sim_data <- load_sim_data[1:10000,]
load_sim_data <- read.csv(paste0('../data/container_line_simulated_data_correlations2_2_rows_1e+05.csv'))
load_sim_data <- load_sim_data %>% filter(Mode == "Line" | Entry <= 10)
load_sim_data <- load_sim_data[1:10000,]

data <- load_sim_data

data <- data %>% mutate(Documentation = as.numeric(Documentation))
data <- data %>% mutate(Mode = ifelse(Mode == 'Line' | RecordIntercept == 0, 'Line', 'Container'))
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

for (i in 1:length(intercept_data$Record_group)){
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


entry_data <- data$Entry
prev_entry = entry_data[1]
for (i in 2:length(entry_data)){
  c_entry <- entry_data[i]
  if (c_entry > prev_entry + 1){
    entry_data[entry_data==c_entry] <- prev_entry + 1
  }
  prev_entry <- entry_data[i]
}
# entry_data[1:100] <- 1
# entry_data[101:length(entry_data)] <- 2

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
                  #Record_groups = intercept_data$Record_group,
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
  file = "basic_model_stan_doc_correlation_v3b.stan",
  data = stan_data,
  chains = 4,
  warmup = 200,
  iter = 1000,
  cores = 4,
  refresh = 500, 
  control = list(adapt_delta = .8),
  init_r = .1,
  # init = init_fun,
)
print(fit)
# saveRDS(fit, "fit_100000_correlation.rds")
# fit1000 <- readRDS("fit_1000_correlation.rds")

stan_data_simple <- list(num_rows = nrow(data),
                         num_records = n_records,
                         Item_class = data$Type,
                         Num_item_classes = length(unique(data$Type)),
                         Document = data$Documentation,
                         num_unique_Entry = 2,
                         Entry = c(integer(length(data$Entry)/2)+1, integer(5+length(data$Entry)/2)+2)[1:length(data$Entry)],
                         # num_unique_Entry = 1,
                         # Entry = integer(length(data$Entry))+1,
                         #Record_groups = intercept_data$Record_group,
                         Record_intercept = intercept_data$Record_intercept,
                         Record_index_start = start_indices,
                         Record_index_end = end_indices)

init_fun <- function(...) list(p=integer(8)+.01,beta_doc=0, sigma_entry=.01, entry_effect=c(0,0))

# init_fun <- function(...) list(p=integer(8)+.01,beta_doc=0, sigma_entry=.01, entry_effect=c(0))

fit_simple <- stan(
  file = "basic_model_stan_doc_correlation.stan",
  data = stan_data_simple,
  chains = 1,
  warmup = 1000,
  iter = 5000,
  cores = 4,
  refresh = 100, 
  #init_r = .1,
  init = init_fun,
  control = list(adapt_delta = 0.99)
)
print(fit_simple)
