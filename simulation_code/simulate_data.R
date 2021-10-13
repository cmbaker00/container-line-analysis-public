library(tidyverse)
library(optimx)

simulation_name <- 'container_line_simulated_data_correlations_country' # A name that is used as part of the save filename

# The true probability of interception of each item type
true_intercept_probability <- qlogis(c(.01, .05, .1, .2, .5, .7, .9, .95))

num_types <- length(true_intercept_probability) # number of item types

# probability of inspection. Currently set to 1 so everything is inspects. However,
# this could be altered in future
true_inspect_probability <- integer(num_types) + 1


prob_line <- 0.25 # probability that an entry is in line mode
target_num_rows_data <- 100 # number of rows of data to generate

pr_doc <- .2 # probability that each row of data has documentation
doc_effect <- 1 # The effect on the probability of interception if there is documetation
entry_correlation_sd <- .25

#country
country_effects <- c(.5, -1, .25)
num_countries <- length(country_effects)

# A check that the length of the inspect and intercept probability lists are the same
if (length(true_inspect_probability) != num_types){
  stop("Inspect and defect probability lists must be the same length")
}

  min_entry_size <- 2
  max_entry_size <- 5


  create_row_of_data <- function (doc, entry_effect){
    # create_row_of_data creates the data for a single row of data
    item_type <- sample.int(num_types, 1) # The item type is selected randomly
    item_country <- sample.int(num_countries, 1) #random country
    item_inspect <- runif(1) < true_inspect_probability[item_type] # Draw whether it is inspected
    if (item_inspect == TRUE){
      # If it has documentation, set the current_doc_effect to the true value, otherwise set it to zero
      if (doc == TRUE){
        current_doc_effect <- doc_effect
      } else {
        current_doc_effect <- 0
      }
      # Store the probability of interception. If current_doc_effect is zero, then this line does nothing.
      # If there is documenation, then the probability of inteception is reduced, using the logistic function
      current_intercept_pr <- plogis(true_intercept_probability[item_type] - current_doc_effect + country_effects[item_country] + entry_effect)

      item_intercept <- runif(1) < current_intercept_pr # Draw whether it was intercepted
    } else {
      item_intercept <- FALSE # If not inspected, intercept is false.
    }
    return(c(item_type, item_country, item_inspect, item_intercept))
  }

  add_entry_to_df <- function(entry_true_data, mode, current_container, line, doc, entry_effect, current_rows, df){
    # Adds the data for a row to the dataframe
    if (current_rows == 0 & line == 1){
      # If the dataframe is empty, then create it
      # Note, at this stage True and Record are identical, and these are adjusted later, for rows in container mode.
      test_data <- data.frame(Type = entry_true_data[1],
                              Mode=mode,
                              Container=current_container,
                              Line=line,
                              Documentation=doc,
                              Country=entry_true_data[2],
                              Entry_correlation=entry_effect,
                              TrueInspect=entry_true_data[3],
                              TrueIntercept=entry_true_data[4],
                              RecordInspect=entry_true_data[3],
                              RecordIntercept=entry_true_data[4])
    } else {
      test_data <- rbind(df, c(entry_true_data[1], mode, current_container,line, doc, entry_true_data[2], entry_effect,
                               entry_true_data[3],
                               entry_true_data[4],
                               entry_true_data[3],
                               entry_true_data[4]))
    }
    return(test_data)
  }

  current_rows <- 0 # Set the num rows of data to zero
  current_entry <- 1 # Initialise with entry number 1
  simulated_data <- NULL # Initialise with data = null

  directions_time <- c()
  directions_index <- c()
  while (current_rows < target_num_rows_data){
    # Add rows of data until the target number is reached

    # Draw the entry size, which is the number of rows of data in the entry
    entry_size <- sample.int(max_entry_size - (min_entry_size - 1), 1) + (min_entry_size - 1)

    # Draw whether the entry is in line or container mode
    if (runif(1) < prob_line){
      mode <- 'Line'
    } else {
      mode <- 'Container'
    }

    # For the number of rows of data in the entry, create the data and add it to the data frame
    current_entry_effect <- rnorm(n=1, mean=0, sd=entry_correlation_sd)
    simulated_data_single_entry <- NULL # Initialise with data = null
    for (i in 1:entry_size){
      doc <- runif(1) <pr_doc # Draw whether there is documentaion
      entry_true_data <- create_row_of_data(doc, entry_effect = current_entry_effect) # Create the data

      # Add the new row to the dataframe
      simulated_data_single_entry <- add_entry_to_df(entry_true_data, mode=mode, current_container=current_entry, line=i,
                                        doc=doc, entry_effect = current_entry_effect,
                                        current_rows=0, df=simulated_data_single_entry)
    }

    if (mode == 'Container'){
      # If the entry is in container mode, if any row of data is inspected, all rows of data are set to inspected.
      # Similarly, if a row is intercpeted, all rows in the entry is set to intercepted.
      if (any(simulated_data_single_entry$TrueInspect == 1)){
        simulated_data_single_entry$RecordInspect <- 1
        if (any(simulated_data_single_entry$TrueIntercept == 1)){
          simulated_data_single_entry$RecordIntercept <- 1
        }
      }
    }
    if (is.null(simulated_data)){
      simulated_data <- simulated_data_single_entry
    } else {
      simulated_data <- rbind(simulated_data, simulated_data_single_entry)
    }

    current_entry <- current_entry + 1 # Increment the entry number
    current_rows <- nrow(simulated_data) # Update the current number of rows of data.
    print(current_rows)
  }
  
  # TODO rename container -> entry throughout simulation --------------------
  simulated_data <- simulated_data %>% rename(Entry = Container) 


# TODO properly remove 'inspected' from the simulation, by making  --------

  simulated_data <- simulated_data %>% select(-TrueInspect, -RecordInspect)
  
  write.csv(simulated_data, paste0('../data/', simulation_name, min_entry_size, '_', max_entry_size,
                                   '_rows_', target_num_rows_data, '.csv'), row.names = FALSE)
  write.csv(simulated_data, paste0('data/', simulation_name, min_entry_size, '_', max_entry_size,
                                   '_rows_', target_num_rows_data, '.csv'), row.names = FALSE)
