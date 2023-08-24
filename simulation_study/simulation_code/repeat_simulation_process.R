# process repeat simulations

library(tidyverse)
library(janitor)

source(here::here("simulation_study", "simulation_naming_functions.R"))

DATA_DIR = here::here("simulation_study", "new_sim_results")
OUT_NAME = "combined_simulated_data"

summary_data = data.frame(matrix(nrow = 0, ncol = 0)) 

extract_params_new <- function (fname) {
    params = basename(fname)
    params = str_split(params, pattern = "_")[[1]]
    list (num_line = as.numeric(params[4]), num_container = as.numeric(params[6]))
}

for(file in list.files(DATA_DIR)){
  params = extract_params_new(file)
  if(!is.null(params)){
    curr = readRDS(file.path(DATA_DIR, file)) %>%
      rownames_to_column(var = "param_name") %>%
      janitor::clean_names() %>%
      select(variable, mean, sd, rhat) %>%
      mutate(
        num_line = params$num_line,
        num_container = params$num_container
      ) %>%
      relocate(variable, num_line, num_container)
    
    summary_data = rbind(summary_data, curr)
  }
}
rm(curr, file, params)

saveRDS(summary_data, file.path('simulation_study/collated_sim_results', OUT_NAME))

colnames(summary_data)

aggd_estimates <- summary_data %>% 
  group_by(variable, num_line, num_container) %>% 
  summarise(summary_mean = mean(mean), summary_sd = sd(mean), summary_5 = quantile(mean,probs=c(.05)), summary_95 = quantile(mean,probs=c(.95)))

saveRDS(aggd_estimates, 'simulation_study/collated_sim_results/aggregated_simulations_quantiles')


knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(boot) # for inv.logit function
library(RColorBrewer) # for colour schemes in plots
library(patchwork)
library(xtable)

summary_data_all <- readRDS('simulation_study/collated_sim_results/aggregated_simulations_quantiles')
colnames(summary_data_all) <- c('param_name', 'num_line', 'num_container', 'summary_mean', 'summary_sd', 'summary_5', 'summary_95')

summary_data <- subset(summary_data_all, param_name != 'lp__' & param_name != 'entry_effect[1]' & param_name != 'sigma_entry')

path <- "visualisations/new_figures/"
summary_data = summary_data %>%
  mutate(
    num_total_rows = num_line + num_container,
    percentage_container_data = num_container/num_total_rows
    )

p_exact_vals = qlogis(c(0.001, 0.01, 0.02, 0.05, .2))
exact_df = data.frame(
  param_type = c(rep("p_intercept", length(p_exact_vals)), rep("country_effect", 3), "beta_doc"),
  param_name = c(paste0("p_intercept[", seq(length(p_exact_vals)), "]"), paste0("country_effect[", seq(3), "]"), "beta_doc"),
  yintercept = c(p_exact_vals, .5, -1, .25, -1)
)
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nrow(exact_df))

# names(summary_data)

{
  df = summary_data %>%
    mutate(
      param_type = case_when(
        gsub("[^a-zA-Z]", "", param_name) == "pintercept" ~ "p_intercept",
        gsub("[^a-zA-Z]", "", param_name) == "countryeffect" ~ "country_effect",
        TRUE ~ "beta_doc"
      )
    ) %>%
    relocate(param_name, param_type)
  
  # New facet labels
  facet_labs <- c("beta", "delta", "alpha") # new names
  names(facet_labs) <- c("beta_doc", "country_effect", "p_intercept") # old names
  
  # New legend labels
  legend_labs = c("beta", paste0("delta_", c(1, 2, 3)), paste0("alpha_", seq(1,5)))
  
  ggplot(df, aes(x = num_total_rows, y = summary_mean, color = param_name, fill = param_name)) +
    geom_point() +
    geom_line(linetype = "dashed") +
    geom_hline(data = exact_df, aes(yintercept = yintercept, color = param_name)) +
    scale_color_manual(values=mycolors, 
                       name="Parameters",
                       labels=legend_labs) +
    scale_fill_manual(values=mycolors,
                      name="Parameters",
                      labels=legend_labs) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_ribbon(aes(ymax=summary_95, ymin=summary_5), alpha=0.2, linetype = 0) +
    facet_wrap(.~param_type,
               labeller = labeller(param_type = facet_labs)) +
    labs(title = "Mean parameter estimates by amount of data",
         subtitle = paste0("Min entry size: ", first(df$min_entry_size),
                           ", Max entry size: ", first(df$max_entry_size),
                           ", Entry correlation: ", first(df$entry_correlation_sd),
                           ", Percentage of container data: ", first(df$percentage_container_data)*100, "%")) +
    xlab('Total number of lines') + ylab('Parameter value')

  ggsave(paste0(path,'simulation_estimates_random_effect_quantiles.pdf'), width=8, height=8)
}