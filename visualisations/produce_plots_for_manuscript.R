knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(boot) # for inv.logit function
library(RColorBrewer) # for colour schemes in plots
library(patchwork)
library(xtable)
# library(scales) this library is used for making percentage scales
source(here::here("simulation_study",
                  "simulation_stan_analysis",
                  "combine_simulated_data.R"))
source(here::here("case_study", "combine_stan_fit_to_real_data.R"))

path <- "visualisations/figures/"
summary_data = summary_data %>%
  mutate(
    num_total_rows = num_line_rows + num_container_rows,
    percentage_container_data = num_container_rows/num_total_rows
    )
  p_exact_vals = qlogis(c(0.001, 0.01, 0.02, 0.05, .2))
exact_df = data.frame(
  param_type = c(rep("p_intercept", length(p_exact_vals)), rep("country_effect", 3), "beta_doc"),
  param_name = c(paste0("p_intercept[", seq(length(p_exact_vals)), "]"), paste0("country_effect[", seq(3), "]"), "beta_doc"),
  yintercept = c(p_exact_vals, .5, -1, .25, -1)
)
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nrow(exact_df))




  {

df = summary_data %>%
  filter(min_entry_size == max_entry_size, !(param_name %in% c("beta_doc", "country_effect[1]", "country_effect[2]", "country_effect[3]","lp__", "entry_effect[1]", "sigma_entry"))) %>%
  mutate(
    `Entry Size` = min_entry_size # have selected out data where min and max entry size are equal
  ) %>%
  filter(`Entry Size` > 1) %>% filter(num_total_rows >= 500, num_total_rows <= 2500)

ggplot(df, aes(x = num_total_rows, y = summary_sd, color = as.factor(percentage_container_data))) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(`Entry Size`~param_name, labeller = labeller(.rows = label_both, .cols = label_value)) +
  scale_color_discrete(name = "Ratio container data") +
  labs(title = "Standard deviation of parameter estimates by amount of data")
ggsave(paste0(path,'sim_study_pint_vs_data_vs_entry_size.pdf'), width = 7, height = 8)

}



{raw_data_2 = raw_data %>%
  pivot_wider(id_cols = c(param_name, week),
              names_from = type,
              values_from = c(summary_mean, summary_sd, summary_rhat)) %>%
  drop_na()

raw_data_2 %>%
  filter(str_detect(param_name, "p_intercept")) %>%
  ggplot(aes(x = summary_mean_all, y = summary_mean_line, color = param_name)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  facet_wrap(.~week, labeller = label_both) +
  theme(legend.position = "none") +
  labs(title = "Comparing estimates of p_intercept[i] for all data and line only data")

  ggsave(paste0(path,'p_int_est_all_vs_line_data.pdf'), width = 8, height = 5)
}




{
df = summary_data %>%
  filter(min_entry_size == 1,
         max_entry_size == 20,
         entry_correlation_sd == 0,
         percentage_container_data == 0.5,
         !(param_name %in% c("lp__", "entry_effect[1]", "sigma_entry"))) %>%
  mutate(
    param_type = case_when(
      gsub("[^a-zA-Z]", "", param_name) == "pintercept" ~ "p_intercept",
      gsub("[^a-zA-Z]", "", param_name) == "countryeffect" ~ "country_effect",
      TRUE ~ "beta_doc"
    )
  ) %>%
  relocate(param_name, param_type)

ggplot(df, aes(x = num_total_rows, y = summary_mean, color = param_name, fill = param_name)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  geom_hline(data = exact_df, aes(yintercept = yintercept, color = param_name)) +
  scale_color_manual(values=mycolors) +
  scale_fill_manual(values=mycolors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymax=summary_mean + summary_sd, ymin=summary_mean - summary_sd), alpha=0.2, linetype = 0) +
  facet_wrap(.~param_type) +
  labs(title = "Mean parameter estimates by amount of data",
       subtitle = paste0("Min entry size: ", first(df$min_entry_size),
                        ", Max entry size: ", first(df$max_entry_size),
                        ", Entry correlation: ", first(df$entry_correlation_sd),
                        ", Percentage of container data: ", first(df$percentage_container_data)*100, "%"))

ggsave(paste0(path,'simulation_estimates.pdf'), width=8, height=8)
}


{
df = summary_data %>%
  filter(min_entry_size == 1,
         max_entry_size == 20,
         entry_correlation_sd == 0,
         num_line_rows == 0,
         !(param_name %in% c("lp__", "entry_effect[1]", "sigma_entry"))) %>%
  mutate(
    param_type = case_when(
      gsub("[^a-zA-Z]", "", param_name) == "pintercept" ~ "p_intercept",
      gsub("[^a-zA-Z]", "", param_name) == "countryeffect" ~ "country_effect",
      TRUE ~ "beta_doc"
    )
  ) %>%
  relocate(param_name, param_type)

ggplot(df, aes(x = num_total_rows, y = summary_mean, color = param_name, fill = param_name)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  geom_hline(data = exact_df, aes(yintercept = yintercept, color = param_name)) +
  scale_color_manual(values=mycolors) +
  scale_fill_manual(values=mycolors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_ribbon(aes(ymax=summary_mean + summary_sd, ymin=summary_mean - summary_sd), alpha=0.2, linetype = 0) +
  facet_wrap(.~param_type) +
  labs(title = "Mean parameter estimates by amount of data",
       subtitle = paste0("Min entry size: ", first(df$min_entry_size),
                        ", Max entry size: ", first(df$max_entry_size),
                        ", Entry correlation: ", first(df$entry_correlation_sd),
                        ", Percentage of container data: ", first(df$percentage_container_data)*100, "%"))

ggsave(paste0(path,'simulation_estimates_container.pdf'), width=8, height=8)
}




  {

  true_intercept_probability <- qlogis(c(0.001, 0.01, 0.02, 0.05, .2))
N = 1000000
set.seed(147)
pint = rnorm(N, -4, 2)
prior_df_int_only = data.frame(
  p = pint)
prior_df_int_only %>%
ggplot(aes(x = p)) +
  geom_histogram(breaks = seq(min(true_intercept_probability)*1.1,2.5,0.0025)) + geom_vline(xintercept = true_intercept_probability)
  labs(title = "Prior without entry effect")
  ggsave(paste0(path,'pint_prior.pdf'), width = 8, height=6)
}

xtable(
raw_data %>%
  pivot_wider(id_cols = c(param_name, week),
              names_from = type,
              values_from = c(summary_mean, summary_sd, summary_rhat)) %>%
  filter_all(any_vars(is.na(.))) %>%
  select(param_name, week, summary_mean_all, summary_sd_all, summary_rhat_all) %>%
  filter(str_detect(param_name, "p_intercept") | str_detect(param_name, "country_effect")))