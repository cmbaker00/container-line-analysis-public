library(tidyverse)
library(dplyr)


full_data <- readRDS('case_study/raw_data/lines-entry-2020-lines-out.RDS')
full_data <- full_data %>% group_by(Entry.ID) %>% mutate(Num_inspect = sum(Inspected)) #%>% ungroup %>% select(-Entry.ID)

full_data <- full_data %>% filter(Num_inspect >= 1)
full_data <- full_data %>% mutate(Tariff.ID.2 = substring(Tariff.ID.4,1,2)) %>% filter(Tariff.ID.2 != '00')


start_date <- as.Date('2020-01-01')
end_date <- as.Date('2020-01-07')
data_summary_df <- data.frame(week = c(), start_date = c(), end_date = c(), perc_container = c(), num_intercept_container_entry = c())
for (i in 0:51){
  start_date <- start_date + 7
  end_date <- end_date + 7
  full_data_week <- full_data %>% filter(Creation.date >= start_date) %>% filter(Creation.date <= end_date)
  num_by_direction <- full_data_week %>% group_by(Direction.mode) %>% summarise(n_by_direciton = n())
  perc_container <- (num_by_direction %>% filter(Direction.mode=='Container'))[[2]]/nrow(full_data_week)
  #full_data_week %>% group_by(Direction.mode) %>% summarise(sum(Non.compliant.inspection))
  num_non_compliant_container_mode_entries <-
    length(unique((full_data_week %>% filter(Direction.mode=='Container', Non.compliant.inspection==TRUE))$Entry.ID))
  data_summary_df <- rbind(data_summary_df, c(i + 1, as.character(start_date), as.character(end_date), perc_container, num_non_compliant_container_mode_entries))
}

colnames(data_summary_df) <- c('week', 'start_date', 'end_date', 'perc_containter','num_container_int')

weeks_to_use <- data_summary_df %>% filter(num_container_int >= 4, perc_containter > 0)


full_data <- full_data %>% arrange(desc(Direction.mode)) %>%
  mutate(Type = match(Tariff.ID.2, unique(Tariff.ID.2))) %>%
  mutate(Country = match(Country.code, unique(Country.code)))

data <- full_data %>% mutate(Entry = match(Entry.ID, unique(Entry.ID))) %>%
  mutate(Type = match(Tariff.ID.2, unique(Tariff.ID.2))) %>%
  mutate(Country = match(Country.code, unique(Country.code))) %>%
  mutate(Documentation = 1 - Non.compliant.documentation) %>%
  mutate(RecordIntercept = Non.compliant.inspection) %>%
  mutate(Mode = Direction.mode) %>%
  group_by(Entry.ID) %>% mutate(Line = match(Line.number, unique(Line.number))) %>% ungroup %>% select(-Entry.ID)

## TODO what is happenign with Entry.ID / Entry here??? In the output data every entry is renamed to 1.

#country_identity <- data %>% group_by(Country.code, Country) %>% summarise(n=n()) %>% select(-n)
#item_identity <- data %>% group_by(Tariff.ID.2, Type) %>% summarise(n=n()) %>% select(-n)
#
#write.csv(country_identity, paste0(data_filename,'_country_ID.csv'))
#write.csv(item_identity, paste0(data_filename,'_item_ID.csv'))

data <- data %>% select(Entry, Line, Mode, Type, Documentation, Country, RecordIntercept, Creation.date)


for (i in seq_len(nrow(weeks_to_use))){
  current_week <- weeks_to_use[i,]
  print(current_week)
  full_data_week <- data %>% filter(Creation.date >= current_week$start_date) %>%
    filter(Creation.date <= current_week$end_date) %>% select(-Creation.date)
  write.csv(full_data_week, paste0('case_study/data_for_stan/furniture_data_2020_week_',i,'_inspected.csv'),
            row.names = FALSE)
}
