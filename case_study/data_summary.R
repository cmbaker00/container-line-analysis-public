library(tidyverse)
data1 <- read.csv('case_study/data_for_stan/furniture_data_2020_week_1_inspected.csv')

data1 %>% group_by(Entry) %>% summarise(EntrySize = n()) %>% group_by(EntrySize) %>% summarise(n())


ggplot(data1 %>% group_by(Entry) %>% summarise(EntrySize = n()), aes(x=EntrySize)) + geom_histogram()