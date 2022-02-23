library(tidyverse)
data1 <- read.csv('case_study/data_for_stan/furniture_data_2020_week_1_inspected.csv')
data1$Week <- 1
data2 <- read.csv('case_study/data_for_stan/furniture_data_2020_week_2_inspected.csv')
data2$Week <- 2
data3 <- read.csv('case_study/data_for_stan/furniture_data_2020_week_3_inspected.csv')
data3$Week <- 3

data1 %>% group_by(Entry) %>% summarise(EntrySize = n()) %>% group_by(EntrySize) %>% summarise(n())


ggplot(data1 %>% group_by(Entry) %>% summarise(EntrySize = n()), aes(x=EntrySize)) + geom_histogram()

data <- rbind(data1, rbind(data2, data3))
data %>% group_by(Week, Mode) %>% summarise(sum(RecordIntercept))