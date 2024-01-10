library(tidyverse)

# read data---
all_d <- 
  read_csv("data/all_data.csv") %>% 
  select(-id)

tab_s1 <- 
  all_d %>% 
  group_by(fish_market, species) %>% 
  # mutate(fecha = ymd(fecha)) %>% 
  summarise(start = ymd(min(date)),
            end = ymd(max(date)),
            years = interval(start, end)/years(1) %>% round(., 1),
            n = n()) %>% 
  arrange(fish_market) %>% 
  ungroup() %>% 
  write_excel_csv('tables/table_S1.csv')

tab_s1

summary(tab_s1)