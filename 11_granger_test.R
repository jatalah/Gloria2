rm(list = ls())
library(lmtest)
library(broom)

all_d <- 
  read_csv("data/all_data.csv") %>% 
  select(-id)


granger_test <- 
all_d %>%
  arrange(fish_market, species, date) %>%
  group_by(fish_market, species) %>%
  nest() %>%
  mutate(gr_test = map(
    data,
    ~grangertest(
      log10_tonnes ~ wave_height_m,
      order = 3,
      data = .x,
      test = "F"
    ) %>%
      broom::tidy()
  ))

res_gr_tests <- 
granger_test %>% 
  select(gr_test) %>% 
  unnest(cols = gr_test) %>% 
  ungroup() %>% 
  drop_na(p.value)


res_gr_tests %>% 
  left_join(dist_sum) %>% 
  ggplot(aes(statistic, min_dist   , color = species)) +
  geom_point() +
  # geom_text(aes(label = fish_market), size = 3) +
  stat_smooth(se = F, method = "loess", span = .9) +
  theme_minimal()