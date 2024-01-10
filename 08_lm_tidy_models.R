rm(list = ls())
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(conflicted)
library(ggpubr)
library(tsibble)
library(zoo)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")
source('06_lms_functions.R')

# read data---
all_d <- 
  read_csv("data/all_data.csv") %>% 
  select(-id) 

dat <-
  all_d %>% 
  group_by(fish_market, species) %>%
  # mutate(roll_wave_3 = zoo::rollmean(wave_height_m, k = 7, fill = NA)) %>% 
  nest()

models <-
  dat %>%
  # filter(species == "Seabass") %>% # test with  one dataset
  mutate(
    splits = map(data, split_func),
    model = map2(.x = data, .y = splits, ~ fit_func(data = .x, splits = .y)),
    refit = pmap(list(model, data, splits), refit_func),
    test = map(splits, testing),
    ts_plot = pmap(list(refit, data, test, fish_market, species),
                   function(refit, data, newdata, fish_market, species) {
                     refit %>%
                       modeltime_forecast(actual_data = data, new_data = newdata)
                   }),
    # ts_plot_all_pred = pmap(list(refit, data, test, fish_market, species),
    #                function(refit, data, newdata, fish_market, species) {
    #                  refit %>%
    #                    modeltime_forecast(actual_data = data, new_data = data)}),
    lm_model = map(model, pull_workflow_fit),
    glance_lm = map(lm_model, glance),
    tidy_lm = map(lm_model, ~tidy(.x, conf.int = T))
  )

saveRDS(models, file = 'data/biomass_lm_models.rds')

## Timeseries plots predictions----
models %>% 
  select(ts_plot) %>% 
  unnest(cols = c(ts_plot)) %>% 
  # filter(10^(.value)<1e6) %>% 
  arrange(species) %>% 
  as_tsibble(key = c(.key, species, fish_market), index = .index) %>%
  fill_gaps(.full = TRUE) %>%
  ggplot(aes(.index, 10^(.value), color = .key)) +
  geom_line(alpha = 1, linewidth = 0.2) +
  theme_minimal(base_size = 9) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(y = "Tonnes", x = NULL) +
  facet_wrap(~paste(fish_market,species, sep = " - "), ncol = 2) +
  scale_color_tableau(name = NULL) +
  theme(legend.position = 'none')


# plot time series with and train set predictions  ---------
ggsave(
  last_plot(),
  filename = "figures/predictions_train_lms.png",
  width = 6,
  height = 10,
  device = "png",
  bg = 'white'
)


# plot regression estimates by fish_market and species------------
coef_table <-
  models %>%
  select(tidy_lm) %>%
  unnest(cols = (tidy_lm)) %>%
  ungroup() %>%
  mutate(across(is.numeric, ~ round(.x, 2))) %>%
  mutate(
    term = fct_recode(
      term,
      Intercept =  "(Intercept)",
      `Wave height` = "wave_height_m",
      `Wave lag 1` = "lag_1_wave_height_m",
      `Wave lag 2` = "lag_2_wave_height_m",
      `Wave lag 3` = "lag_3_wave_height_m",
      `Tonnes lag 1` = "lag_1_log10_tonnes",
      `Tonnes lag 2` = "lag_2_log10_tonnes",
      `Tonnes lag 3` = "lag_3_log10_tonnes"
      
    )
  ) %>%
  select(-statistic) %>% 
  mutate(p.value = scales::pvalue(p.value)) %>% 
  rename(P = p.value) %>% 
  select(-S.E.)


write_excel_csv(coef_table, 'tables/coef_table_lms.csv')

coef_table %>% 
  filter(term !="(Intercept)") %>% 
  mutate(term = str_to_sentence(str_replace_all(term, "_", " "))) %>%
  mutate(term = str_remove_all(term, " m")) %>%
  mutate(term = str_remove_all(term, "log10")) %>%
  ggplot(aes(fct_rev(fct_inorder(term)), color = fish_market)) +
  geom_pointrange(
    aes(
      y = estimate,
      ymin = conf.low,
      ymax =  conf.high
    ),
    alpha = .8,
    size = .3,
    position = position_dodge2(width = .2)
  ) +
  coord_flip() +
  facet_wrap( ~ species, scales = "free") +
  theme_minimal(base_size = 8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = NULL, y = "Estimate") +
  scale_color_viridis_d(name = NULL, option = 'D')

ggsave(
  last_plot(),
  filename = "figures/coef_plot_lms.png",
  width = 7,
  height = 3.5,
  device = "png",
  bg = 'white'
)

# regression summary tables------
model_summaries <- 
models %>%
  select(glance_lm) %>%
  unnest(cols = (glance_lm)) 

model_summaries %>%
  group_by(species) %>%
  summarise(across(
    adj.r.squared,
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      Q1 = ~ quantile(., probs = 0.25, na.rm = TRUE),
      Q3 = ~ quantile(., probs = 0.75, na.rm = TRUE)
    )
  )) %>% 
  pivot_longer(cols = -species)

model_summaries %>%
  group_by(species) %>% 
  get_summary_stats() %>% 
  print(n = Inf)

# save suplmentary table----
model_summaries %>%
  mutate(p.value = scales::pvalue(p.value)) %>%
  rename(P = p.value) %>%
  mutate(across(is.numeric, ~ round(.x, 2))) %>%
  write_excel_csv('tables/summary_table_lms.csv')
