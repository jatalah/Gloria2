rm(list = ls())
library(timetk)
library(janitor)

conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")

all_d <- read_csv("data/all_data.csv")

# deseasonalise data and obtain anomalies ------

trends_anom <- 
  all_d %>%
  group_by(fish_market, species) %>%
  tk_anomaly_diagnostics(date,
                         tonnes,
                         .alpha = 0.025,
                         .max_anomalies = 0.02) %>%
  ungroup() %>%
  bind_cols(all_d %>% select(log10_tonnes, wave_height_m)) %>% 
  group_by(species, fish_market) %>%
  mutate(anomaly = if_else(
    anomaly == "Yes" &
      trend < quantile(trend, probs = .75),
    "No",
    anomaly
  ), .groups = 'drop')

write_csv(trends_anom, 'data/biomass_waves_trends_anom.csv')

# check class unbalance
d <- 
trends_anom %>% 
  mutate(
    wave_height_lag1 = lag(wave_height_m, 1),
    wave_height_lag2 = lag(wave_height_m, 2),
    wave_height_lag3 = lag(wave_height_m, 3),
    tonnes_lag1 = lag(observed, 1),
    tonnes_lag2 = lag(observed, 2),
    tonnes_lag3 = lag(observed, 3),
    anomaly = factor(anomaly)
  ) %>% 
  select(date, 
         fish_market, 
         species,
         anomaly,
         contains("wave"),
         contains("tonnes"),
         -log10_tonnes,
         -date) %>%
  drop_na() %>% 
  write_csv('data/anomalies_data.csv')

names(d)

# Check VIF 
diag(solve(cor(d[,-c(1:3)]))) %>% 
  enframe()