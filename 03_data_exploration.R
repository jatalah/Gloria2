library(tsibble)
all_d <- read_csv("data/all_data.csv") 

# summary table----
all_d %>% 
  group_by(fish_market, species) %>% 
  summarise(start = min(date),
            end = max(date),
            months = end - start,
            n = n(), .groups = 'drop') %>% 
  arrange(fish_market)

all_d %>% 
  mutate(tonnes = 10^log10_tonnes) %>% 
  group_by(species) %>% 
  summarise(across(
    tonnes,
    list(
      min = min,
      mean = mean,
      median = median,
      max = max,
      sd = sd,
      Q1 = \(x) quantile(x, probs = 0.25),
      Q3 = \(x) quantile(x, probs = 0.75)
    )
  ))
  

# anomalies exploration--------------
trends_anom <- 
  read_csv('data/biomass_waves_trends_anom.csv') %>% 
  mutate(month = month(date),
         season = case_when(month %in% c(12,1:2) ~ "Winter",
                            month %in% 3:5 ~ "Spring",
                            month %in% 6:8 ~ "Summer",
                            month %in% 9:11 ~ "Autum")) 

janitor::tabyl(trends_anom, anomaly)

# anonmalies by season-------
trends_anom %>% 
  filter(anomaly=="Yes") %>% 
  tabyl(season)

# anomalies by market-------
trends_anom %>% 
  group_by(fish_market, species, anomaly) %>% 
  count() %>% 
  print(n = Inf) %>% 
  arrange(n)

# anomaly seasonality -----
trends_anom %>% 
  filter(anomaly=="Yes") %>% 
  ggplot(aes(month)) +
  geom_histogram(bins = 12) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  theme_minimal()

# tonnes trends and anomalies--------
ggplot(trends_anom, aes(x = date, color = species)) +
  labs(x = NULL, y = "trend") +
  geom_line(aes(y = trend), alpha = .8, linewidth = .25) +
  facet_wrap( ~ fish_market, scales = "free", ncol = 3) +
  theme_minimal(base_size = 8) +
  scale_color_discrete(name = NULL) +
  scale_y_log10() +
  theme(legend.position = 'bottom') +
  geom_point(
    data = trends_anom %>% filter(anomaly == "Yes"),
    aes(date, trend, color = species),
    color = "black",
    size = .5,
    alpha = .3
  )


ggsave(last_plot(),
       filename = 'figures/tonnes_trends_ts_plots.png',
       width = 7, 
       height = 5,
       dpi = 300,
       bg = 'white',
       device = 'png')


# seasonal trends----
all_d %>%
  as_tsibble(index = date, key = c(species,fish_market), regular = F) %>% 
  group_by_key() %>%
  index_by(month = ~month(.)) %>% # monthly aggregates
  summarise(mean = mean(catch_tonnes, na.rm = TRUE),
            sd =- sd(catch_tonnes, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, color = species)) +
  labs(x = "Month", y = "Catch (tonnes)") +
  geom_path(aes(y = exp(mean)), alpha = .6) +
  facet_wrap( ~ fish_market, scales = "free", ncol = 5) +
  theme_minimal(base_size = 8) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  theme(legend.position = 'bottom') +
  scale_color_discrete(name = NULL)

trends_anom %>%
  as_tsibble(index = date, key = c(species,fish_market), regular = F) %>% 
  group_by_key() %>% 
  index_by(month = ~month(.)) %>% # monthly aggregates
  summarise(mean = mean(trend, na.rm = TRUE),
            sd =- sd(trend, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, color = species)) +
  labs(x = "Month", y = "Trend") +
  geom_path(aes(y = 10^(mean)), alpha = .6) +
  facet_wrap(species~ fish_market, scales = "free", ncol = 5) +
  theme_minimal(base_size = 8) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  theme(legend.position = 'bottom') +
  scale_color_discrete(name = NULL)

ggsave(last_plot(),
       filename = 'figures/seasonal_tonnes_trend.png',
       width = 7,
       height = 5,
       dpi = 300,
       bg = 'white',
       device = 'png')
