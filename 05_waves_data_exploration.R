library(timetk)
library(janitor)


# read data------------
waves <- read_csv('data/waves_daily.csv')


waves %>% select(Wave_height_m:Peak_wave_period_s) %>% cor()

waves %>%
  pivot_wider(names_from = Locality,
              values_from = Wave_height_m,
              id_cols = date) %>%
  select(-date) %>%
  cor()

waves %>% count(Locality)


waves %>% 
  filter(Wave_height_m>3) %>% 
  group_by(Locality) %>% 
  count()

# summaries for plots------
waves_sum <-
  waves %>%
  drop_na() %>%
  group_by(Locality) %>%
  mutate(direction_cut = cut(
    Average_direction_N,
    breaks = seq(0, 360, by = 45),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  )) %>%
  group_by(Locality, direction_cut) %>%
  summarise(frequency = n())

# create waves roses----
wave_roses <-
  ggplot(waves_sum,
         aes(x = direction_cut, y = frequency, fill = direction_cut)) +
  geom_col(width = 1) +
  coord_polar(start = -pi / 8) +
  facet_wrap( ~ Locality, ncol = 1, scales = "fixed") +
  scale_fill_viridis_d(guide = NULL) +
  labs(y = NULL, x = NULL) +
  theme_minimal(base_size = 8)

wave_roses

# wave time series decomposition ---------
ts_decomp_wave <-
  waves %>%
  group_by(Locality) %>%
  tk_anomaly_diagnostics(.date_var = date, .value = Wave_height_m)

write_csv(ts_decomp_wave, 'data/waves_decomposed.csv')

# correlations between decomposed time series------
ts_decomp_wave %>%
  pivot_wider(names_from = Locality,
              values_from = trend ,
              id_cols = date) %>%
  select(-date) %>%
  cor()

# long-term trend plots-------------
wave_trend_plot <-
  ggplot(ts_decomp_wave, aes(x = date, y = trend)) +
  labs(x = NULL, y = "trend") +
  geom_line(alpha = .6, linewidth = .2) +
  facet_wrap(~ Locality, scales = "free", ncol = 1) +
  theme_minimal(base_size = 8) +
  geom_smooth(linewidth = .2)

wave_trend_plot

# glue plots together------------
ggpubr::ggarrange(wave_trend_plot, wave_roses)

# save plot--------
ggsave(
  last_plot(),
  filename = 'figures/wave_plots.svg',
  width = 4,
  height = 10,
  dpi = 300,
  device = 'svg',
  bg = 'white'
)

# rolling averages  plot--------
library(zoo)
ts_decomp_wave %>%
  group_by(Locality) %>%
  mutate(rolling_avg = rollmean(observed, 7, fill = NA, align = "right")) %>%
  ggplot(aes(x = date, y = rolling_avg)) +
  labs(x = NULL, y = "Rolling Average of Trend") +
  geom_line(alpha = .6, linewidth = .2) +
  geom_smooth(linewidth = .2) +
  facet_wrap(~ Locality, scales = "free", ncol = 2) +
  theme_minimal(base_size = 8)

# waves raw------
ggplot(ts_decomp_wave, aes(x = as_date(date), y = observed)) +
  labs(x = NULL, y = "Wave heigtht (m)") +
  geom_line(alpha = .6, linewidth = .2) +
  facet_wrap(~ Locality, ncol = 1) +
  theme_minimal(base_size = 8) +
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y"))
