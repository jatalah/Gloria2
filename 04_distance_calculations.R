rm(list = ls())

library(sf)
library(tidyverse)
library(ggpubr)

sf_use_s2(FALSE)

lonja_sf <- 
  read_csv('data/coord_lonjas.csv') %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

recintos_buffer <- st_read('data/recintos_buffer.shp')

# distance between fish_markets and farms -----
dist <- 
  st_distance(lonja_sf, recintos_buffer) %>% 
  as_tibble() %>% 
  mutate(across(everything(), as.numeric)) %>%
  rename_with(., ~LETTERS[1:nrow(recintos_buffer)]) %>% 
  mutate(fish_market = lonja_sf$fish_market) %>%  
  pivot_longer(cols = -fish_market) %>% 
  mutate(distance = value/1e3, .keep = 'unused') %>% 
  write_csv('data/distances.csv')


dist <- read_csv('data/distances.csv')

dist_wide <- 
dist %>% pivot_wider(id_cols = fish_market, values_from = distance)

order <-
  hclust(dist(dist_wide[,-1], method = "euclidean"), method = "ward.D")$order %>% as_vector()

dist_wide$fish_market[order]

dist_ordered <- 
dist %>% 
  mutate(fish_market = fct_relevel(fish_market, dist_wide$fish_market[order]))

ggplot(dist_ordered) +
  geom_raster(aes(name, fish_market, fill = distance), alpha= .9) +
  theme_minimal() +
  scale_fill_viridis_c(option = 'B', direction = -1, trans = 'log10') +
  labs(x = 'Farm', fill = "km", y = 'Fish market') +
  theme(panel.grid.major = element_blank())

summary(dist_ordered)

ggsave(
  last_plot(),
  filename = 'figures/distance_matrix_plot.png',
  width = 6,
  height = 4,
  bg = 'white',
  dpi = 150
)


dist_sum <-
  dist %>%
  group_by(fish_market) %>%
  summarise(mean_dist = mean(distance),
            min_dist = min(distance),
            sum_dist =sum(distance),
            q10_dist = quantile(distance, .1)) %>%
  left_join(dist %>%
              group_by(fish_market) %>%
              arrange(distance) %>% 
              top_n(-1, distance) %>%
              summarise(mean_dist3 = mean(distance))) 

d <- 
  # read_csv('tables/coef_table_glm.csv') %>% 
  read_csv('tables/coef_table_lms.csv') %>% 
  left_join(dist_sum) %>% 
  filter(term =="Wave lag 1")

d %>%
  group_by(species) %>%
  nest() %>% 
  mutate(cor = map(data, ~cor.test(.x$estimate, .x$mean_dist3)),
         tidied = map(cor, broom::tidy)) %>% 
  unnest(tidied)

ggplot(d, aes(mean_dist3, estimate, color = species)) +
  geom_point(alpha = .7) +
  # geom_text(aes(label = fish_market), size = 3) +
  # stat_smooth(se = T, method = "gam", formula = y ~ s(x, k = 5)) +
  stat_smooth(
    se = T,
    method = "lm",
    formula = y ~ x,
    alpha = .1
  ) +
  # facet_wrap(~term, scales = 'free') +
  theme_minimal(base_size = 8) +
  labs(x = "Distance to the closest fish market (km)", y = "Effect of wave height lagged by 1 day") +
  scale_color_discrete(name = NULL)

ggsave(
  last_plot(),
  filename = 'figures/wave_effect_vs_distance.png',
  width = 89,
  height = 45,
  dpi = 300,
  units = 'mm',
  # device = 'svg',
  bg = 'white'
)


# correlation with glm coefficients --------

dd <- 
  # read_csv('tables/coef_table_glm.csv') %>% 
  read_csv('tables/coef_table_glm.csv') %>% 
  left_join(dist_sum)

dd %>% 
  filter(term =="wave_height_lag1") %>% 
  ggplot(aes(mean_dist3, estimate, color = species)) +
  geom_point(alpha = .7) +
  # geom_text(aes(label = fish_market), size = 3) +
  # stat_smooth(se = T, method = "gam", formula = y ~ s(x, k = 5)) +
  stat_smooth(se = T, method = "lm", formula = y ~ x, alpha = .1) +
  # facet_wrap(~term, scales = 'free') +
  theme_minimal(base_size = 8) +
  labs(x = "Distance to the closest fish market (km)", y = "Effect of wave height lagged by 1 day") +
  scale_color_discrete(name = NULL)

dd %>%
  group_by(species) %>%
  nest() %>% 
  mutate(cor = map(data, ~cor.test(.x$estimate, .x$mean_dist3)),
         tidied = map(cor, broom::tidy)) %>% 
  unnest(tidied)