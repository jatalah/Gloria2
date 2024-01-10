aqua <-
  read_rds('C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/data/FAO aqua/production_data_all.RDS') %>%
  filter(
    country_name == "Spain",
    Scientific_Name %in% c("Sparus aurata", "Dicentrarchus labrax"),
    ENVIRONMENT.ALPHA_2_CODE == "MA",
    PRODUCTION_AREA == 37
  )

write_csv(aqua, 'data/fao_aqua_seabream_seabass.csv')

fisheries <- 
  read_rds('C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/data/FAO captures/') %>%
  filter(
    country_name == "Spain",
    Scientific_Name %in% c("Sparus aurata", "Dicentrarchus labrax"),
    ENVIRONMENT.ALPHA_2_CODE == "MA",
    PRODUCTION_AREA == 37
  )

# plot aqua production data -----------------
ggplot(aqua, aes(Year, VALUE, color = spp_name)) +
  geom_path() +
  theme_minimal(base_size = 7) +
  # scale_x_continuous(breaks = seq(1980, 2030, 4)) +
  labs(color = NULL, x = NULL, y = "Production (tonnes)") +
  theme(legend.position = c(.2,.8)) +
  scale_y_continuous(labels = scales::comma)


ggsave(
  last_plot(),
  filename = 'figures/aqu_production.png',
  width = 89,
  height = 45,
  dpi = 300,
  units = 'mm',
  # device = 'svg',
  bg = 'white'
)

aqua %>% distinct(spp_name)

read_csv("data/all_data.csv") %>%
  group_by(Year = year(date), species) %>%
  summarise(landing  = sum(10 ^ log10_tonnes)) %>%
  mutate(spp_name = fct_recode(
    species,
    `European seabass` = "Seabass",
    `Gilthead seabream` = "Seabream"
  )) %>%
  left_join(aqua, by = c("Year", "spp_name")) %>%
  pivot_longer(cols = c("landing", "VALUE")) %>% 
  filter(Year<2021) %>% 
  ggplot(aes(Year, value, color = name)) +
  geom_path() +
  facet_wrap(~spp_name) +
  theme_minimal() +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1980, 2030, 4)) +
  labs(color = NULL, x = NULL, y = "Aquaculture production (tonnes)")


read_csv("data/all_data.csv") %>%
  group_by(Year = year(date), species) %>%
  summarise(landing  = sum(10 ^ log10_tonnes)) %>%
  mutate(spp_name = fct_recode(
    species,
    `European seabass` = "Seabass",
    `Gilthead seabream` = "Seabream"
  )) %>%
  left_join(aqua, by = c("Year", "spp_name")) %>%
  # pivot_longer(cols = c("landing", "VALUE")) %>% 
  filter(Year<2021 & !spp_name =="European seabass" |landing<200) %>%
  ggplot(aes(VALUE, landing)) +
  geom_smooth() +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~spp_name, scales = 'free') +
  theme_minimal()

                     