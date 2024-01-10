library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(cowplot)
library(mapview)
sf_use_s2(FALSE)

# load("C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/data/study_area_map.RData")

# bbox of study area-----------
bbox <-
  st_bbox(c(
    xmin = -2,
    ymin = 37.2,
    xmax = 1,
    ymax = 41
  ), crs = st_crs(4326))

# Map of Spain---------------
spain <-
  ne_countries(country = "Spain",
               returnclass = "sf",
               scale = "large")

coast <-
  read_sf(
    'C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/CCAA/Comunidades_Autonomas_ETRS89_30N.shp'
  ) %>%
  st_transform(crs = st_crs(4326)) %>%
  st_crop(bbox) %>%
  st_union()

# Bathymetry--------------
bathy <- 
  stars::read_stars('C:/Users/javiera/OneDrive - Cawthron/UA/MODESTA/data/gebco_2022_n42.4292_s34.4751_w-6.6577_e3.0981.tif', crs = st_crs(4326)) %>% 
  st_crop(bbox) %>% 
  rename(depth = 1) %>% 
  as_tibble() %>% 
  mutate(depth =- as.numeric(depth)) %>% 
  filter(depth>=0) 

# Fish markets locations------------
lonja_sf <- 
  read_csv('data/coord_lonjas.csv') %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Farm locations-----
recintos <-
  read_sf('C:/Users/javiera/OneDrive - Cawthron/Stats/mapping/acuivisor_data/Recintos.shp') %>%
  st_crop(st_bbox(c(
    xmin = -2,
    ymin = 37.2,
    xmax = 1,
    ymax = 40.3
  ), crs = st_crs(4326))) %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area>1e4)

# mapview(recintos)

# Buffer recintos----------
recintos_buffer <-
  recintos %>%
  st_buffer(dist = 0.015) %>%
  st_difference(st_buffer(coast, dist = 0.006)) %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(ID = LETTERS[1:nrow(.)])

# mapview(recintos_buffer)

st_write(
  recintos_buffer,
  'data/recintos_buffer.shp',
  # layer_options = "OVERWRITE=true",
  append=FALSE
)


# SIMAR points -----
# data frame with location names and degree coordinates
simar_coords <- tibble(
  Locality = c(
    "San Pedro del Pinatar",
    "Cartagena y Águilas",
    "Alicante",
    "Burriana",
    "Guardamar",
    "Xilxes",
    "Altea",
    "Villajoyosa",
    "El Campello"
  ),
  lat = c(
    37.8234,
    37.6257,
    38.3452,
    39.8896,
    38.1086,
    39.8608,
    38.5970,
    38.5072,
    38.4275
  ),
  lon = c(
    -0.7744,
    -0.9964,
    -0.4810,
    -0.0646,
    -0.6496,
    -0.2015,
    0.0463,
    -0.2339,
    -0.3945
  )
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Map -------------------
map_lonjas <- 
ggplot() +
  # geom_raster(data = bathy, aes(x, y, fill = depth)) +
  geom_contour(
    data = bathy,
    aes(x, y, z = depth),
    color = 'gray70',
    linewidth = .1
  ) +
  # geom_sf(data = simar_coords, color = "yellow", size = 2, alpha = .7) +
  geom_sf(data = spain, fill = 'gray90') +
  geom_sf(data = lonja_sf,
          color = "darkblue",
          alpha = .5) +
  geom_sf(
    data = recintos_buffer,
    fill = 'darkgreen',
    color = 'darkgreen',
    linewidth = .5,
    alpha = .5
  ) +
  ggrepel::geom_label_repel(
    data = lonja_sf,
    aes(label = fish_market, geometry = geometry),
    stat = "sf_coordinates",
    nudge_x = -.3,
    # hjust = 0,
    size = 2.5,
    alpha = .75
  ) +
  geom_sf_text(
    data = recintos_buffer,
    aes(label = ID),
    nudge_x = .15,
    # nudge_x = .1,
    size = 2.5,
    alpha = .75
  ) +
  coord_sf(xlim = c(bbox[1], bbox[3]),
           ylim = c(bbox[2], bbox[4]),
           expand = F) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(1, "cm"),
    pad_y = unit(.75, "cm"),
    style = north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  annotation_scale(location = "br", width_hint = 0.15) +
  scale_y_continuous(breaks = seq(37, 41, by = 1)) +
  scale_x_continuous(breaks = seq(-2, 2, by = 1))+
  theme(panel.border = element_rect(color = 'gray30', fill= NA))

map_lonjas

# Inset map of Spain-------------
iberian_map <-
ggplot() +
  geom_sf(
    data = ne_countries(
      scale = 'large',
      type = 'map_units',
      returnclass = 'sf',
      continent = "Europe"
    ),
    fill = 'gray70',
    color = 'white'
  ) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(panel.border = element_rect(color = 'gray30', fill = NA)) +
  geom_sf(data = st_as_sfc(bbox),
          fill = 'transparent',
          color = 'darkred',
          lty = 1) +
  coord_sf(
    xlim = c(-10, 4.5),
    ylim = c(35.5, 44.3),
    expand = FALSE
  )

iberian_map

# join maps----------
fig_map <- 
  cowplot::ggdraw() +
  draw_plot(map_lonjas) +
  draw_plot(
    iberian_map,
    x = 0.2,
    y = .75,
    width = .25,
    height = .25
  )

fig_map


# save map--------------------
ggsave(
  last_plot(),
  filename = 'figures/map_figure.png',
  width = 5,
  height = 6,
  units = 'in',
  bg = 'white'
)
