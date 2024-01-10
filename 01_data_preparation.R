library(readxl)
library(tidyverse)
library(conflicted)
library(stringr)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")

# need to set wd to folder containing all files
setwd("C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/data/GLORIA2 - Datos/Dataframes procesados")
file.list <- list.files(pattern = '*.xlsx')
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names


df_raw <-
  bind_rows(
    map_df(file.list, ~ read_xlsx(., sheet = 1), .id = "id") %>%
      janitor::clean_names() %>%
      mutate(
        lonja = str_remove(id, " interpolado.xlsx"),
        specie = "Dorada",
        .after = id
      ),
    map_df(file.list, ~ read_xlsx(., sheet = 2), .id = "id") %>%
      janitor::clean_names() %>%
      mutate(
        lonja = str_remove(id, " interpolado.xlsx"),
        specie = "Lubina",
        .after = id
      )
  )

write_csv(df_raw, 'C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/data/df_raw.csv')

df <- 
df_raw %>% 
  filter(cantidad_kg > 0) %>% 
  mutate(specie = fct_recode(specie, Seabream = "Dorada", Seabass = "Lubina"),
         fecha = as_date(fecha),
         # log_tonnes = log(cantidad_kg/1e3),
         log10_tonnes = log10(cantidad_kg/1e3),
         tonnes = cantidad_kg/1e3) %>% # log transform the response variable 
 rename(
    fish_market = lonja,
    species = specie,
    date = fecha,
    wave_height_m = oleaje_m,
    wave_period_s = periodo_s,
    peak_wave_period_s = periodo_de_pico_s,
    swell_height_m = mar_de_fondo_m,
    wind_speed_m_s = velocidad_del_viento_m_s
  ) %>% 
  group_by(fish_market, species) %>%
  mutate(n = n()) %>% 
  filter(n > 2e3) %>% # select timeseries with 2,000 observations
  arrange(fish_market, species, date) %>%
  ungroup() %>%
  select(
    -cantidad_kg, # remove original response variable
    -wave_period_s,
    -wave_period_s,
    -peak_wave_period_s,
    -swell_height_m,
    -wind_speed_m_s,
    -n,
    -tonnes
  ) 

# check data
head(df)
dim(df)

# save model data -----------
setwd("C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2")
write_csv(df, "C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/data/all_data.csv")


## WAVES data ---------------------

# read and prepare data ----------
waves <-
  read_csv('data/Gloria_wave.csv', na = "NA") %>%
  mutate(Date = as.POSIXct(Date, format = "%Y %m %d %H")) %>%
  drop_na() %>%
  group_by(Locality, date = floor_date(Date, unit = "day")) %>%
  summarize(
    Wave_height_m = mean(Wave_height_m),
    Average_wave_period_s = mean(Average_wave_period_s),
    Peak_wave_period_s = max(Peak_wave_period_s),
    Average_direction_N = mean(Average_direction_N)
  )

write_csv(waves, 'data/waves_daily.csv')