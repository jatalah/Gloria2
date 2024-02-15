map_df(file.list, ~ read_xlsx(., sheet = 1), .id = "id") %>% janitor::clean_names())
  mutate(
    lonja = str_remove(id, " interpolado.xlsx"),
    specie = "Dorada",
    .after = id
  )

"C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/GLORIA2 - Datos/Dataframes procesados/Aguilas interpolado.xlsx"

read_xlsx(
  "C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/GLORIA2 - Datos/Dataframes procesados/Aguilas interpolado.xlsx",
  sheet = 1)

nm <- list.files()
nm <- list.files(path = "paste(wd()C:/Users/javiera/OneDrive - Cawthron/UA/Gloria2/GLORIA2 - Datos/Dataframes procesados", pattern = '*.xlsx')
map_df(nm, ~ read_xlsx(., sheet = 1), .id = "id")
