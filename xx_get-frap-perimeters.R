library(sf)
library(dplyr)
library(lubridate)

fired <- sf::read_sf("data/raw/FIRED_CONUS_Daily_w_README/fired_conus_daily_nov2001-jan2019.gpkg")

st_layers("data/raw/fire20_1.gdb")

frap <- 
  st_read("data/raw/fire20_1.gdb", layer = "firep20_1", as_tibble = FALSE) %>% 
  mutate(ALARM_DATE = lubridate::with_tz(time = ALARM_DATE, tzone = "UTC"),
         CONT_DATE = lubridate::with_tz(time = CONT_DATE, tzone = "UTC"))

modis_frap <- 
  frap %>% 
  filter(ALARM_DATE > lubridate::ymd(min(fired$date))) %>% 
  mutate(id = 1:nrow(.)) %>% 
  dplyr::select(id, everything())

st_write(obj = modis_frap, dsn = "data/out/frap-perims-during-modis-record.gpkg", delete_dsn = TRUE)
