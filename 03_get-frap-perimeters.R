library(sf)
library(dplyr)
library(lubridate)

fired <- sf::read_sf("data/raw/FIRED_CONUS_Daily_w_README/fired_conus_daily_nov2001-jan2019.gpkg")

download.file(url = "https://frap.fire.ca.gov/media/10969/fire19_1.zip",
              destfile = "data/raw/fire19_1.zip")

unzip(zipfile = "data/raw/fire19_1.zip", exdir = "data/raw")

unlink("data/raw/fire19_1.zip")

st_layers("data/raw/fire19_1.gdb")

frap <- st_read("data/raw/fire19_1.gdb", layer = "firep19_1")

fired <- 

modis_frap <- 
  frap %>% 
  filter(ALARM_DATE > lubridate::ymd(min(fired$date)))

st_write(obj = modis_frap, dsn = "data/out/frap-during-modis-record.gpkg")
