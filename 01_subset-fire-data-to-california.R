library(sf)
library(USAboundaries)
library(data.table)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out", recursive = TRUE, showWarnings = FALSE)

### FIRED ###
# To get FIRED perimeters, we ran the firedpy algorithm using this specific commit:
# https://github.com/earthlab/firedpy/commit/9a585f828a8e8e7510a15171ff0311457defad13
# firedpy -proj_dir H:\dev\megafired_california\data\raw --shapefile -landcover_type 1 -daily yes

fired_daily <- sf::st_read("data/raw/fired_daily.gpkg") %>% sf::st_transform(3310)
fired_events <- sf::st_read("data/raw/fired_events.gpkg") %>% sf::st_transform(3310)

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_geometry()

events_ca <- 
  sf::st_intersection(x = fired_events, y = ca) %>% 
  dplyr::mutate(total_area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  dplyr::rename(geometry = geom)

daily_ca <- 
  fired_daily %>% 
  dplyr::filter(id %in% events_ca$id) %>% 
  dplyr::rename(geometry = geom) %>% 
  mutate(daily_area_ha = as.numeric(sf::st_area(.) / 10000)) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  mutate(cum_area_ha = cumsum(daily_area_ha)) %>% 
  mutate(cum_area_ha_tminus1 = cum_area_ha - daily_area_ha) %>% 
  dplyr::ungroup()

# nrow(daily_ca) ## 20520 day/event unique combinations
# length(unique(daily_ca$id)) ## 4337 unique fire events
sf::st_write(daily_ca, "data/out/fired_daily_ca.gpkg", delete_dsn = TRUE)
sf::st_write(events_ca, dsn = "data/out/fired_events_ca.gpkg", delete_dsn = TRUE)

###

### MCD14ML MODIS active fire product ###

afd <- 
  data.table::fread(input = "data/raw/DL_FIRE_M-C61_200690/fire_archive_M-C61_200690.csv",
                    colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
  sf::st_transform(3310)

afd_ca <- 
  sf::st_intersection(x = afd, y = ca) %>% 
  dplyr::mutate(pixel_area = track * scan,
                frp_per_area = frp / pixel_area) %>% 
  mutate(x = sf::st_coordinates(.)[, 1],
         y = sf::st_coordinates(.)[, 2]) %>% 
  sf::st_drop_geometry()

data.table::fwrite(x = afd_ca, file = "data/out/mcd14ml_ca.csv")