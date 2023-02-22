library(sf)
library(dplyr)
library(stringr)
library(USAboundaries)

dir.create(path = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020_subgeo", recursive = TRUE, showWarnings = FALSE)
dir.create(path = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020", recursive = TRUE, showWarnings = FALSE)
dir.create(path = "data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020", recursive = TRUE, showWarnings = FALSE)
dir.create(path = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020_biggest-poly", recursive = TRUE, showWarnings = FALSE)

fired_events <- 
  sf::st_read("data/out/fired/01_spatial-subset/fired_events_ca.gpkg") %>% 
  dplyr::filter(ignition_year >= 2003 & ignition_year <= 2020) %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  dplyr::select(id) %>% 
  dplyr::rename(geometry = geom)

fired_daily <-
  sf::st_read("data/out/fired/01_spatial-subset/fired_daily_ca_v2.gpkg") %>% 
  dplyr::filter(ignition_year >= 2003 & ignition_year <= 2020) %>% 
  sf::st_transform(3310) %>% 
  dplyr::mutate(samp_id = 0) %>% 
  sf::st_set_agr(value = "constant") %>% 
  dplyr::select(did, id, date, samp_id) %>% 
  dplyr::rename(geometry = geom)

fired_daily_multipart <-
  sf::st_cast(x = fired_daily, to = "POLYGON") %>% 
  dplyr::mutate(subgeo_id = str_split_fixed(string = rownames(.), pattern = "\\.", n = 2)[, 2],
                subgeo_id = ifelse(subgeo_id == "", yes = 0, no = as.numeric(subgeo_id))) %>% 
  dplyr::select(did, id, date, subgeo_id, samp_id) %>% 
  sf::st_set_agr(value = "constant") 

rownames(fired_daily_multipart) <- 1:nrow(fired_daily_multipart)

fired_daily_multipart_biggest <-
  fired_daily_multipart %>% 
  dplyr::mutate(area = sf::st_area(.)) %>% 
  dplyr::group_by(did, id, date) %>% 
  dplyr::arrange(desc(area)) %>% 
  dplyr::slice(1) %>% 
  dplyr::select(-area)

fired_daily_centroids <-
  fired_daily %>% 
  sf::st_centroid()

## determine the fires that we want to work with based on filtering criteria
# We only want fire/day combinations whose centroids are within California 
ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_geometry()

target_fires <- 
  fired_daily_centroids %>% 
  dplyr::filter(sf::st_intersects(x = ., y = ca, sparse = FALSE))

target_fires_id <- unique(target_fires$id)
target_fires_did <- unique(target_fires$did)

fired_events_out <-
  fired_events %>%
  dplyr::filter(id %in% target_fires_id)

fired_daily_out <-
  fired_daily %>%
  dplyr::filter(did %in% target_fires_did)

fired_daily_multipart_out <-
  fired_daily_multipart %>%
  dplyr::filter(did %in% target_fires_did)

fired_daily_multipart_biggest_out <-
  fired_daily_multipart_biggest %>% 
  dplyr::filter(did %in% target_fires_did)

# write out a set of .shp because that's what Earth Engine requires
sf::st_write(obj = fired_events_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020/fired_events_ca_epsg3310_2003-2020.shp", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020/fired_daily_ca_epsg3310_2003-2020.shp", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_multipart_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020_subgeo/fired_daily_ca_epsg3310_2003-2020_subgeo.shp", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_multipart_biggest_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020_biggest-poly/fired_daily_ca_epsg3310_2003-2020_biggest-poly.shp", delete_dsn = TRUE)

# Write out a set of .gpkg also, because those are often simpler to deal with
sf::st_write(obj = fired_events_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_multipart_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020_subgeo.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_multipart_biggest_out, dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020_biggest-poly.gpkg", delete_dsn = TRUE)
