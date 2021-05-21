# identifying extreme wildfire events along simultaneous axes of biggest, most intense, fastest

library(sf)
library(dplyr)
library(mgcv)

fired_ca <- sf::st_read("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
fired_ca_event <- 
  sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california.gpkg") %>% 
  dplyr::select(id, ignition_date, last_date, max_growth_ha, final_perimeter_m, duration, total_area_ha)

ca <- USAboundaries::us_states(resolution = "high", states = "California") %>% sf::st_transform(sf::st_crs(fired_ca))

fired_frap_mtbs <- sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california_frap-mtbs-joined.gpkg")

modis_afd <- 
  sf::st_read("data/raw/DL_FIRE_M6_184262/fire_archive_M6_184262.shp") %>% 
  sf::st_transform(sf::st_crs(fired_ca)) %>% 
  dplyr::mutate(x = st_coordinates(.)[, 1],
                y = st_coordinates(.)[, 2])

n_workers <- 10

fired_with_afd <- 
  sf::st_join(fired_ca_event, modis_afd) %>% 
  filter(ACQ_DATE >= ignition_date & ACQ_DATE <= last_date) %>% 
  dplyr::mutate(pixel_area = TRACK * SCAN,
                frp_per_area = FRP / pixel_area) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

# Total fire size
biggest_size <-
  fired_ca_event %>% 
  mutate(total_area_ha = as.numeric(sf::st_area(fired_ca_event) / 10000)) %>% 
  dplyr::select(id, total_area_ha) %>% 
  st_drop_geometry()

# Max FRP
biggest_frp <-
  fired_with_afd %>% 
  group_by(id) %>% 
  filter(CONFIDENCE > 90 & pixel_area < 2 & frp_per_area == max(frp_per_area)) %>% 
  dplyr::select(id, frp_per_area, everything()) %>% 
  arrange(desc(frp_per_area)) %>% 
  st_drop_geometry() %>%
  dplyr::ungroup() %>% 
  dplyr::select(id, frp_per_area)

# biggest deviation from expected area of increase (i.e., fastest)
fired_for_gam <-
  fired_ca %>% 
  mutate(daily_area_ha = as.numeric(sf::st_area(fired_ca) / 10000)) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  mutate(cum_area_ha = cumsum(daily_area_ha)) %>% 
  mutate(cum_area_ha_tminus1 = cum_area_ha - daily_area_ha) %>% 
  dplyr::ungroup()

fm1 <- mgcv::gam(daily_area_ha ~ s(cum_area_ha_tminus1, by = lc_name) + s(id, bs = "re"), 
                 method = "REML",
                 data = fired_for_gam)

biggest_area_of_increase <-
  fired_for_gam %>% 
  mutate(preds = predict(fm1),
         area_of_increase_residual = residuals(fm1)) %>% 
  dplyr::select(id, area_of_increase_residual) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(area_of_increase_residual == max(area_of_increase_residual))


# join all together

ranking_ewe <-
  fired_ca_event %>% 
  dplyr::select(-total_area_ha) %>% 
  left_join(biggest_size) %>% 
  left_join(biggest_frp) %>% 
  left_join(biggest_area_of_increase)

pairs(x = st_drop_geometry(ranking_ewe[, c("total_area_ha", "frp_per_area", "area_of_increase_residual")]))
