# identifying extreme wildfire events along simultaneous axes of biggest, most intense, fastest

library(sf)
library(dplyr)
library(mgcv)
library(ggplot2)
library(purrr)
library(data.table)
library(patchwork)

daily_orig <- 
  sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
  dplyr::mutate(lc_name = as.factor(lc_name),
                date = lubridate::ymd(date)) %>% 
  dplyr::select(did, id, date, ignition_date, ignition_day, ignition_month, ignition_year, last_date, event_day, event_duration, daily_area_ha, cum_area_ha, cum_area_ha_tminus1)

events <- 
  sf::st_read("data/out/fired_events_ca.gpkg") %>% 
  dplyr::select(id, ignition_date, ignition_day, ignition_month, ignition_year, last_date, event_duration, total_area_ha)

# We use Resolve Ecoregions within California to divide up the state into different kinds of fire-prone regions
# resolve <- 
#   sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
#   sf::st_transform(3310) %>% 
#   dplyr::select(BIOME_NAME, ECO_NAME) %>% 
#   dplyr::rename_all(.funs = tolower)

events_resolve <- 
  read.csv("data/out/fired_events_resolve.csv") %>% 
  dplyr::mutate(eco_name = as.factor(eco_name),
                biome_name = as.factor(biome_name)) %>% 
  dplyr::as_tibble()

daily_resolve <-
  read.csv("data/out/fired_daily_resolve.csv") %>% 
  dplyr::mutate(eco_name = as.factor(eco_name),
                biome_name = as.factor(biome_name)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(date = as.Date(date))

daily <-
  daily_orig %>% 
  dplyr::left_join(daily_resolve, by = c("did", "id", "date")) %>% 
  dplyr::filter(!is.na(biome_name))

daily 

fired_frap_mtbs_join <- read.csv(file = "data/out/fired-frap-mtbs-join.csv")

modis_afd <- 
  data.table::fread("data/out/mcd14ml_ca.csv", colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
  dplyr::mutate(acq_date = lubridate::ymd(acq_date))

n_workers <- 10

# maximum scan is 4.8 km, meaning the centroid could represent
# a fire 2.4 km on either side ~east/west
# maximum track is 2 km, meaning the centroid could represent
# a fire 1 km on either side ~north/south
# So the hypotenuse of the 1 km x 2.4 km rectangle represents
# the longest possible distance a centroid could be from a fire
# and still represent an active fire within the defined burned area
# is 2.6 km. Of course, there could be additional uncertainties in
# the exact locations of the fire perimeters, too.
fired_with_afd <- 
  sf::st_join(sf::st_buffer(x = events, dist = 2600), modis_afd) %>% 
  filter(acq_date >= ignition_date & acq_date <= last_date) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

# Total fire size
biggest_size_event_scale <-
  events %>% 
  dplyr::select(id, total_area_ha) %>% 
  st_drop_geometry() %>% 
  as_tibble()

# 90th percentile FRP of the fire to try to account for
# saturation of individual FRP measurements
biggest_frp_daily_scale <-
  fired_with_afd %>% 
  st_drop_geometry() %>%
  # filter(confidence > 90 & pixel_area < 2) %>% 
  filter(confidence > 50 & pixel_area < 2) %>%
  group_by(id, acq_date) %>% 
  dplyr::summarize(frp_90 = quantile(x = frp_per_area, probs = 0.9, na.rm = TRUE, names = FALSE)) %>%
  dplyr::ungroup()

biggest_frp_event_scale <-
  biggest_frp_daily_scale %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(frp_90 == max(frp_90)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id, frp_90, acq_date) %>% 
  dplyr::rename(acq_date_frp = acq_date)

biggest_frp_event_scale

# Here, we model the expected area of increase as a function of the fire's size the previous day
# We use a random effect for the individual fires, and fit a smooth for each landcover type
# We noticed some weirdness with the lc_name variable (plurality of fires come up as croplands, which
# doesn't seem right) so we'll try with Resolve Ecoregions instead

# use the log scale to better approximate predicted growth (can't be negative this way!)
# Add 0.1 to all cumulative area burned so that the value for each fire's first day
# isn't NaN when taking log(0) 
# fm1 <- mgcv::gam(log(daily_area_ha) ~ s(log(cum_area_ha_tminus1 + 0.1), by = eco_name) + s(id, bs = "re"), 
#                  method = "REML",
#                  data = daily)
# 
# ggplot(daily_for_model, aes(x = log(cum_area_ha_tminus1 + 0.1), y = log(daily_area_ha), color = eco_name)) +
#   geom_point() +
#   geom_smooth()
# 
# # By biome seems to make a little more sense, because there are only 4
# fm2 <- mgcv::gam(log(daily_area_ha) ~ s(log(cum_area_ha_tminus1 + 0.1), by = biome_name) + s(id, bs = "re"), 
#                  method = "REML",
#                  data = daily)

# By biome seems to make a little more sense, because there are only 4. Don't include a random effect of
# fire ID, because we want the residual to be based on an *across-fire* model not a *within-fire* model
fm3 <- mgcv::gam(log(daily_area_ha) ~ s(log(cum_area_ha_tminus1 + 0.1), by = biome_name), 
                 method = "REML",
                 data = daily)


ggplot(daily, aes(x = log(cum_area_ha_tminus1 + 0.1), y = log(daily_area_ha), color = biome_name)) +
  geom_point() +
  geom_smooth()

# biggest_area_of_increase_residual_daily_scale <-
#   daily %>% 
#   mutate(predicted_aoi_log = as.numeric(predict(fm1)),
#          predicted_aoi = exp(predicted_aoi_log),
#          aoir_modeled = residuals(fm2),
#          aoir = daily_area_ha - predicted_aoi) %>% 
#   dplyr::select(id, date, event_day, daily_area_ha, aoir, aoir_modeled, predicted_aoi, cum_area_ha_tminus1,
#                 predicted_aoi_log) %>%
#   st_drop_geometry()
# 
# biggest_area_of_increase_residual_daily_scale <-
#   daily %>% 
#   mutate(predicted_aoi_log = as.numeric(predict(fm2)),
#          predicted_aoi = exp(predicted_aoi_log),
#          aoir_modeled = residuals(fm2),
#          aoir = daily_area_ha - predicted_aoi) %>% 
#   dplyr::select(id, date, event_day, daily_area_ha, aoir, aoir_modeled, predicted_aoi, cum_area_ha_tminus1,
#                 predicted_aoi_log) %>%
#   st_drop_geometry()

biggest_area_of_increase_residual_daily_scale <-
  daily %>% 
  mutate(predicted_aoi_log = as.numeric(predict(fm3)),
         predicted_aoi = exp(predicted_aoi_log),
         aoir_modeled = residuals(fm3),
         aoir = daily_area_ha - predicted_aoi) %>% 
  dplyr::select(id, date, event_day, daily_area_ha, aoir, aoir_modeled, predicted_aoi, cum_area_ha_tminus1,
                predicted_aoi_log) %>%
  st_drop_geometry()

biggest_area_of_increase_residual_event_scale <-
  biggest_area_of_increase_residual_daily_scale %>% 
  group_by(id) %>% 
  filter(aoir_modeled == max(aoir_modeled)) %>% 
  dplyr::rename(modeled_max_aoir = aoir_modeled,
                aoir_max = aoir,
                actual_aoi_during_max_aoir = daily_area_ha,
                acq_date_aoir = date,
                event_day_aoir = event_day) %>% 
  dplyr::ungroup()

# Some of these variables come with the `daily` dataset, so no need to have them
# be redundant here
biggest_area_of_increase_residual_daily_scale <-
  biggest_area_of_increase_residual_daily_scale %>% 
  dplyr::select(-daily_area_ha, -event_day, -cum_area_ha_tminus1)

# biggest area of increase
# biggest area of increase doesn't need a daily-scale dataset because the
# raw value of daily_area_ha is already the response variable that
# we'd be looking for at a daily time step
biggest_area_of_increase_event_scale <-
  daily %>% 
  dplyr::select(id, date, event_day, daily_area_ha) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(daily_area_ha == max(daily_area_ha)) %>% 
  # One fire had it's maximum area of increase on two separate days (the minimum possible area of increase-- one pixel) so just take the first day that happens
  # using arrange() then slice(1)
  dplyr::arrange(event_day) %>% 
  dplyr::slice(1) %>% 
  dplyr::rename(aoi_max = daily_area_ha,
                acq_date_aoi = date,
                event_day_aoi = event_day) %>%
  dplyr::ungroup()

# Event-scale ranking by joining all the event-scale data together
events
biggest_size_event_scale
biggest_frp_event_scale
biggest_area_of_increase_residual_event_scale
biggest_area_of_increase_event_scale

# code if using daily 90th percentile FRP
ranking_ewe_events <-
  events %>% 
  dplyr::select(-total_area_ha) %>% 
  dplyr::left_join(events_resolve) %>%
  dplyr::filter(!is.na(biome_name)) %>% 
  dplyr::left_join(biggest_size_event_scale) %>% 
  dplyr::left_join(biggest_frp_event_scale) %>% 
  dplyr::left_join(biggest_area_of_increase_residual_event_scale) %>%
  dplyr::left_join(biggest_area_of_increase_event_scale) %>% 
  dplyr::mutate(frp_90 = ifelse(is.na(frp_90), yes = 0, no = frp_90)) %>%
  dplyr::mutate(size_rank = rank(-total_area_ha)) %>% 
  dplyr::mutate(frp_rank = rank(-frp_90)) %>% 
  dplyr::mutate(aoir_rank = rank(-aoir_max)) %>% 
  dplyr::mutate(aoi_rank = rank(-aoi_max)) %>% 
  dplyr::left_join(fired_frap_mtbs_join, by = c(id = "id_fired")) %>% 
  sf::st_drop_geometry()

ranked_ewe_events <- 
  ranking_ewe_events %>% 
  dplyr::select(id, total_area_ha, frp_90, aoir_max) %>%
  mutate(mecdf = pmap_dbl(., .f = function(id, total_area_ha, frp_90, aoir_max) {
    mean(.[, "total_area_ha"] <= total_area_ha & .[, "frp_90"] <= frp_90 & .[, "aoir_max"] <= aoir_max)
  })) %>% 
  mutate(ewe_rank = rank(-mecdf)) %>% 
  left_join(ranking_ewe_events) %>%
  dplyr::arrange(ewe_rank) %>% 
  as_tibble() %>% 
  dplyr::select(id, name_frap, ignition_date, ignition_year, ewe_rank, size_rank, frp_rank, aoir_rank, aoi_rank, mecdf, everything())

ranked_ewe_events_out <- 
  ranked_ewe_events %>% 
  dplyr::select(id, name_frap, overlapping_pct_frap, ignition_date, ignition_year, ignition_month, last_date, mecdf, ewe_rank, size_rank, frp_rank, aoir_rank, aoi_rank, total_area_ha, frp_90, aoir_max, aoi_max, predicted_aoi, actual_aoi_during_max_aoir, cum_area_ha_tminus1, acq_date_frp, acq_date_aoir, acq_date_aoi, event_day_aoir, event_day_aoi, event_duration, modeled_max_aoir, eco_name, biome_name)

## Daily-scale data
# Event-scale ranking by joining all the event-scale data together
daily
biggest_frp_daily_scale
biggest_area_of_increase_residual_daily_scale

ranking_ewe_daily <-
  daily %>% 
  dplyr::left_join(biggest_frp_daily_scale, by = c(id = "id", date = "acq_date")) %>% 
  dplyr::left_join(biggest_area_of_increase_residual_daily_scale, by = c("id", "date")) %>% 
  dplyr::left_join(fired_frap_mtbs_join, by = c(id = "id_fired")) %>% 
  sf::st_drop_geometry()

ranked_ewe_daily <- 
  ranking_ewe_daily %>% 
  as_tibble() %>% 
  dplyr::select(did, id, date, name_frap, ignition_date, ignition_year, everything())

ranked_ewe_events_simple <-
  ranked_ewe_events_out %>% 
  dplyr::select(id, mecdf, total_area_ha, ewe_rank, size_rank, frp_rank, aoir_rank, aoi_rank)

ranked_ewe_daily_out <- 
  ranked_ewe_daily %>% 
  dplyr::select(did, id, date, name_frap, overlapping_pct_frap, ignition_date, ignition_year, ignition_month, biome_name, eco_name, last_date, event_day, event_duration, daily_area_ha, cum_area_ha, cum_area_ha_tminus1, frp_90, aoir, predicted_aoi, aoir_modeled, predicted_aoi_log) %>% 
  dplyr::left_join(ranked_ewe_events_simple)

# Version 2 reformulates the area of increase model to remove the fire-level random effect
# It also removes some of the fire events outside of California and which burn outside of 2003 to 2020
write.csv(x = ranked_ewe_events_out, file = "data/out/extreme-wildfire-events-ranking_v2.csv", row.names = FALSE)
write.csv(x = ranked_ewe_daily_out, file = "data/out/extreme-wildfire-daily-ranking_v2.csv", row.names = FALSE)
