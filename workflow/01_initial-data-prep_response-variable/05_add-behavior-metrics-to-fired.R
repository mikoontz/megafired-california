# identifying extreme wildfire events along simultaneous axes of biggest, most intense, fastest

library(sf)
library(dplyr)
library(mgcv)
library(ggplot2)
library(purrr)
library(data.table)
library(patchwork)

dir.create("data/out/fired/05_daily-with-behavior-metrics", recursive = TRUE, showWarnings = FALSE)

daily_orig <- 
  sf::st_read("data/out/fired/01_spatial-subset/fired_daily_ca_v2.gpkg") %>% 
  dplyr::mutate(lc_name = as.factor(lc_name),
                date = as.Date(date)) %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::select(did, id, date, event_day, daily_area_ha, daily_area_tminus1_ha, cum_area_ha, cum_area_ha_tminus1, daily_perim_km, daily_perim_tminus1_km, active_fireline_km)

# We use Resolve Ecoregions within California to divide up the state into different kinds of fire-prone regions
daily_resolve <- 
  read.csv("data/out/fired/03_joined-with-other-data/fired_daily_resolve.csv") %>% 
  dplyr::mutate(eco_name = as.factor(eco_name),
                biome_name = as.factor(biome_name)) %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(date = as.Date(date))

daily <-
  daily_orig %>% 
  dplyr::left_join(daily_resolve, by = c("did", "id", "date")) %>% 
  dplyr::filter(!is.na(biome_name))

daily 

modis_afd <- 
  data.table::fread("data/out/active-fire/mcd14ml_ca.csv", colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
  dplyr::mutate(acq_date = lubridate::ymd(acq_date)) %>% 
  dplyr::mutate(local_datetime = lubridate::ymd_hm(paste0(acq_date, " ", acq_time))  - lubridate::hours(x = 7),
                local_hour = lubridate::hour(local_datetime),
                local_minute = lubridate::minute(local_datetime),
                local_date = lubridate::ymd(paste(lubridate::year(local_datetime),
                                                  lubridate::month(local_datetime),
                                                  lubridate::day(local_datetime),
                                                  sep = "-")))

daily_with_afd <- 
  sf::st_join(sf::st_buffer(x = daily, dist = 2600), modis_afd) %>% 
  filter(local_date == date) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

daily_with_afd_summary <-
  daily_with_afd %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(did, id, date, daily_area_ha) %>% 
  dplyr::summarize(frp90 = quantile(x = frp, probs = 0.9),
                   n_afd = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(afd_per_ha = n_afd / daily_area_ha) %>% 
  dplyr::select(-daily_area_ha)

# fired_frap_mtbs_join <- read.csv(file = "data/out/fired-frap-mtbs-join.csv")

ggplot(daily, aes(x = log(cum_area_ha_tminus1 + 0.1), y = log(daily_area_ha), color = biome_name)) +
  geom_point() +
  geom_smooth()

ggplot(daily, aes(x = active_fireline_km, y = log(daily_area_ha), color = biome_name)) +
  geom_point() +
  geom_smooth()

ggplot(daily, aes(x = daily_perim_tminus1_km, y = log(daily_area_ha), color = biome_name)) +
  geom_point() +
  geom_smooth()

ggplot(daily, aes(x = log(daily_area_tminus1_ha + 0.1), y = log(daily_area_ha), color = biome_name)) +
  geom_point() +
  geom_smooth()

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

# # By biome seems to make a little more sense, because there are only 4. Don't include a random effect of
# # fire ID, because we want the residual to be based on an *across-fire* model not a *within-fire* model
fm_log_cum_area_tm1 <- mgcv::gam(log(daily_area_ha) ~ s(log(cum_area_ha_tminus1 + 0.1), by = biome_name),
                                 method = "REML",
                                 data = daily)
# 
# ggplot(daily, aes(x = log(cum_area_ha_tminus1 + 0.1), y = log(daily_area_ha), color = biome_name)) +
#   geom_point() +
#   geom_smooth()

# By biome seems to make a little more sense, because there are only 4. Don't include a random effect of
# fire ID, because we want the residual to be based on an *across-fire* model not a *within-fire* model
fm_sqrt_area_tm1 <- mgcv::gam(log(daily_area_ha) ~ s(sqrt(daily_area_tminus1_ha), by = biome_name), 
                              method = "REML",
                              data = daily)

summary(fm_sqrt_area_tm1)

ggplot(daily, aes(x = sqrt(daily_area_tminus1_ha), y = log(daily_area_ha), color = biome_name)) +
  geom_point() +
  geom_smooth()

ggplot(daily, aes(x = sqrt(daily_area_tminus1_ha), y = daily_area_ha, color = biome_name)) +
  geom_point() +
  geom_smooth()

daily_with_response <-
  daily %>% 
  dplyr::left_join(daily_with_afd_summary) %>% 
  mutate(predicted_aoi_log_cumarea_tm1 = as.numeric(predict(fm_log_cum_area_tm1)),
         predicted_aoi_cumarea_tm1 = exp(predicted_aoi_log_cumarea_tm1),
         aoir_modeled_cumarea_tm1 = residuals(fm_log_cum_area_tm1),
         aoir_cumarea_tm1 = daily_area_ha - predicted_aoi_cumarea_tm1,
         predicted_aoi_log_sqrtarea_tm1 = as.numeric(predict(fm_sqrt_area_tm1)),
         predicted_aoi_sqrtarea_tm1 = exp(predicted_aoi_log_sqrtarea_tm1),
         aoir_modeled_sqrtarea_tm1 = residuals(fm_sqrt_area_tm1),
         aoir_sqrtarea_tm1 = daily_area_ha - predicted_aoi_sqrtarea_tm1
  ) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::as_tibble()


# Version 2 reformulates the area of increase model to remove the fire-level random effect
# It also removes some of the fire events outside of California and which burn outside of 2003 to 2020

# Version 3 includes a few different kinds of response variables to the daily data, and also changes the filename
# to better reflect it's true nature.
write.csv(x = daily_with_response, file = "data/out/fired/05_daily-with-behavior-metrics/fired_daily_ca_behavior-metrics.csv", row.names = FALSE)
