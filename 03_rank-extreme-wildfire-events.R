# identifying extreme wildfire events along simultaneous axes of biggest, most intense, fastest

library(sf)
library(dplyr)
library(mgcv)
library(ggplot2)
library(purrr)
library(data.table)
library(patchwork)
library(plotly)

daily <- 
  sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
  dplyr::mutate(lc_name = as.factor(lc_name))

events <- sf::st_read("data/out/fired_events_ca.gpkg")

fired_frap_mtbs_join <- read.csv(file = "data/out/fired-frap-mtbs-join.csv")

modis_afd <- 
  data.table::fread("data/out/mcd14ml_ca.csv", colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

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
biggest_size <-
  events %>% 
  dplyr::select(id, total_area_ha) %>% 
  st_drop_geometry() %>% 
  as_tibble()

# 90th percentile FRP of the fire to try to account for
# saturation of individual FRP measurements
biggest_frp <-
  fired_with_afd %>% 
  st_drop_geometry() %>%
  # filter(confidence > 90 & pixel_area < 2) %>% 
  filter(confidence > 50 & pixel_area < 2) %>%
  group_by(id, acq_date) %>% 
  dplyr::summarize(frp_90 = quantile(x = frp_per_area, probs = 0.9, na.rm = TRUE, names = FALSE)) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(id) %>% 
  dplyr::filter(frp_90 == max(frp_90)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id, frp_90, acq_date) %>% 
  dplyr::rename(acq_date_frp = acq_date)

biggest_frp

ggplot(daily, aes(x = cum_area_ha_tminus1, y = daily_area_ha, color = lc_name)) +
  geom_point()

# biggest deviation from expected area of increase (i.e., fastest)
# fm1 <- mgcv::gam(daily_area_ha ~ s(cum_area_ha_tminus1, by = lc_name) + s(id, bs = "re"), 
#                  method = "REML",
#                  data = daily)

# use the log scale to better approximate predicted growth (can't be negative this way!)
# Add 0.1 to all cumulative area burned so that the value for each fire's first day
# isn't NaN when taking log(0) 
fm1 <- mgcv::gam(log(daily_area_ha) ~ s(log(cum_area_ha_tminus1 + 0.1), by = lc_name) + s(id, bs = "re"), 
                 method = "REML",
                 data = daily)

biggest_area_of_increase_residual <-
  daily %>% 
  mutate(preds_raw = predict(fm1),
         preds = exp(preds_raw),
         area_of_increase_residual = residuals(fm1)) %>% 
  dplyr::select(id, area_of_increase_residual, event_day, daily_area_ha, preds, cum_area_ha_tminus1, date,
                preds_raw) %>%
  dplyr::rename(acq_date_aoir = date,
                predicted_aoi = preds,
                event_day_aoir = event_day,
                predicted_aoi_log = preds_raw) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(area_of_increase_residual == max(area_of_increase_residual)) %>% 
  dplyr::rename(modeled_max_aoir = area_of_increase_residual,
                actual_aoi_during_max_aoir = daily_area_ha) %>% 
  dplyr::mutate(aoir_max = actual_aoi_during_max_aoir - predicted_aoi) %>% 
  dplyr::ungroup()

biggest_area_of_increase_residual

aoi_gg <- 
  ggplot(daily, aes(x = (cum_area_ha_tminus1 + 0.1), y = daily_area_ha)) + 
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = expression("Cumulative area burned on day" ~ italic("t-1") ~ "(ha)"),
       y = expression("Area of increase on day" ~ italic("t") ~ "(ha)")) +
  theme_bw()

aoi_gg

ggsave(filename = "figs/modeled-area-of-increase-vs-cumulative-area-burned.png", dpi = 300, width = 5, height = 5, units = "in")

# biggest area of increase
biggest_area_of_increase <-
  daily %>% 
  dplyr::select(id, daily_area_ha, event_day, date) %>% 
  dplyr::rename(acq_date_aoi = date,
                event_day_aoi = event_day) %>%
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(daily_area_ha == max(daily_area_ha)) %>% 
  # One fire had it's maximum area of increase on two separate days (the minimum possible area of increase-- one pixel) so just take the first day that happens
  # using arrange() then slice(1)
  dplyr::arrange(event_day_aoi) %>% 
  dplyr::slice(1) %>% 
  dplyr::rename(aoi_max = daily_area_ha) %>% 
  dplyr::ungroup()

# join all together
events
biggest_size
biggest_frp
biggest_area_of_increase_residual
biggest_area_of_increase

# code if using daily 90th percentile FRP
ranking_ewe <-
  events %>% 
  dplyr::select(-total_area_ha) %>% 
  left_join(biggest_size) %>% 
  left_join(biggest_frp) %>% 
  left_join(biggest_area_of_increase_residual) %>%
  left_join(biggest_area_of_increase) %>% 
  mutate(frp_90 = ifelse(is.na(frp_90), yes = 0, no = frp_90)) %>%
  mutate(size_rank = rank(-total_area_ha)) %>% 
  mutate(frp_rank = rank(-frp_90)) %>% 
  mutate(aoir_rank = rank(-aoir_max)) %>% 
  mutate(aoi_rank = rank(-aoi_max)) %>% 
  left_join(fired_frap_mtbs_join, by = c(id = "id_fired")) %>% 
  sf::st_drop_geometry()

pairs(x = st_drop_geometry(ranking_ewe[, c("total_area_ha", "frp_90", "aoir_max")]))

ranked_ewe <- 
  ranking_ewe %>% 
  dplyr::filter(ignition_year <= 2020) %>% 
  dplyr::select(id, total_area_ha, frp_90, aoir_max) %>%
  mutate(mecdf = pmap_dbl(., .f = function(id, total_area_ha, frp_90, aoir_max) {
    mean(.[, "total_area_ha"] <= total_area_ha & .[, "frp_90"] <= frp_90 & .[, "aoir_max"] <= aoir_max)
  })) %>% 
  mutate(ewe_rank = rank(-mecdf)) %>% 
  left_join(ranking_ewe) %>%
  dplyr::arrange(ewe_rank) %>% 
  as_tibble() %>% 
  dplyr::select(id, name_frap, ignition_date, ignition_year, ewe_rank, size_rank, frp_rank, aoir_rank, aoi_rank, mecdf, everything())

ranked_ewe_out <- 
  ranked_ewe %>% 
  dplyr::select(id, name_frap, overlapping_pct_frap, ignition_date, ignition_year, ignition_month, last_date, ewe_rank, size_rank, frp_rank, aoir_rank, aoi_rank, mecdf, total_area_ha, frp_90, aoir_max, aoi_max, predicted_aoi, actual_aoi_during_max_aoir, cum_area_ha_tminus1, acq_date_frp, acq_date_aoir, acq_date_aoi, event_day_aoir, event_day_aoi, event_duration, modeled_max_aoir)

write.csv(x = ranked_ewe_out, file = "data/out/extreme-wildfire-events-ranking.csv", row.names = FALSE)
