# identifying extreme wildfire events along simultaneous axes of biggest, most intense, fastest

library(sf)
library(dplyr)
library(mgcv)
library(ggplot2)
library(purr)
library(data.table)

daily <- 
  sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
  dplyr::mutate(lc_name = as.factor(lc_name))
events <- sf::st_read("data/out/fired_events_ca.gpkg")

fired_frap_mtbs_join <- read.csv(file = "data/out/fired-frap-mtbs-join.csv")

modis_afd <- 
  data.table::fread("data/out/mcd14ml_ca.csv") %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

n_workers <- 10

fired_with_afd <- 
  sf::st_join(events, modis_afd) %>% 
  filter(acq_date >= ignition_date & acq_date <= last_date) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

# Total fire size
biggest_size <-
  events %>% 
  dplyr::select(id, total_area_ha) %>% 
  st_drop_geometry()

# Max FRP
biggest_frp <-
  fired_with_afd %>% 
  st_drop_geometry() %>%
  group_by(id) %>% 
  filter(confidence > 90 & pixel_area < 2 & frp_per_area == max(frp_per_area)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id, frp_per_area)

# biggest deviation from expected area of increase (i.e., fastest)
fm1 <- mgcv::gam(daily_area_ha ~ s(cum_area_ha_tminus1, by = lc_name) + s(id, bs = "re"), 
                 method = "REML",
                 data = daily)

biggest_area_of_increase_residual <-
  daily %>% 
  mutate(preds = predict(fm1),
         area_of_increase_residual = residuals(fm1)) %>% 
  dplyr::select(id, area_of_increase_residual) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(area_of_increase_residual == max(area_of_increase_residual))

ggplot(daily, aes(x = cum_area_ha_tminus1, y = daily_area_ha)) + 
  geom_point() +
  geom_smooth()

# biggest area of increase
biggest_area_of_increase <-
  daily %>% 
  dplyr::select(id, daily_area_ha) %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(daily_area_ha == max(daily_area_ha))

# join all together

ranking_ewe <-
  events %>% 
  dplyr::select(-total_area_ha) %>% 
  left_join(biggest_size) %>% 
  left_join(biggest_frp) %>% 
  left_join(biggest_area_of_increase_residual) %>%
  left_join(biggest_area_of_increase) %>% 
  mutate(frp_per_area = ifelse(is.na(frp_per_area), yes = 0, no = frp_per_area)) %>%
  mutate(size_rank = rank(-total_area_ha)) %>% 
  mutate(frp_rank = rank(-frp_per_area)) %>% 
  mutate(aoir_rank = rank(-area_of_increase_residual)) %>% 
  mutate(aoi_rank = rank(-daily_area_ha)) %>% 
  left_join(fired_frap_mtbs_join, by = c(id = "id_fired"))

ranking_ewe %>% arrange(size_rank) %>% st_drop_geometry()
ranking_ewe %>% arrange(aoi_rank) %>% st_drop_geometry() %>% mutate(rank_diff = aoir_rank - aoi_rank) %>% filter(rank_diff == min(rank_diff))

pairs(x = st_drop_geometry(ranking_ewe[, c("total_area_ha", "frp_per_area", "area_of_increase_residual")]))

ranked_ewe <- 
  ranking_ewe %>% 
  dplyr::select(id, total_area_ha, frp_per_area, area_of_increase_residual) %>%
  st_drop_geometry() %>%
  mutate(mecdf = pmap_dbl(., .f = function(id, total_area_ha, frp_per_area, area_of_increase_residual) {
    mean(.[, "total_area_ha"] <= total_area_ha & .[, "frp_per_area"] <= frp_per_area & .[, "area_of_increase_residual"] <= area_of_increase_residual)
    })) %>% 
  mutate(ewe_rank = rank(-mecdf)) %>% 
  left_join(ranking_ewe) %>% 
  dplyr::arrange(ewe_rank) %>% 
  as_tibble() %>% 
  dplyr::select(id, name_frap, ignition_date, ignition_year, ewe_rank, size_rank, frp_rank, aoir_rank, mecdf, everything())

ranked_ewe

ranked_ewe %>% filter(name_frap == "KING")
ranked_ewe %>% filter(name_frap == "TUBBS")

ggplot(ranked_ewe, aes(x = mecdf)) + 
  geom_histogram()

ggplot(ranked_ewe, aes(x = ignition_date, y = total_area_ha)) + 
  geom_point() +
  geom_smooth()

ggplot(ranked_ewe, aes(x = ignition_date, y = area_of_increase_residual)) + 
  geom_point() +
  geom_smooth()

ggplot(ranked_ewe, aes(x = ignition_date, y = frp_per_area)) + 
  geom_point() +
  geom_smooth() +
  scale_y_log10()

ggplot(ranked_ewe, aes(x = ignition_date, y = mecdf)) + 
  geom_point() +
  geom_smooth()

ggplot(ranked_ewe, aes(x = ignition_month, y = mecdf)) + 
  geom_point() +
  geom_smooth()
