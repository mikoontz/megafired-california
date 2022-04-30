library(sf)
library(dplyr)
library(stringr)
library(USAboundaries)

dir.create("data/out/fired_daily_ca_epsg3310_2003-2020_subgeo", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_daily_ca_epsg3310_2003-2020", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_events_ca_epsg3310_2003-2020", recursive = TRUE, showWarnings = FALSE)

fired_events <- 
  sf::st_read("data/out/fired_events_ca.gpkg") %>% 
  dplyr::filter(ignition_year >= 2003 & ignition_year <= 2020) %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  dplyr::select(id) %>% 
  dplyr::rename(geometry = geom)
  

fired_daily <-
  sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
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

fired_daily_centroids <-
  fired_daily %>% 
  sf::st_centroid()

## determine the fires that we want to work with based on filtering criteria
# We only want fire/day combinations whose centroids are within California 
ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_geometry() %>% 
  sf::st_set_agr(value = "constant")

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

sf::st_write(obj = fired_events_out, dsn = "data/out/fired_events_ca_epsg3310_2003-2020/fired_events_ca_epsg3310_2003-2020.shp", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_out, dsn = "data/out/fired_daily_ca_epsg3310_2003-2020/fired_daily_ca_epsg3310_2003-2020.shp", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_multipart_out, dsn = "data/out/fired_daily_ca_epsg3310_2003-2020_subgeo/fired_daily_ca_epsg3310_2003-2020_subgeo.shp", delete_dsn = TRUE)

sf::st_write(obj = fired_events_out, dsn = "data/out/fired_events_ca_epsg3310_2003-2020.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_out, dsn = "data/out/fired_daily_ca_epsg3310_2003-2020.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_multipart_out, dsn = "data/out/fired_daily_ca_epsg3310_2003-2020_subgeo.gpkg", delete_dsn = TRUE)

# Expand the daily fire perimeters such that they are placed on top of 1000 random points (within each Resolve Biome)

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  dplyr::select(BIOME_NAME, ECO_NAME) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  sf::st_set_agr(value = "constant")

ca_or_nv_az <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Oregon", "Nevada", "Arizona")) %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_union() %>% 
  sf::st_geometry()

# join Resolve ecoregion and biomes to FIRED data at the event scale
events_resolve_geo <- sf::st_join(x = fired_events_out, y = resolve, largest = TRUE)
events_resolve <- sf::st_drop_geometry(events_resolve_geo)

fired_daily_working <- dplyr::left_join(x = fired_daily_out, y = events_resolve, by = "id") %>% sf::st_set_agr(value = "constant")

fired_tcf <-
  fired_daily_working %>% 
  dplyr::filter(biome_name == "Temperate Conifer Forests") %>% 
  dplyr::select(did, id, date) %>% 
  dplyr::mutate(centroid_x = sf::st_coordinates(sf::st_centroid(.))[, "X"],
                centroid_y = sf::st_coordinates(sf::st_centroid(.))[, "Y"])

tcf <- 
  resolve %>% 
  dplyr::filter(biome_name == "Temperate Conifer Forests") %>% 
  sf::st_intersection(y = ca) %>% 
  sf::st_set_agr(value = "constant")

# Buffer in from the coast, but not from the internal borders of California with other states
tcf_buff <- sf::st_intersection(x = tcf, y = sf::st_buffer(x = ca_or_nv_az, dist = -50000))

set.seed(1224)
tcf_sample_points <- 
  sf::st_sample(x = tcf_buff, size = 1000, type = "hexagonal") %>% 
  sf::st_sf() %>% 
  dplyr::mutate(samp_id = 1:nrow(.),
                x_samp = sf::st_coordinates(.)[, "X"],
                y_samp = sf::st_coordinates(.)[, "Y"]) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::as_tibble()

(start_time <- Sys.time())
# Pack each row representing the the actual fire spread geometries with a data.frame
# that represents the new set of 1,000 centroids for those fire spread geometries, then
# unnest the data to expand it (1,000 rows now for each of the fire/day combinations)
# Note this results in 6.2 million geometries
fired_tcf_with_samps <-
  fired_tcf %>% 
  dplyr::mutate(samps = list(tcf_sample_points)) %>% 
  tidyr::unnest(cols = "samps")

# What are the new centroids for each of those 6.2 million geometries?
samps_geo <-
  fired_tcf_with_samps %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x_samp", "y_samp"), crs = 3310) %>%
  sf::st_geometry()

# What are the old centroids of the actual locations of the fire/day combinations? 
fired_centroids <- 
  fired_tcf_with_samps %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 3310) %>% 
  sf::st_geometry()

# What does the affine transformation look like to slide each fire/day combination
# geometry from its actual location to the new centroid (1 of 1,000)?
affine_transformation <- samps_geo - fired_centroids

# Apply the 6.2 million affine transformations to the original fire/day combination
# geometries and set the CRS to what we know it is: EPSG3310
new_fired_geo <- (sf::st_geometry(fired_tcf_with_samps) + affine_transformation) %>% sf::st_set_crs(3310)

# Updated sf object of 6.2 million geometires now has each fire/day combination geometry
# located at 1 of 1,000 sample points within the Temperate Conifer Forest biome
fired_tcf_with_new_geo <- sf::st_set_geometry(x = fired_tcf_with_samps, value = new_fired_geo)
fired_tcf_with_new_geo_simple <-
  fired_tcf_with_new_geo %>% 
  dplyr::select(did, id, date, samp_id)

(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

# v2 used 1,000 points
# v3 used 1,000 points and the simplified sf object with just did, date, id, samp_id
sf::st_write(obj = fired_tcf_with_new_geo_simple, dsn = "data/out/fired_daily_random-locations_temperate-conifer-forests_v3.gpkg")
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 86.65956 mins
fired_tcf_with_new_geo_simple <- sf::st_read("data/out/fired_daily_random-locations_temperate-conifer-forests_v3.gpkg")

sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id <= 200, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0001-0200.gpkg")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 200 & fired_tcf_with_new_geo_simple$samp_id <= 400, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0201-0400.gpkg")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 400 & fired_tcf_with_new_geo_simple$samp_id <= 600, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0401-0600.gpkg")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 600 & fired_tcf_with_new_geo_simple$samp_id <= 800, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0601-0800.gpkg")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 800, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0801-1000.gpkg")


dir.create("data/out/fired_daily_random-locations_tcf_v3_0001-0200", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_daily_random-locations_tcf_v3_0201-0400", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_daily_random-locations_tcf_v3_0401-0600", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_daily_random-locations_tcf_v3_0601-0800", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_daily_random-locations_tcf_v3_0801-1000", recursive = TRUE, showWarnings = FALSE)

(start_time <- Sys.time())

sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id <= 200, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0001-0200/fired_daily_random-locations_tcf_v3_0001-0200.shp")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 200 & fired_tcf_with_new_geo_simple$samp_id <= 400, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0201-0400/fired_daily_random-locations_tcf_v3_0201-0400.shp")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 400 & fired_tcf_with_new_geo_simple$samp_id <= 600, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0401-0600/fired_daily_random-locations_tcf_v3_0401-0600.shp")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 600 & fired_tcf_with_new_geo_simple$samp_id <= 800, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0601-0800/fired_daily_random-locations_tcf_v3_0601-0800.shp")
sf::st_write(obj = fired_tcf_with_new_geo_simple[fired_tcf_with_new_geo_simple$samp_id > 800, ], 
             dsn = "data/out/fired_daily_random-locations_tcf_v3_0801-1000/fired_daily_random-locations_tcf_v3_0801-1000.shp")

(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

































# fired_daily_multipart <-
#   sf::st_cast(x = fired_daily, to = "POLYGON") %>% 
#   dplyr::mutate(subgeo_id = str_split_fixed(string = rownames(.), pattern = "\\.", n = 2)[, 2],
#                 subgeo_id = ifelse(subgeo_id == "", yes = 0, no = as.numeric(subgeo_id))) %>% 
#   dplyr::select(did, id, date, subgeo_id, samp_id) %>% 
#   dplyr::mutate(did_subgeo = paste(did, subgeo_id, sep = "_")) %>%
#   sf::st_set_agr(value = "constant") 
# 
# fired_daily_multipart_centroids <-
#   fired_daily_multipart %>% 
#   sf::st_centroid()
# 
# target_fires <- 
#   fired_daily_multipart_centroids %>% 
#   dplyr::filter(sf::st_intersects(x = ., y = ca, sparse = FALSE))
# 
# idx <- which(!(fired_daily_multipart_centroids$did_subgeo %in% target_fires$did_subgeo))
# 
# bad_id <- unique(fired_daily_multipart_centroids$id[idx])
# bad_did <- unique(fired_daily_multipart_centroids$did[idx])
# bad_did_subgeo <- unique(fired_daily_multipart_centroids$did_subgeo[idx])
# 
# fired_events_out <-
#   fired_events %>% 
#   dplyr::filter(!(id %in% bad_id))
# 
# fired_daily_out <- 
#   fired_daily %>% 
#   dplyr::filter(!(id %in% bad_id))
# 
# fired_daily_multipart_out <-
#   fired_daily_multipart %>% 
#   dplyr::filter(!(id %in% bad_id))


# dir.create("data/out/fired_events_ca_ewe_rank", recursive = TRUE, showWarnings = FALSE)
# dir.create("data/out/fired_daily_ca_ewe_rank", recursive = TRUE, showWarnings = FALSE)
# 
# fired_events <- 
#   sf::st_read("data/out/fired_events_ca.gpkg") %>% 
#   sf::st_transform(4326) %>% 
#   rename(ig_date = ignition_date,
#          ig_day = ignition_day,
#          ig_month = ignition_month,
#          ig_year = ignition_year,
#          event_dur = event_duration,
#          tot_px = total_pixels,
#          tot_ar_km2 = total_area_km2,
#          fsr_px_dy = fsr_pixels_per_day,
#          fsr_km2_dy = fsr_km2_per_day,
#          mx_grw_px = max_growth_pixels,
#          mn_grw_px = min_growth_pixels,
#          mu_grw_px = mean_growth_pixels,
#          mx_grw_km2 = max_growth_km2,
#          mn_grw_km2 = min_growth_km2,
#          mu_grw_km2 = mean_growth_km2,
#          mx_grw_dte = max_growth_date,
#          lc_code = landcover_code,
#          lc_mode = landcover_mode,
#          lc_name = lc_name,
#          lc_desc = lc_description,
#          lc_type = landcover_type,
#          ig_utm_x = ignition_utm_x,
#          ig_utm_y = ignition_utm_y,
#          tot_perim = final_perimeter,
#          geometry = geom) %>% 
#   dplyr::select(-ig_utm_x, -ig_utm_y)
# 
# fired_daily <-
#   sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
#   sf::st_transform(4326) %>% 
#   rename(ig_date = ignition_date,
#          ig_day = ignition_day,
#          ig_month = ignition_month,
#          ig_year = ignition_year,
#          event_dur = event_duration,
#          tot_px = total_pixels,
#          tot_ar_km2 = total_area_km2,
#          fsr_px_dy = fsr_pixels_per_day,
#          fsr_km2_dy = fsr_km2_per_day,
#          mx_grw_px = max_growth_pixels,
#          mn_grw_px = min_growth_pixels,
#          mu_grw_px = mean_growth_pixels,
#          mx_grw_km2 = max_growth_km2,
#          mn_grw_km2 = min_growth_km2,
#          mu_grw_km2 = mean_growth_km2,
#          mx_grw_dte = max_growth_date,
#          lc_code = landcover_code,
#          lc_mode = landcover_mode,
#          lc_name = lc_name,
#          lc_desc = lc_description,
#          lc_type = landcover_type,
#          ig_utm_x = ignition_utm_x,
#          ig_utm_y = ignition_utm_y,
#          day_ar_km2 = daily_area_km2,
#          day_ar_ha = daily_area_ha,
#          cum_ar_t = cum_area_ha,
#          cum_ar_tm1 = cum_area_ha_tminus1,
#          geometry = geom) %>% 
#   dplyr::select(-ig_utm_x, -ig_utm_y)
# 
# ewe_events <- 
#   read.csv("data/out/extreme-wildfire-events-ranking_v5.csv") %>% 
#   dplyr::filter(ignition_year <= 2020) %>% 
#   dplyr::rename(olap_frap = overlapping_pct_frap, 
#                 ig_date = ignition_date, 
#                 ig_year = ignition_year, 
#                 ig_month = ignition_month, 
#                 date_frp = acq_date_frp, 
#                 date_aoir = acq_date_aoir,
#                 tot_hect = total_area_ha,
#                 pred_aoi = predicted_aoi,
#                 act_aoi = actual_aoi_during_max_aoir,
#                 c_area_tm1 = cum_area_ha_tminus1,
#                 date_aoi = acq_date_aoi,
#                 e_day_aoir = event_day_aoir,
#                 e_day_aoi = event_day_aoi,
#                 event_dur = event_duration,
#                 model_aoir = modeled_max_aoir) %>% 
#   dplyr::mutate(megafire = ifelse(mecdf >= 0.9, yes = "megafire", "non-megafire")) %>% 
#   dplyr::select(id, megafire, everything())
# 
# fired_events_ee <-
#   fired_events %>% 
#   dplyr::filter(ig_year <= 2020) %>% 
#   dplyr::select(id) %>% 
#   dplyr::left_join(ewe_events)
# 
# # daily product join
# ewe_daily <- 
#   read.csv("data/out/extreme-wildfire-daily-ranking_v5.csv") %>% 
#   dplyr::filter(ignition_year <= 2020) %>% 
#   dplyr::rename(olap_frap = overlapping_pct_frap, 
#                 ig_date = ignition_date, 
#                 ig_year = ignition_year, 
#                 ig_month = ignition_month, 
#                 tot_hect = total_area_ha,
#                 aoi_ha = daily_area_ha,
#                 pred_aoi = predicted_aoi,
#                 pred_aoi_l = predicted_aoi_log,
#                 aoir_mod = aoir_modeled,
#                 c_area_ha = cum_area_ha,
#                 c_area_tm1 = cum_area_ha_tminus1,
#                 event_dur = event_duration) %>% 
#   dplyr::mutate(megafire = ifelse(mecdf >= 0.9, yes = "megafire", "non-megafire")) %>% 
#   dplyr::select(id, megafire, everything())
# 
# fired_daily_ee <-
#   fired_daily %>% 
#   dplyr::filter(ig_year <= 2020 & ig_year >= 2003) %>% 
#   dplyr::select(did) %>% 
#   dplyr::left_join(ewe_daily)
# 
# sf::st_write(obj = fired_events_ee, dsn = "data/out/fired_events_ca_ewe_rank_v5.gpkg", delete_dsn = TRUE)
# sf::st_write(obj = fired_daily_ee, dsn = "data/out/fired_daily_ca_ewe_rank_v5.gpkg", delete_dsn = TRUE)
# 
# sf::st_write(obj = fired_events_ee, dsn = "data/out/fired_events_ca_ewe_rank/fired_events_ca_ewe_rank_v5.shp", delete_dsn = TRUE)
# sf::st_write(obj = fired_daily_ee, dsn = "data/out/fired_daily_ca_ewe_rank/fired_daily_ca_ewe_rank_v5.shp", delete_dsn = TRUE)
# 
# ggplot(fired_events_ee, aes(x = ig_month)) +
#   geom_histogram() +
#   facet_wrap(facets = "megafire")
# 
# ggplot(fired_daily_ee, aes(x = ig_year)) +
#   geom_histogram() +
#   facet_wrap(facets = "megafire", scales = "free_y")
# 
# fired_events_ee %>% 
#   st_drop_geometry() %>% 
#   count(megafire, ig_year) %>% 
#   tidyr::pivot_wider(id_cols = ig_year, names_from = "megafire", values_from = "n") %>% 
#   dplyr::mutate(prop_mega = megafire / (megafire + `non-megafire`)) %>% 
#   as.data.frame()
