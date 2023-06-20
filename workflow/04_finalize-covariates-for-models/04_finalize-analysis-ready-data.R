# Combine all drivers info so they are analysis ready
library(dplyr)
library(sf)
library(tidyr)
library(data.table)
library(tidyselect)
library(spatialsample)
library(purrr)
library(lubridate)
library(here)

local_out_dir <- here::here("data", "ard", lubridate::today())
gdrive_out_dir <- file.path("D:", "google-drive_cu-boulder", "My Drive", "_projects", "moore-foundation", "manuscripts", "megafired-california", "data", "ard", lubridate::today())

dir.create(local_out_dir, 
           recursive = TRUE,
           showWarnings = FALSE)
dir.create(gdrive_out_dir, 
           recursive = TRUE,
           showWarnings = FALSE)

# fluc_static <- 
#   data.table::fread("data/out/drivers/fluc-static-driver-proportion-percentiles.csv") |>
#   dplyr::select(did, id, date,
#                 elevation, rumple_index, caltrans_road_density_mpha,
#                 ndvi, veg_structure_rumple, 
#                 peak_ridge_cliff, valleys, slope_warm, slope_cool, slope_neutral, 
#                 trees_tm01, shrubs_tm01, grass_forb_herb_tm01, barren_tm01,
#                 landform_diversity, landcover_diversity_tm01)
fluc_static <- 
  data.table::fread("data/out/drivers/fluc-static-driver-proportion-percentiles.csv") |>
  dplyr::select(did, id, date,
                elevation, rumple_index, caltrans_road_density_mpha,
                ndvi, veg_structure_rumple, 
                peak_ridge_cliff, valleys, slope_warm, slope_cool, slope_neutral, flat, 
                trees_tm01, shrubs_tm01, grass_forb_herb_tm01, barren_tm01,
                landform_diversity, landcover_diversity_tm01)

# 2020 Creek Fire
# landfire[id == 135921, ]
# Use full 10 year disturbance history for more lumping
# |>
#  dplyr::select(did, id, date, 
#                fire_high_tm01_tm10,
#                fire_tm01_tm10,
#                clearcut_harvest_othermech_tm01_tm10,
#                fuel_trt_tm01_tm10,
#                insect_disease_high_tm01_tm10, 
#                insect_disease_tm01_tm10)

# Late December, 2022 version which includes less prominent disturbance types (a bad
# idea we think!)
# landfire <- 
#   data.table::fread("data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv") |>
#   dplyr::select(did, id, date,
#                 fire_high_tm01_tm05, fire_high_tm06_tm10,
#                 fire_not_high_tm01_tm05, fire_not_high_tm06_tm10,
#                 clearcut_harvest_othermech_tm01_tm05, clearcut_harvest_othermech_tm06_tm10,
#                 fuel_trt_tm01_tm05, fuel_trt_tm06_tm10,
#                 insect_disease_high_tm01_tm05, insect_disease_high_tm06_tm10,
#                 insect_disease_not_high_tm01_tm05, insect_disease_not_high_tm06_tm10)

# landfire <- 
#   data.table::fread("data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv") |>
#   dplyr::select(did, id, date, fire_tm01_tm10)

landfire <- 
  data.table::fread("data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv") |>
  dplyr::select(did, id, date,
                fire_high_tm01_tm05, fire_high_tm06_tm10,
                fire_not_high_tm01_tm05, fire_not_high_tm06_tm10,
                insect_disease_tm01_tm10)

# ERA5 Land data
weather_drivers <- 
  data.table::fread("data/out/drivers/weather-drivers-as-percentiles.csv") |>
  dplyr::select(!tidyselect::contains("rtma")) |> # not the RTMA drivers; use the ERA5 drivers in their place
  dplyr::select(did, id, date,
                wind_dir_ns_era5, wind_dir_ew_era5,
                wind_anisotropy_ns_era5, wind_anisotropy_ew_era5,
                min_wind_speed_era5_pct, max_wind_speed_era5_pct,
                min_rh_era5_pct, max_rh_era5_pct, min_temp_era5_pct, max_temp_era5_pct, min_vpd_era5_pct, max_vpd_era5_pct,
                spei14d, spei30d, spei90d, spei180d, spei270d, spei1y, spei2y, spei5y, pdsi_z,
                erc_pct, bi_pct, fm100_pct, fm1000_pct) %>% 
  dplyr::filter(did %in% fluc_static$did)


# RTMA weather data
# weather_drivers <- 
#   data.table::fread("data/out/drivers/weather-drivers-as-percentiles.csv") |>
#   dplyr::select(!tidyselect::contains("era5")) |> # not the ERA5 drivers; use the RTMA drivers in their place
#   dplyr::select(did, id, date,
#                 wind_dir_ns_rtma, wind_dir_ew_rtma,
#                 wind_anisotropy_ns_rtma, wind_anisotropy_ew_rtma,
#                 min_wind_speed_rtma_pct, max_wind_speed_rtma_pct, min_wind_filled_gust_rtma_pct, max_wind_filled_gust_rtma_pct,
#                 min_rh_rtma_pct, max_rh_rtma_pct, min_temp_rtma_pct, max_temp_rtma_pct, min_vpd_rtma_pct, max_vpd_rtma_pct,
#                 spei14d, spei30d, spei90d, spei180d, spei270d, spei1y, spei2y, spei5y, pdsi_z,
#                 erc_pct, bi_pct, fm100_pct, fm1000_pct) %>% 
#   dplyr::filter(did %in% fluc_static$did)

# weather_drivers <- 
#   data.table::fread("data/out/drivers/weather-drivers-as-percentiles.csv") |>
#   dplyr::select(!tidyselect::contains("era5")) |> # not the ERA5 drivers; use the RTMA drivers in their place
#   dplyr::select(did, id, date,
#                 wind_anisotropy_rtma, wind_terrain_anisotropy_rtma, min_wind_terrain_alignment_rtma_pct, max_wind_terrain_alignment_rtma_pct,
#                 min_wind_speed_rtma_pct, max_wind_speed_rtma_pct, min_wind_filled_gust_rtma_pct, max_wind_filled_gust_rtma_pct,
#                 min_rh_rtma_pct, max_rh_rtma_pct, min_temp_rtma_pct, max_temp_rtma_pct, min_vpd_rtma_pct, max_vpd_rtma_pct,
#                 spei14d, spei30d, spei90d, spei180d, spei270d, spei1y, spei2y, spei5y, pdsi_z,
#                 erc_pct, bi_pct, fm100_pct, fm1000_pct) %>% 
#   dplyr::filter(did %in% fluc_static$did)

other_fires_summary <- 
  data.table::fread("data/out/drivers/other-fires-summary.csv") |>
  dplyr::select(-concurrent_fires, -cumu_count, -cumu_area_ha) %>% 
  dplyr::filter(did %in% fluc_static$did)

# Remove fires that never reached more than 121 hectares (300 acres)
target_event_ids <-
  sf::read_sf("data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  dplyr::filter(area_ha >= 121.406) %>% 
  dplyr::pull(id)

# RTMA means we can only use data back to 2011
# drivers <- 
#   merge(x = weather_drivers, y = other_fires_summary, by = c("did", "id", "date"), all = TRUE) |>
#   merge(y = fluc_static, by = c("did", "id", "date"), all = TRUE) |>
#   merge(y = landfire, by = c("did", "id", "date"), all = TRUE) |>
#   dplyr::filter(id %in% target_event_ids) |>
#   dplyr::filter(date >= as.Date("2011-01-01"))

# Using ERA5 means we can go back to 2003
drivers <-
  merge(x = weather_drivers, y = other_fires_summary, by = c("did", "id", "date"), all = TRUE) |>
  merge(y = fluc_static, by = c("did", "id", "date"), all = TRUE) |>
  merge(y = landfire, by = c("did", "id", "date"), all = TRUE) |>
  dplyr::filter(id %in% target_event_ids) |>
  dplyr::filter(date >= as.Date("2003-01-01"))

# For defining "ewe" or not, what is the percentage threshold? E.g., 0.95 means an "ewe" is in the top
# 5th percentile for daily area of increase
pct_threshold <- 0.95

# Defining "ewe" as whether daily area of increase was >95th percentile for the biome
biome_lookup <- 
  tibble::tibble(biome_name_daily = c("Temperate Conifer Forests", 
                                      "Mediterranean Forests, Woodlands & Scrub", 
                                      "Temperate Grasslands, Savannas & Shrublands", 
                                      "Deserts & Xeric Shrublands"),
                 biome_shortname = c("tcf", "mfws", "tgss", "dxs"))

# We need to get X/Y coordinates for biggest polygon
fired_biggest_poly <- 
  data.table::fread("data/out/fired/03_joined-with-other-data/fired-biggest-poly-info.csv") |>
  dplyr::select(did, id, date, samp_id, x_biggest_poly_3310, y_biggest_poly_3310)

fires <-
  data.table::fread("data/out/fired/05_daily-with-behavior-metrics/fired_daily_ca_behavior-metrics.csv") |>
  dplyr::mutate(area_log10 = log10(daily_area_ha),
                sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha),
                fireline_length_proxy_km = sqrt((sqrt_aoi_tm1^2*1e4)/pi)/1e3*pi) |>
  dplyr::rename(cumu_area_tm01 = cum_area_ha_tminus1) |>
  dplyr::left_join(biome_lookup, by = "biome_name_daily")

fires <-
  merge(x = fires, y = fired_biggest_poly, by = c("did", "id", "date", "samp_id")) |>
  dplyr::select(did, id, date, biome_shortname, biome_name_daily, eco_name_daily,  x_biggest_poly_3310, y_biggest_poly_3310, 
                daily_area_ha, area_log10, fireline_length_proxy_km, sqrt_aoi_tm1, event_day, cumu_area_tm01)

fires

# Merge fire data with drivers data
fires_drivers <- 
  merge(drivers, fires, by = c("did", "id", "date")) |>
  # dplyr::group_by(biome_name_daily) |> 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10),
                ewe = ifelse(area_log10_pct >= pct_threshold, yes = 1, no = 0)) |> 
  # dplyr::ungroup() |>
  dplyr::mutate(early_late = as.numeric(event_day <= 7)) %>% 
  as.data.frame()

# 95th percentile growth for temperate conifer forests is 2787 ha per day
# 95th percentile growth for Mediterranean Forest, Woodland & Scrub is 3342 ha per day
# 95th percentile growth for temperate grasslands, savannas & shrublands is 217 ha per day
# 95th percentile growth for desert & xeric shrublands is 2377 ha per day
fires_drivers %>%
  filter(ewe == 1) %>%
  group_by(biome_name_daily) %>%
  filter(daily_area_ha == min(daily_area_ha)) %>%
  select(biome_name_daily, daily_area_ha)

### Prep for individual biomes
driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
predictor.variable.names <- driver_descriptions$variable

# # Summaries of static, fluc, and landfire disturbance data
# static_fluc_summary <- 
#   data.table::fread("data/out/drivers/fluc-static-driver-proportion-percentiles_long.csv") %>% 
#   dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)
# 
# lf_summary <- 
#   data.table::fread("data/out/drivers/landfire-disturbance-driver-proportion-percentiles_long.csv") %>% 
#   dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)
# 
# Summary of weather data
# weather_drivers_summary <-
#   weather_drivers %>%
#   dplyr::select(tidyselect::all_of(c("did", driver_descriptions$variable[driver_descriptions$type == "weather"]))) %>%
#   data.table::as.data.table() %>%
#   data.table::melt(id.vars = c("did"), value.name = "measured_value") %>%
#   dplyr::mutate(expected_value = 0.5,
#                 expected_value_median = 0.5,
#                 adj = measured_value,
#                 diff = measured_value - expected_value,
#                 diff_median = measured_value - expected_value_median) %>%
#   dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj) %>% 
#   dplyr::mutate(expected_value = ifelse(variable %in% c("wind_dir_ns_rtma", "wind_dir_ew_rtma", "wind_anisotropy_ns_rtma", "wind_anisotropy_ew_rtma"),
#                                         yes = NA,
#                                         no = expected_value),
#                 expected_value_median = ifelse(variable %in% c("wind_dir_ns_rtma", "wind_dir_ew_rtma", "wind_anisotropy_ns_rtma", "wind_anisotropy_ew_rtma"),
#                                                yes = NA,
#                                                no = expected_value_median))
# 
# nonnormalized_summary <-
#   fires_drivers %>% 
#   dplyr::select(tidyselect::all_of(c("did", "ewe", "biome_shortname", driver_descriptions$variable[driver_descriptions$type %in% c("fire")], "npl", "short_concurrent_fires"))) %>% 
#   data.table::as.data.table() %>% 
#   data.table::melt(id.vars = c("did", "ewe", "biome_shortname"), value.name = "measured_value") %>% 
#   dplyr::mutate(expected_value = NA,
#                 expected_value_median = NA,
#                 adj = measured_value,
#                 diff = measured_value - expected_value,
#                 diff_median = measured_value - expected_value_median) %>% 
#   dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)
# 
# all_summary <-
#   rbind(static_fluc_summary, lf_summary, weather_drivers_summary, nonnormalized_summary) %>% 
#   dplyr::left_join(fires_drivers[, c("did", "ewe", "biome_shortname", "daily_area_ha")]) %>% 
#   dplyr::filter(!is.na(biome_shortname))

set.seed(2308)

lapply(X = biome_lookup$biome_shortname, FUN = function(biome_shortname) {
  
  out <- fires_drivers[fires_drivers$biome_shortname == biome_shortname, ]
  
  # Which variables have lots of NAs?
  apply(out[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))
  
  # Drop all rows that have an NA in any column
  out <- out[complete.cases(out), ]
  
  out <- out[, c("did", "event_day", "daily_area_ha", "cumu_area_tm01", "ewe", "biome_name_daily", "biome_shortname", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
  
  # Figure out which variables will exhibit badly scale-dependent behavior 
  # (more than 50% of expected median values for EWE or non-EWE)
  
  # biome_driver_summary <- all_summary[all_summary$biome_shortname == biome_shortname, ]
  # 
  # biome_driver_summary %>%
  #   dplyr::filter(did %in% out$did) %>% 
  #   dplyr::filter(daily_area_ha >= 25) %>% 
  #   group_by(variable, ewe) %>%
  #   summarize(mean = mean(expected_value_median),
  #             median = median(expected_value_median),
  #             n_zeroes = length(which(expected_value_median == 0)),
  #             n = n(),
  #             prop_zeroes = n_zeroes / n) %>%
  #   arrange(median) %>% 
  #   print(n = 100)
  # 
  
  
  # set up spatial folds
  # https://spatialsample.tidymodels.org/articles/spatialsample.html
  out <-
    out %>% 
    sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), 
                 crs = 3310, remove = FALSE) %>%
    spatialsample::spatial_clustering_cv(v = 10) %>% 
    purrr::pmap(.f = function(id, splits) {
      spatial_fold <- id
      assessment_data <- 
        splits %>% 
        rsample::assessment() %>% 
        sf::st_drop_geometry()
      
      return(cbind(assessment_data, spatial_fold))
    }) %>% 
    data.table::rbindlist()
  
  # Remove spatial folds with fewer than 40 observations or 2 EWE's
  n_in_fold <- 
    out %>% 
    dplyr::group_by(spatial_fold) %>% 
    dplyr::summarize(n = n(),
                     n_ewe = length(which(ewe == 1)))
  
  enough_n_folds <- 
    n_in_fold %>% 
    dplyr::filter(n_ewe >= 1) %>% 
    dplyr::pull(spatial_fold)
  
  out <- out[spatial_fold %in% enough_n_folds, ]
  
  # no columnns with 0 variance (rounded to 4 decimal places) across whole dataset
  zero_variance_columns <-
    out %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(predictor.variable.names),
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(zero_var = any(round(var(value, na.rm = TRUE), digits = 4) == 0)) %>%
    dplyr::filter(zero_var) %>%
    dplyr::pull(variable)
  
  # no columnns with 0 variance (rounded to 4 decimal places) in any folds
  # particularly important if using sequential knockoffs (as you might be if
  # you have factors as features)
  
  # zero_variance_columns <- 
  #   out %>% 
  #   tidyr::pivot_longer(cols = tidyselect::all_of(predictor.variable.names),
  #                       names_to = "variable",
  #                       values_to = "value") %>% 
  #   dplyr::group_by(spatial_fold, variable) %>% 
  #   dplyr::summarize(zero_var = round(var(value, na.rm = TRUE), digits = 4) == 0) %>% 
  #   dplyr::group_by(variable) %>% 
  #   dplyr::summarize(zero_var = any(zero_var)) %>% 
  #   dplyr::filter(zero_var) %>% 
  #   dplyr::pull(variable)
  
  out <- 
    out %>% 
    dplyr::select(!tidyselect::all_of(zero_variance_columns))
  
  data.table::fwrite(x = out, file = paste0(local_out_dir, "/daily-drivers-of-california-megafires_", biome_shortname,".csv"))
  
  file.copy(from = paste0(local_out_dir, "/daily-drivers-of-california-megafires_", biome_shortname,".csv"),
            to = paste0(gdrive_out_dir, "/daily-drivers-of-california-megafires_", biome_shortname,".csv"))
  
  return(NULL)
})
