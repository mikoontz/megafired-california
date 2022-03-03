# assemble daily-scale drivers

library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(pbapply)

dir.create("data/out/analysis-ready", showWarnings = FALSE)

#### --- input data
hourly_drivers <- data.table::fread("data/out/ee/FIRED-hourly-drivers_california.csv")

daily_drivers_list <-
  list.files("data/out/ee/",
             pattern = "FIRED-daily-drivers_california_",
             full.names = TRUE) %>%
  lapply(FUN = fread)


## read in various forms of the FIRED data
fired_daily <-
  sf::st_read("data/out/fired_daily_ca_ewe_rank.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(date = lubridate::ymd(date),
                ig_date = lubridate::ymd(ig_date)) %>% 
  dplyr::select(did, id, date, everything())

fired_events <- 
  sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>% 
  dplyr::rename(geometry = geom)

npl <- 
  read.csv("data/out/national-preparedness-level.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste0(year, "-", month, "-", day))) %>% 
  dplyr::select(-c(year, month, day)) %>% 
  as.data.table()

r_era5 <- 
  terra::rast("data/out/era5-weather-percentiles_1981-01-01_2020-12-31.tif")

r_gridmet <-
  terra::rast("data/out/gridmet-weather-percentiles_1981-01-01_2020-12-31.tif")

#### --- hourly drivers prep

hourly_drivers <- hourly_drivers[date < "2021-01-01", ]
hourly_drivers[, `:=`(rh = ea / esat,
                      `system:index` = NULL, megafire = NULL,
                      ea = NULL,
                      era5_datetime = NULL,
                      esat = NULL,
                      surface_pressure = NULL,
                      u_component_of_wind_10m = NULL,
                      v_component_of_wind_10m = NULL,
                      wind_aspect_alignment_deg = NULL,
                      wind_dir_deg = NULL,
                      .geo = NULL)]

# convert some raw hourly values to percentiles
# percentile raster exported from Earth Engine for ERA5 weather data
# 2005 bands; 5 weather variables * 1001 percentiles
# temperature_2m, volumetric_soil_water_layer_1, vpd_hPa, rh, wind_speed
# 00000, 00025, 00050, 00075, 00100

# other percentiles that might be valuable
vars_era5 <- c("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")
pcts <- stringr::str_pad(string = as.character(seq(0, 100, by = 0.25) * 100),
                         width = 5, side = "left", pad = "0")

bandnames_era5 <- 
  expand.grid(pcts = pcts, vars = vars_era5) %>% 
  as.data.frame() %>% 
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

names(r_era5) <- bandnames_era5$bandname


temp <- r_era5[[1:401]]
vsw <- r_era5[[402:802]]
vpd <- r_era5[[803:1203]]
rh <- r_era5[[1204:1604]]
wind <- r_era5[[1605:2005]]



## get centroids of daily perimeters
centroids <-
  fired_events %>% 
  dplyr::select(id, geometry) %>%
  sf::st_make_valid() %>% 
  sf::st_centroid()

extract_percentile <- function(x) {
  idx <- which.min(x)
  if(length(idx) == 0) {
    percentile <- NA
  } else percentile <- as.numeric(pcts[idx]) / 100  
  
  return(percentile)
}

match_percentile <- function(r, var, drivers_DT) {
  
  percentile_matrix <- terra::extract(x = r, y = sf::st_coordinates(centroids), method = "simple")
  percentile_matrix <- cbind(id = centroids$id, percentile_matrix)
  percentile_matrix <- as.data.table(percentile_matrix)
  percentile_matrix <- percentile_matrix[drivers_DT[, c("id", ..var)], on = "id"][, id := NULL]
  percentile_matrix[, names(r) := lapply(names(r), FUN = function(i) {return(abs(get(i) - get(var)))})]
  data.table::set(x = percentile_matrix, j = var, value = NULL)
  percentile_matrix[, paste0(var, "_percentile") := apply(.SD, 1, extract_percentile)]
  
  data.table::set(x = percentile_matrix, j = names(r), value = NULL)
  
  return(percentile_matrix)
}

l <- list(temp, vsw, vpd, rh, wind)
var <- list("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")

hourly_pct_out <- pblapply(seq_along(l), FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], drivers_DT = hourly_drivers))
hourly_pct_out <- do.call(what = cbind, args = hourly_pct_out)
hourly_drivers <- cbind(hourly_drivers, hourly_pct_out)

hourly_drivers_summarized <-
  hourly_drivers %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  group_by(id, date, did) %>%
  summarize(wind_anisotropy = var(sin(wind_dir_rad)),
            wind_terrain_anisotropy = var(sin(wind_aspect_alignment_rad)),
            max_wind_speed = max(wind_speed),
            min_wind_speed = min(wind_speed),
            max_wind_speed_pct = max(wind_speed_percentile),
            min_wind_speed_pct = min(wind_speed_percentile),
            max_rh = max(rh),
            min_rh = min(rh),
            max_rh_pct = max(rh_percentile),
            min_rh_pct = min(rh_percentile),
            max_temp = max(temperature_2m - 273.15),
            min_temp = min(temperature_2m - 273.15),
            max_temp_pct = max(temperature_2m_percentile),
            min_temp_pct = min(temperature_2m_percentile),
            max_soil_water = max(volumetric_soil_water_layer_1),
            min_soil_water = min(volumetric_soil_water_layer_1),
            max_soil_water_pct = max(volumetric_soil_water_layer_1_percentile),
            min_soil_water_pct = min(volumetric_soil_water_layer_1_percentile),
            max_vpd = max(vpd_hPa),
            min_vpd = min(vpd_hPa),
            max_vpd_pct = max(vpd_hPa_percentile),
            min_vpd_pct = min(vpd_hPa_percentile)) %>%
  ungroup()
#### --- end hourly drivers prep


#### -- daily drivers prep

# GRIDMET Drought product doesn't seem to have the spi1y or spi270d layers for 2018 (checked on Earth Engine, too-- those layers are masked)
# so we want to be able to remove those columns so we can bind the whole dataset together
names_of_incomplete_vars <- names(daily_drivers_list[[1]])[which(!(names(daily_drivers_list[[1]]) %in% names(daily_drivers_list[[19]])))]

lapply(daily_drivers_list[-19], FUN = function(x) {
  data.table::set(x = x, j = names_of_incomplete_vars, value = NULL)
  invisible()
})

daily_drivers <- data.table::rbindlist(daily_drivers_list)
daily_drivers[, `:=`(.geo = NULL, `system:index` = NULL, pdsi_z = z, z = NULL)]

# get some of the human factors in tidy shape
# Took 48 minutes; would have taken ~3 hours with a simple for loop
# start_time <- Sys.time()
if(!file.exists("data/out/other-fires-summary.csv")) {
  
  other_fires <- function(DT, target_fire) {
    DT[, difftime_days := difftime(DT$date, target_fire$date, units = "days")]
    
    # number of concurrent fires for the target_fire (a fire/day combo) is the number of fires with
    # an ignition date before or equal to the day of interest and a last date after or
    # equal to the day of interest; AND that isn't the target fire itself
    concurrent_fires <- DT[id != target_fire$id & ig_date <= target_fire$date & last_date >= target_fire$date & difftime_days <= 0]
    if (nrow(concurrent_fires) == 0) {
      concurrent_fires <- 0
    } else {
      concurrent_fires <- nrow(concurrent_fires[, .SD[difftime_days == max(difftime_days, na.rm = TRUE)], , by = .(id)])
    }
    
    # number of events to date for the target_fire (a fire/day combo) is the events with
    # an ignition date in the same year but prior to the target day and with an id
    # that isn't the target fire's ID. We also subset to just fire/day combos where the difference in the 
    # target fire/day date and the other fires/days is less than 0 so we can filter and get the cumulative
    # area of the fires that meet these criteria 
    this_year_events_to_date <- DT[id != target_fire$id & ig_date < target_fire$date & ig_year == target_fire$ig_year & difftime_days < 0]
    this_year_events_to_date[, cumu_area_ha := c_area_tm1 + act_aoi]
    if (nrow(this_year_events_to_date) == 0) {
      cumu_count <- 0
      cumu_area_ha <- 0
    } else {
      this_year_events_to_date <- this_year_events_to_date[, .SD[difftime_days == max(difftime_days, na.rm = TRUE)], , by = .(id)]
      cumu_area_ha <- sum(this_year_events_to_date$cumu_area_ha)
      cumu_count <- nrow(this_year_events_to_date)
    }
    
    return(list(concurrent_fires, cumu_count, cumu_area_ha))
    
  }
  
  other_fires_summary <-
    fired_daily %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(did, id, date, ig_year, ig_date, last_date, c_area_tm1, act_aoi) %>% 
    as.data.table()

  other_fires_summary[, c("concurrent_fires", "cumu_count", "cumu_area_ha") := other_fires(DT = data.table::copy(other_fires_summary), target_fire = .SD), by = seq_len(NROW(other_fires_summary))]
  
  # join with the NPL date matching the ignition date to see what the NPL was on the day of ignition
  npl[, `:=`(ig_date = date, date = NULL)]
  other_fires_summary <- npl[other_fires_summary, on = "ig_date"]
  other_fires_summary[, `:=`(npl_at_ignition = npl, npl = NULL)]

  # join with the NPL date matching the actual date of the fire/day combo to see what the NPL was on that day
  npl[, `:=`(date = ig_date, ig_date = NULL)]
  other_fires_summary <- npl[other_fires_summary, on = "date"]
  other_fires_summary[, c("ig_date", "ig_year", "last_date", "c_area_tm1", "act_aoi") := NULL]
  other_fires_summary <- dplyr::select(other_fires_summary, did, id, date, npl, npl_at_ignition, everything())
  
  write.csv(x = other_fires_summary, file = "data/out/other-fires-summary.csv", row.names = FALSE)
}

other_fires_summary <- 
  read.csv(file = "data/out/other-fires-summary.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  as_tibble()

# end_time <- Sys.time()
# print(difftime(end_time, start_time, units = "mins"))

# convert some raw daily values to percentiles

# From GRIDMET: bi, erc, fm100, fm1000
vars_gridmet <- c("bi", "erc", "fm100", "fm1000")

bandnames_gridmet <-
  expand.grid(pcts = pcts, vars = vars_gridmet) %>%
  as.data.frame() %>%
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

names(r_gridmet) <- bandnames_gridmet$bandname

bi <- r_gridmet[[1:401]]
erc <- r_gridmet[[402:802]]
fm100 <- r_gridmet[[803:1203]]
fm1000 <- r_gridmet[[1204:1604]]

l <- list(bi, erc, fm100, fm1000)
var <- list("bi", "erc", "fm100", "fm1000")

daily_pct_out <- pblapply(seq_along(l), FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], drivers_DT = daily_drivers))
daily_pct_out <- do.call(what = cbind, args = daily_pct_out)
daily_drivers <- 
  cbind(daily_drivers, daily_pct_out) %>% 
  as_tibble()

# Convert coverage of different landcovers to actual area
# Each data type needs a different multiplier to reflect the scale at which the pixel count was done in Earth Engine
# csp_ergo_landforms: scale was 10.2m (based on NED DEM) so multiply the pixel count by 10.2*10.2=104.04
# lcms (including lcms_change, lcms_landcover, lcms_landuse): scale was 30m (based on Landsat) so multiply the pixel count by 30*30=900 m^2
# We also calculate the proportion of the total area within each category

daily_drivers_out <-
  daily_drivers %>% 
  dplyr::mutate(surf_area_ha = surf_area / 10000,
                proj_area_ha = proj_area / 10000,
                road_density_mpha = ((road_length_m) / (proj_area_ha)),
                date = lubridate::ymd(date)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10.2*10.2/10000)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30/10000)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / proj_area_ha, .names = "{.col}_prop")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / proj_area_ha, .names = "{.col}_prop")) %>% 
  dplyr::select(-gridmet_datetime, -gridmet_drought_datetime, -surf_area, -proj_area, -megafire) %>% 
  dplyr::select(did, id, date, everything())

#### --- end daily drivers prep

#### --- join daily drivers and hourly drivers summarized to daily time steps

other_fires_summary
hourly_drivers_summarized
daily_drivers_out

out_sf <- 
  fired_daily %>% 
  dplyr::left_join(other_fires_summary) %>% 
  dplyr::left_join(hourly_drivers_summarized) %>% 
  dplyr::left_join(daily_drivers_out)

out <- 
  out_sf %>% 
  sf::st_drop_geometry() %>% 
  as.data.table()

sf::st_write(obj = out_sf, dsn = "data/out/analysis-ready/FIRED-daily-scale-drivers_california.gpkg")
data.table::fwrite(x = out, file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california.csv")