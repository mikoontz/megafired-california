# convert absolute weather variable values to percentile

library(dplyr)
library(terra)
library(stringr)
library(data.table)
library(exactextractr)
library(sf)
library(pbapply)
library(broom)
library(pbapply)
library(lubridate)

dir.create("data/out/analysis-ready", showWarnings = FALSE)

npl <- 
  read.csv("data/out/national-preparedness-level.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste0(year, "-", month, "-", day)))

if(!file.exists("data/out/human-drivers-summary.csv")) {
  events <- 
    sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>% 
    sf::st_make_valid() %>% 
    dplyr::filter(ig_year <= 2020)
  
  out <- vector(mode = "list", length = nrow(events))
  
  for (i in 1:nrow(events)) {
    this_event <- events[i, ]
    concurrent_fires <- sum(events$ig_date >= this_event$ig_date & events$ig_date <= this_event$last_date)
    
    this_year_events_todate <-
      events %>% 
      filter(ig_year == this_event$ig_year & 
               id != this_event$id &
               ig_date < this_event$ig_date)
    
    starting_npl <- npl[npl$date == this_event$ig_date, "npl"]
    npl[npl$date == this_event$ig_date, "npl"]
    
    mean_npl <- 
      npl %>% 
      dplyr::filter(date >= this_event$ig_date & date <= this_event$last_date) %>% 
      dplyr::summarize(npl = mean(npl)) %>% 
      as.numeric()
    
    out[[i]] <- data.frame(id = this_event$id, 
                           concurrent = concurrent_fires,
                           cum_count = nrow(this_year_events_todate),
                           cum_area = as.numeric(sum(sf::st_area(this_year_events_todate))),
                           starting_npl = starting_npl,
                           mean_npl = mean_npl)
    
  }
  
  human_factors <- data.table::rbindlist(out)
  
  write.csv(x = human_factors, file = "data/out/human-drivers-summary.csv", row.names = FALSE)
}

human_factors <- read.csv("data/out/human-drivers-summary.csv")

ewe_ranks <- read.csv("data/out/extreme-wildfire-events-ranking.csv")

human_factors_mega <- 
  human_factors %>% 
  dplyr::rename(driver_starting_npl = starting_npl,
                driver_mean_npl = mean_npl,
                driver_concurrent_fire_count = concurrent,
                driver_cumulative_fire_count = cum_count,
                driver_cumulative_fire_area = cum_area) %>% 
  dplyr::mutate(driver_cumulative_fire_area = driver_cumulative_fire_area / 1000000)

event_drivers <- data.table::fread("data/out/ee/FIRED-event-scale-drivers_california.csv")
hourly_drivers <- data.table::fread("data/out/ee/FIRED-hourly-drivers_california.csv")

daily_drivers_list <- 
  list.files("data/out/ee/", 
             pattern = "FIRED-daily-drivers_california_", 
             full.names = TRUE) %>% 
  lapply(FUN = fread)

names_of_incomplete_vars <- names(daily_drivers_list[[1]])[which(!(names(daily_drivers_list[[1]]) %in% names(daily_drivers_list[[19]])))]

lapply(daily_drivers_list[-19], FUN = function(x) {
  data.table::set(x = x, j = names_of_incomplete_vars, value = NULL)
})

daily_drivers <- data.table::rbindlist(daily_drivers_list)

event_drivers[, `:=`(.geo = NULL,
                     `system:index` = NULL)]
event_drivers <-
  event_drivers %>% 
  dplyr::rename(driver_friction = friction)

daily_drivers[, .geo := NULL]
hourly_drivers[, .geo := NULL]
hourly_drivers[, rh := ea / esat]

# percentile raster exported from Earth Engine for ERA5 weather data
# 2005 bands; 5 weather variables * 1001 percentiles
# temperature_2m, volumetric_soil_water_layer_1, vpd_hPa, rh, wind_speed
# 00000, 00025, 00050, 00075, 00100

# other percentiles that might be valuable
# From GRIDMET: bi, erc, fm100, fm1000
vars_era5 <- c("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")
vars_gridmet <- c("bi", "erc", "fm100", "fm1000")
pcts <- stringr::str_pad(string = as.character(seq(0, 100, by = 0.25) * 100),
                         width = 5, side = "left", pad = "0")

bandnames_era5 <- 
  expand.grid(pcts = pcts, vars = vars_era5) %>% 
  as.data.frame() %>% 
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

r_era5 <- 
  terra::rast("data/out/era5-weather-percentiles_1981-01-01_2020-12-31.tif") %>% 
  setNames(bandnames_era5$bandname)

bandnames_gridmet <- 
  expand.grid(pcts = pcts, vars = vars_gridmet) %>% 
  as.data.frame() %>% 
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

r_gridmet <-
  terra::rast("data/out/gridmet-weather-percentiles_1981-01-01_2020-12-31.tif") %>% 
  setNames(bandnames_gridmet$bandname)

temp <- r_era5[[1:401]]
vsw <- r_era5[[402:802]]
vpd <- r_era5[[803:1203]]
rh <- r_era5[[1204:1604]]
wind <- r_era5[[1605:2005]]
bi <- r_gridmet[[1:401]]
erc <- r_gridmet[[402:802]]
fm100 <- r_gridmet[[803:1203]]
fm1000 <- r_gridmet[[1204:1604]]

## Summarize event-scale, daily, and hourly drivers

## get centroids of daily perimeters
perims <- 
  sf::st_read("data/out/fired_daily_ca_ewe_rank.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::select(did, geometry)

centroids <- sf::st_centroid(perims)

# hourly_drivers_perims <-
#   hourly_drivers %>% 
#   dplyr::select(c("did", "temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")) %>% 
#   dplyr::left_join(perims, by = "did") %>% 
#   sf::st_as_sf()

hourly_drivers_centroids <-
  hourly_drivers %>% 
  dplyr::select(c("system:index", "did", "temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")) %>% 
  dplyr::left_join(centroids, by = "did") %>% 
  sf::st_as_sf()

daily_drivers_centroids <-
  daily_drivers %>% 
  dplyr::select(c("system:index", "did", "bi", "erc", "fm100", "fm1000")) %>% 
  dplyr::left_join(centroids, by = "did") %>% 
  sf::st_as_sf()

match_percentile_hourly <- function(r, var, label) {
  # percentile_matrix_polys <- exactextractr::exact_extract(x = temp, y = hourly_drivers_perims)
  percentile_matrix <- 
    terra::extract(x = r, y = sf::st_coordinates(hourly_drivers_centroids), method = "simple") %>% 
    as.matrix()
  
  diffs_matrix <- abs(percentile_matrix - as.data.frame(hourly_drivers)[, var])
  
  percentiles_vec <- apply(diffs_matrix, MARGIN = 1, FUN = function(x) {
    idx <- which.min(x)
    if(length(idx) == 0) {
      percentile <- NA
    } else percentile <- as.numeric(pcts[idx]) / 100  
    
    return(percentile)
  })
  
  data.table::set(x = hourly_drivers, j = label, value = percentiles_vec)
  
}

match_percentile_daily <- function(r, var, label) {
  # percentile_matrix_polys <- exactextractr::exact_extract(x = temp, y = hourly_drivers_perims)
  percentile_matrix <- 
    terra::extract(x = r, y = sf::st_coordinates(daily_drivers_centroids), method = "simple") %>% 
    as.matrix()
  
  diffs_matrix <- abs(percentile_matrix - as.data.frame(daily_drivers)[, var])
  
  percentiles_vec <- apply(diffs_matrix, MARGIN = 1, FUN = function(x) {
    idx <- which.min(x)
    if(length(idx) == 0) {
      percentile <- NA
    } else percentile <- as.numeric(pcts[idx]) / 100  
    
    return(percentile)
  })
  
  data.table::set(x = daily_drivers, j = label, value = percentiles_vec)
  
}

l <- list(temp, vsw, vpd, rh, wind)
var <- list("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")
label <- list("temp_percentile", "soil_water_percentile", "vpd_percentile", "rh_percentile", "wind_speed_percentile")

pblapply(1:5, FUN = function(i) match_percentile_hourly(r = l[[i]], var = var[[i]], label = label[[i]]))

l <- list(bi, erc, fm100, fm1000)
var <- list("bi", "erc", "fm100", "fm1000")
label <- list("bi_percentile", "erc_percentile", "fm100_percentile", "fm1000_percentile")

pblapply(1:4, FUN = function(i) match_percentile_daily(r = l[[i]], var = var[[i]], label = label[[i]]))

data.table::fwrite(x = event_drivers, file = "data/out/analysis-ready/FIRED-event-scale-drivers_california.csv")
data.table::fwrite(x = hourly_drivers, file = "data/out/analysis-ready/FIRED-hourly-scale-drivers_california.csv")
data.table::fwrite(x = daily_drivers, file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california.csv")
