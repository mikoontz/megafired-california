# convert absolute weather variable values to percentile

library(dplyr)
library(terra)
library(stringr)
library(data.table)
library(exactextractr)
library(sf)
library(pbapply)

# percentile raster exported from Earth Engine for ERA5 weather data
# 2005 bands; 5 weather variables * 1001 percentiles
# temperature_2m, volumetric_soil_water_layer_1, vpd_hPa, rh, wind_speed
# 00000, 00025, 00050, 00075, 00100

# other percentiles that might be valuable
# From GRIDMET: bi, erc, fm100, fm1000
vars <- c("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")
pcts <- stringr::str_pad(string = as.character(seq(0, 100, by = 0.25) * 100),
                         width = 5, side = "left", pad = "0")

bandnames <- 
  expand.grid(pcts = pcts, vars = vars) %>% 
  as.data.frame() %>% 
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

r <- 
  terra::rast("data/out/ee/era5-weather-percentiles_1981-01-01_2020-12-31.tif") %>% 
  setNames(bandnames$bandname)

temp <- r[[1:401]]
vsw <- r[[402:802]]
vpd <- r[[803:1203]]
rh <- r[[1204:1604]]
wind <- r[[1605:2005]]

## Summarize event-scale, daily, and hourly drivers

## get centroids of daily perimeters
perims <- 
  sf::st_read("data/out/fired_daily_ca_ewe_rank.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::select(did, geometry)

centroids <- sf::st_centroid(perims)

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

hourly_drivers_perims <-
  hourly_drivers %>% 
  dplyr::select(c("did", "temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")) %>% 
  dplyr::left_join(perims, by = "did") %>% 
  sf::st_as_sf()

hourly_drivers_centroids <-
  hourly_drivers %>% 
  dplyr::select(c("did", "temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")) %>% 
  dplyr::left_join(centroids, by = "did") %>% 
  sf::st_as_sf()


match_percentile <- function(r, var, label) {
  # percentile_matrix_polys <- exactextractr::exact_extract(x = temp, y = hourly_drivers_perims)
  percentile_matrix <- 
    terra::extract(x = r, y = sf::st_coordinates(hourly_drivers_centroids), method = "bilinear") %>% 
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


l <- list(temp, vsw, vpd, rh, wind)
var <- list("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")
label <- list("temp_percentile", "soil_water_percentile", "vpd_percentile", "rh_percentile", "wind_speed_percentile")

pblapply(1:5, FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], label = label[[i]]))

hourly_drivers
