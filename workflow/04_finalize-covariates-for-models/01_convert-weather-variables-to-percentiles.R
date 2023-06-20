# Normalize weather variables into percentile scale

library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(pbapply)
library(USAboundaries)

fired_daily <- 
  sf::st_read("data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  sf::st_set_agr(value = "constant")

fired_daily_biggest_poly_centroids <-
  data.table::fread("data/out/fired/03_joined-with-other-data/fired-biggest-poly-info.csv")

# Some important notes on the wind variables derived from ERA5 and RTMA
# wind_aspect_alignment_rad = wind direction (radians) - slope aspect (radians); wind blowing uphill = 0, wind blowing downhill = pi, wind blowing across slope is pi/2 or 3pi/2
# wind terrain alignment = abs(cos(wind_aspect_alignment_rad)); wind blowing uphill and wind blowing downhill get max value of 1; wind blowing across slope gets value of 0
# Daily wind terrain alignment = mean(wind_terrain_alignment)
# daily wind anisotropy = sd(cos(wind direction (radians))); 
# daily wind terrain anisotropy = sd(wind terrain alignment)

# https://stats.stackexchange.com/questions/45588/variance-of-a-bounded-random-variable; If X [m, M], then var(X) <= (M - m)^2 / 4, then sd(X) <= (M - m) / 2
# Therefore, maximum standard deviation of variable bounded by [0, 1] is 0.5
# Therefore, maximum standard deviation of variable bounded by [-1, 1] is 1

# Wind terrain alignment is bound by [0, 1] because of the abs(cos(X)) function applied
# wind anisotropy is bound by [0, 1] because standard deviation has to be greater than 0, and the internal cos(X) function is bound by [-1, 1]
# wind terrain alignment anisotropy is bound by [0, 0.5] because standard deviation is greater than 0, and max standard deviation of variable bounded by [0, 1] is 0.5
# Above, we multiplied terrain alignment anisotropy by 2 to make it bound by [0,1] like these other variables

### Machinery to convert some raw values of weather variables to percentiles
### through time by using a many-layered raster of the values of the weather
### variables of interest that correspond to a fine mesh of percentiles
### (0 to 100 in increments of 0.25)
extract_percentile <- function(x) {
  idx <- which.min(x)
  if(length(idx) == 0) {
    percentile <- NA
  } else percentile <- as.numeric(pcts[idx]) / 10000  
  
  return(percentile)
}

match_percentile <- function(r, var, drivers_DT) {
  
  percentile_matrix <- terra::extract(x = r, y = as.matrix(fired_daily_biggest_poly_centroids[, c("x_biggest_poly_3310", "y_biggest_poly_3310")]), method = "simple")
  percentile_matrix <- cbind(did = fired_daily_biggest_poly_centroids$did, percentile_matrix)
  percentile_matrix <- as.data.table(percentile_matrix)
  percentile_matrix <- percentile_matrix[drivers_DT[, c("did", ..var)], on = "did"][, did := NULL]
  percentile_matrix[, names(r) := lapply(names(r), FUN = function(i) {return(abs(get(i) - get(var)))})]
  data.table::set(x = percentile_matrix, j = var, value = NULL)
  percentile_matrix[, paste0(var, "_pct") := apply(.SD, 1, extract_percentile)]
  
  data.table::set(x = percentile_matrix, j = names(r), value = NULL)
  
  return(percentile_matrix)
}

r_era5 <- 
  terra::rast("data/out/ee/era5-weather-percentiles_1981-01-01_2020-12-31.tif")

r_gridmet <-
  terra::rast("data/out/ee/gridmet-weather-percentiles_1981-01-01_2020-12-31.tif")

r_rtma <-
  terra::rast("data/out/ee/rtma-weather-percentiles_2011-01-01_2020-12-31.tif")

pcts <- stringr::str_pad(string = as.character(seq(0, 100, by = 0.25) * 100),
                         width = 5, side = "left", pad = "0")

#### --- era5 drivers prep
era5_drivers <- data.table::fread("data/out/ee/FIRED-era5-drivers_california_biggest-poly.csv")
era5_drivers <- era5_drivers[did %in% fired_daily$did, ]
era5_drivers[, `:=`(`system:index` = NULL,
                    ea = NULL,
                    era5_datetime = NULL,
                    esat = NULL,
                    surface_pressure = NULL,
                    u_component_of_wind_10m = NULL,
                    v_component_of_wind_10m = NULL,
                    wind_dir_deg = NULL,
                    .geo = NULL)]

# convert some raw era5 values to percentiles
# percentile raster exported from Earth Engine for ERA5 weather data
# 2005 bands; 5 weather variables * 1001 percentiles
# temperature_2m, volumetric_soil_water_layer_1, vpd_hPa, rh, wind_speed
# 00000, 00025, 00050, 00075, 00100

# other percentiles that might be valuable
vars_era5 <- c("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")

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


l <- list(temp, vsw, vpd, rh, wind)
var <- list("temperature_2m", "volumetric_soil_water_layer_1", "vpd_hPa", "rh", "wind_speed")

era5_pct_out <- pblapply(seq_along(l), FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], drivers_DT = era5_drivers))
era5_pct_out <- do.call(what = cbind, args = era5_pct_out)
era5_drivers <- cbind(era5_drivers, era5_pct_out)

era5_drivers_summarized <-
  era5_drivers %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  group_by(id, date, did) %>%
  summarize(wind_dir_ns_era5 = mean(cos(wind_dir_rad)), # average northness of wind direction in a day (1 is all north winds; -1 is all south winds)
            wind_dir_ew_era5 = mean(sin(wind_dir_rad)), # average eastness of wind direction in a day (1 is all east winds; -1 is all west winds)
            wind_anisotropy_ns_era5 = sd(cos(wind_dir_rad)), # greater standard deviation means MORE north/south variability in wind direction in a day
            wind_anisotropy_ew_era5 = sd(sin(wind_dir_rad)), # greater standard deviation means MORE east/west variability in wind direction in a day
            # # multiply wind terrain anisotropy by 2 to put it on the same [0,1] scale as wind anisotropy and wind terrain alignment
            # # instead of [0,0.5]
            # wind_terrain_anisotropy_ns_era5 = 2*sd(abs(cos(wind_aspect_alignment_rad))), # greater standard deviation means MORE north/south variability in wind/terrain alignment in a day
            # wind_terrain_anisotropy_ew_era5 = 2*sd(abs(sin(wind_aspect_alignment_rad))), # greater standard deviation means MORE east/west variability in wind/terrain alignment in a day
            # wind_terrain_alignment_era5 = mean(abs(cos(wind_aspect_alignment_rad))), # cos() such that exact alignment (wind blowing into uphill slope) gets a 1, 180 degrees off gets a -1 (wind blowing into downhill slope); take the absolute value such that either blowing into uphill or downhill slope gets maximum alignment value 
            # min_wind_terrain_alignment_era5 = min(abs(cos(wind_aspect_alignment_rad))),
            # max_wind_terrain_alignment_era5 = max(abs(cos(wind_aspect_alignment_rad))),
            max_wind_speed_era5 = max(wind_speed),
            min_wind_speed_era5 = min(wind_speed),
            max_wind_speed_era5_pct = max(wind_speed_pct),
            min_wind_speed_era5_pct = min(wind_speed_pct),
            max_rh_era5 = max(rh),
            min_rh_era5 = min(rh),
            max_rh_era5_pct = max(rh_pct),
            min_rh_era5_pct = min(rh_pct),
            max_temp_era5 = max(temperature_2m - 273.15),
            min_temp_era5 = min(temperature_2m - 273.15),
            max_temp_era5_pct = max(temperature_2m_pct),
            min_temp_era5_pct = min(temperature_2m_pct),
            max_soil_water_era5 = max(volumetric_soil_water_layer_1),
            min_soil_water_era5 = min(volumetric_soil_water_layer_1),
            max_soil_water_era5_pct = max(volumetric_soil_water_layer_1_pct),
            min_soil_water_era5_pct = min(volumetric_soil_water_layer_1_pct),
            max_vpd_era5 = max(vpd_hPa),
            min_vpd_era5 = min(vpd_hPa),
            max_vpd_era5_pct = max(vpd_hPa_pct),
            min_vpd_era5_pct = min(vpd_hPa_pct)) %>%
  ungroup()
#### --- end era5 drivers prep

#### --- begin RTMA drivers prep (only for fires from 2011 to 2020)
rtma_drivers <- data.table::fread("data/out/ee/FIRED-rtma-drivers_california_biggest-poly.csv")
rtma_drivers <- rtma_drivers[did %in% fired_daily$did, ]
rtma_drivers[, `:=`(`system:index` = NULL,
                    ea = NULL,
                    rtma_datetime = NULL,
                    esat = NULL,
                    PRES = NULL,
                    UGRD = NULL,
                    VGRD = NULL,
                    wind_aspect_alignment_deg = NULL,
                    WDIR = NULL,
                    .geo = NULL)]

vars_rtma <- c('TMP', 'vpd_hPa', 'rh', 'WIND', 'GUST', 'wind_filled_gust', 'wind_aspect_alignment_rad')

bandnames_rtma <- 
  expand.grid(pcts = pcts, vars = vars_rtma) %>% 
  as.data.frame() %>% 
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

names(r_rtma) <- bandnames_rtma$bandname

temp_rtma <- r_rtma[[1:401]]
vpd_rtma <- r_rtma[[402:802]]
rh_rtma <- r_rtma[[803:1203]]
wind_rtma <- r_rtma[[1204:1604]]
gust_rtma <- r_rtma[[1605:2005]]
wind_filled_gust_rtma <- r_rtma[[2006:2406]]
wind_terrain_alignment_rtma <- r_rtma[[2407:2807]]

l <- list(temp_rtma, vpd_rtma, rh_rtma, wind_rtma, gust_rtma, wind_filled_gust_rtma, wind_terrain_alignment_rtma)
var <- list('TMP', 'vpd_hPa', 'rh', 'WIND', 'GUST', 'wind_filled_gust', 'wind_aspect_alignment_rad')

rtma_pct_out <- pblapply(seq_along(l), FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], drivers_DT = rtma_drivers))
rtma_pct_out <- do.call(what = cbind, args = rtma_pct_out)
rtma_drivers <- cbind(rtma_drivers, rtma_pct_out)

rtma_drivers_summarized <-
  rtma_drivers %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  group_by(id, date, did) %>%
  summarize(wind_dir_ns_rtma = mean(cos(WDIR_rad)), # average northness of the wind direction in a day (1 is 24 hours of north winds; -1 is 24 hours of south winds)
            wind_dir_ew_rtma = mean(sin(WDIR_rad)), # average eastness of the wind direction in a day (1 is 24 hours of north winds; -1 is 24 hours of south winds)
            wind_anisotropy_ns_rtma = sd(cos(WDIR_rad)), # greater standard deviation means MORE north/south variability in wind direction in a day
            wind_anisotropy_ew_rtma = sd(sin(WDIR_rad)), # greater standard deviation means MORE east/west variability in wind direction in a day
            # # multiply wind terrain anisotropy by 2 to put it on the same [0,1] scale as wind anisotropy and wind terrain alignment
            # # instead of [0,0.5]
            # wind_terrain_anisotropy_ns_rtma = 2*sd(abs(cos(wind_aspect_alignment_rad))), # greater standard deviation means MORE north/south asymmetry in wind/terrain alignment in a day
            # wind_terrain_anisotropy_ew_rtma = 2*sd(abs(sin(wind_aspect_alignment_rad))), # greater standard deviation means MORE east/west asymmetry in wind/terrain alignment in a day
            # wind_terrain_alignment_rtma = mean(abs(cos(wind_aspect_alignment_rad))), # cos() such that exact alignment (wind blowing into uphill slope) gets a 1, 180 degrees off gets a -1 (wind blowing into downhill slope); take the absolute value such that either blowing into uphill or downhill slope gets maximum alignment value 
            # min_wind_terrain_alignment_rtma = min(abs(cos(wind_aspect_alignment_rad))),
            # max_wind_terrain_alignment_rtma = max(abs(cos(wind_aspect_alignment_rad))),
            # min_wind_terrain_alignment_rtma_pct = min(wind_aspect_alignment_rad_pct),
            # max_wind_terrain_alignment_rtma_pct = max(wind_aspect_alignment_rad_pct),
            max_wind_speed_rtma = max(WIND),
            min_wind_speed_rtma = min(WIND),
            max_wind_gust_rtma = max(GUST),
            min_wind_gust_rtma = min(GUST),
            max_wind_filled_gust_rtma = max(wind_filled_gust),
            min_wind_filled_gust_rtma = min(wind_filled_gust),
            max_wind_speed_rtma_pct = max(WIND_pct),
            min_wind_speed_rtma_pct = min(WIND_pct),
            max_wind_gust_rtma_pct = max(GUST_pct),
            min_wind_gust_rtma_pct = min(GUST_pct),
            max_wind_filled_gust_rtma_pct = max(wind_filled_gust_pct),
            min_wind_filled_gust_rtma_pct = min(wind_filled_gust_pct),
            max_rh_rtma = max(rh),
            min_rh_rtma = min(rh),
            max_rh_rtma_pct = max(rh_pct),
            min_rh_rtma_pct = min(rh_pct),
            max_temp_rtma = max(TMP),
            min_temp_rtma = min(TMP),
            max_temp_rtma_pct = max(TMP_pct),
            min_temp_rtma_pct = min(TMP_pct),
            max_vpd_rtma = max(vpd_hPa),
            min_vpd_rtma = min(vpd_hPa),
            max_vpd_rtma_pct = max(vpd_hPa_pct),
            min_vpd_rtma_pct = min(vpd_hPa_pct)) %>%
  ungroup()

#### --- end RTMA drivers prep


#### -- daily drivers prep
# GRIDMET drivers

gridmet_drivers <- data.table::fread("data/out/ee/FIRED-daily-gridmet-drivers_california_biggest-poly.csv")
gridmet_drivers <- gridmet_drivers[did %in% fired_daily$did, ]
gridmet_drivers[, `:=`(.geo = NULL, samp_id = NULL,
                       `system:index` = NULL, 
                       pdsi_z = z, z = NULL, 
                       gridmet_datetime = NULL, gridmet_drought_datetime = NULL)]

# convert some raw daily values to percentiles

# From GRIDMET: erc, bi, fm100, fm1000
vars_gridmet <- c("erc", "bi", "fm100", "fm1000")

bandnames_gridmet <-
  expand.grid(pcts = pcts, vars = vars_gridmet) %>%
  as.data.frame() %>%
  dplyr::mutate(bandname = paste0(vars, "_p_", pcts))

names(r_gridmet) <- bandnames_gridmet$bandname

erc <- r_gridmet[[1:401]]
bi <- r_gridmet[[402:802]]
fm100 <- r_gridmet[[803:1203]]
fm1000 <- r_gridmet[[1204:1604]]

l <- list(erc, bi, fm100, fm1000)
var <- list("erc", "bi", "fm100", "fm1000")

gridmet_pct_out <- pblapply(seq_along(l), FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], drivers_DT = gridmet_drivers))
gridmet_pct_out <- do.call(what = cbind, args = gridmet_pct_out)

gridmet_drivers <- 
  cbind(gridmet_drivers, gridmet_pct_out) %>% 
  dplyr::mutate(date = lubridate::ymd(date))

summary(gridmet_drivers)

## Reality check with known fires
frap <- read.csv("data/out/fired/03_joined-with-other-data/fired-frap-mtbs-join.csv")
frap %>% filter(name_frap == "TUBBS")
creek <- rtma_drivers_summarized[rtma_drivers_summarized$id == 135921, ]
tubbs <- rtma_drivers_summarized[rtma_drivers_summarized$id == 117324, ]

creek$max_wind_gust_rtma_pct
creek$min_wind_speed_rtma_pct
creek$max_vpd_rtma_pct

tubbs$max_wind_gust_rtma_pct
tubbs$min_wind_gust_rtma_pct
tubbs$min_wind_speed_rtma_pct
tubbs$wind_anisotropy_ns_rtma
tubbs$wind_anisotropy_ew_rtma
tubbs$wind_dir_ns_rtma
tubbs$wind_dir_ew_rtma

tubbs$max_vpd_rtma_pct

###

out_sf <- 
  fired_daily %>% 
  dplyr::left_join(era5_drivers_summarized, by = c("did", "id", "date")) %>% 
  dplyr::left_join(rtma_drivers_summarized, by = c("did", "id", "date")) %>% 
  dplyr::left_join(gridmet_drivers, by = c("did", "id", "date"))

out <- 
  out_sf %>% 
  sf::st_drop_geometry() %>% 
  as.data.table()

data.table::fwrite(x = out, file = "data/out/drivers/weather-drivers-as-percentiles.csv")
