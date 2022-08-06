# assemble daily-scale drivers

library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(pbapply)
library(USAboundaries)

static_version <- "v4"
fluc_version <- "v5"
driver_version <- "v8"

#### --- input FIRED data
fired_events <- 
  sf::st_read("data/out/fired_events_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::rename(geometry = geom)

fired_daily <- 
  sf::st_read("data/out/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(date = lubridate::ymd(date),
                area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  sf::st_set_agr(value = "constant")

fired_daily_centroids <-
  fired_daily %>% 
  sf::st_centroid() %>% 
  dplyr::mutate(x_3310 = sf::st_coordinates(.)[, "X"],
                y_3310 = sf::st_coordinates(.)[, "Y"])

fired_biggest_poly <-
  sf::st_read("data/out/fired_daily_ca_epsg3310_2003-2020_biggest-poly") %>% 
  dplyr::select(-subgeo_id, -samp_id) %>% 
  dplyr::mutate(date = lubridate::ymd(date),
                biggest_poly_area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  sf::st_set_agr(value = "constant")

fired_daily_biggest_poly_centroids <-
  fired_biggest_poly %>% 
  dplyr::filter(did %in% fired_daily$did) %>% 
  sf::st_centroid() %>% 
  dplyr::mutate(x_biggest_poly_3310 = sf::st_coordinates(.)[, "X"],
                y_biggest_poly_3310 = sf::st_coordinates(.)[, "Y"]) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_drop_geometry() %>%
  dplyr::left_join(sf::st_drop_geometry(fired_daily_centroids)) %>% 
  dplyr::mutate(biggest_poly_frac = biggest_poly_area_ha / area_ha)

target_fires_did <- fired_daily$did
## 

### Machinery to convert some raw values of weather variables to percentiles
### through time by using a many-layered raster of the values of the weather
### variables of interest that correspond to a fine mesh of percentiles
### (0 to 100 in increments of 0.25)
extract_percentile <- function(x) {
  idx <- which.min(x)
  if(length(idx) == 0) {
    percentile <- NA
  } else percentile <- as.numeric(pcts[idx]) / 100  
  
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
era5_drivers <- era5_drivers[did %in% target_fires_did, ]
era5_drivers[, `:=`(`system:index` = NULL,
                    ea = NULL,
                    era5_datetime = NULL,
                    esat = NULL,
                    surface_pressure = NULL,
                    u_component_of_wind_10m = NULL,
                    v_component_of_wind_10m = NULL,
                    wind_aspect_alignment_deg = NULL,
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
  summarize(wind_anisotropy = sd(cos(wind_dir_rad)), # greater standard deviation means MORE asymmetry in wind direction in a day
            wind_terrain_anisotropy = sd(abs(cos(wind_aspect_alignment_rad))), # greater standard deviation means MORE asymmetry in wind/terrain alignment in a day
            wind_terrain_alignment = mean(abs(cos(wind_aspect_alignment_rad))), # cos() such that exact alignment (wind blowing into uphill slope) gets a 1, 180 degrees off gets a -1 (wind blowing into downhill slope); take the absolute value such that either blowing into uphill or downhill slope gets maximum alignment value 
            max_wind_speed = max(wind_speed),
            min_wind_speed = min(wind_speed),
            max_wind_speed_pct = max(wind_speed_pct),
            min_wind_speed_pct = min(wind_speed_pct),
            max_rh = max(rh),
            min_rh = min(rh),
            max_rh_pct = max(rh_pct),
            min_rh_pct = min(rh_pct),
            max_temp = max(temperature_2m - 273.15),
            min_temp = min(temperature_2m - 273.15),
            max_temp_pct = max(temperature_2m_pct),
            min_temp_pct = min(temperature_2m_pct),
            max_soil_water = max(volumetric_soil_water_layer_1),
            min_soil_water = min(volumetric_soil_water_layer_1),
            max_soil_water_pct = max(volumetric_soil_water_layer_1_pct),
            min_soil_water_pct = min(volumetric_soil_water_layer_1_pct),
            max_vpd = max(vpd_hPa),
            min_vpd = min(vpd_hPa),
            max_vpd_pct = max(vpd_hPa_pct),
            min_vpd_pct = min(vpd_hPa_pct)) %>%
  ungroup()
#### --- end era5 drivers prep

#### --- begin RTMA drivers prep (only for fires from 2011 to 2020)
rtma_drivers <- data.table::fread("data/out/ee/FIRED-rtma-drivers_california_biggest-poly.csv")
rtma_drivers <- rtma_drivers[did %in% target_fires_did, ]
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

vars_rtma <- c('TMP', 'vpd_hPa', 'rh', 'WIND', 'GUST')

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


l <- list(temp_rtma, vpd_rtma, rh_rtma, wind_rtma, gust_rtma)
var <- list('TMP', 'vpd_hPa', 'rh', 'WIND', 'GUST')

rtma_pct_out <- pblapply(seq_along(l), FUN = function(i) match_percentile(r = l[[i]], var = var[[i]], drivers_DT = rtma_drivers))
rtma_pct_out <- do.call(what = cbind, args = rtma_pct_out)
rtma_drivers <- cbind(rtma_drivers, rtma_pct_out)

rtma_drivers_summarized <-
  rtma_drivers %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  group_by(id, date, did) %>%
  summarize(wind_anisotropy_rtma = sd(cos(WDIR_rad)), # greater standard deviation means MORE asymmetry in wind direction in a day
            wind_terrain_anisotropy_rtma = sd(abs(cos(wind_aspect_alignment_rad))), # greater standard deviation means MORE asymmetry in wind/terrain alignment in a day
            wind_terrain_alignment_rtma = mean(abs(cos(wind_aspect_alignment_rad))), # cos() such that exact alignment (wind blowing into uphill slope) gets a 1, 180 degrees off gets a -1 (wind blowing into downhill slope); take the absolute value such that either blowing into uphill or downhill slope gets maximum alignment value 
            max_wind_speed_rtma = max(WIND),
            min_wind_speed_rtma = min(WIND),
            max_wind_gust_rtma = max(GUST),
            min_wind_gust_rtma = min(GUST),
            max_wind_speed_rtma_pct = max(WIND_pct),
            min_wind_speed_rtma_pct = min(WIND_pct),
            max_wind_gust_rtma_pct = max(GUST_pct),
            min_wind_gust_rtma_pct = min(GUST_pct),
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

frap <- read.csv("data/out/fired-frap-mtbs-join.csv")
creek <- rtma_drivers_summarized[rtma_drivers_summarized$id == 135921, ]
creek$max_wind_gust_rtma_pct
creek$min_wind_speed_rtma_pct
creek$max_vpd_rtma_pct

#### --- end RTMA drivers prep

#### -- daily drivers prep
# GRIDMET drivers

gridmet_drivers <- data.table::fread("data/out/ee/FIRED-daily-gridmet-drivers_california_biggest-poly.csv")
gridmet_drivers <- gridmet_drivers[did %in% target_fires_did, ]
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

### End prep for weather variables

npl <- 
  read.csv("data/out/national-preparedness-level.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste0(year, "-", month, "-", day))) %>% 
  dplyr::select(-c(year, month, day)) %>% 
  as.data.table()

### landform and landcover variables
static_drivers <- data.table::fread(paste0("data/out/ee/FIRED-daily-static-drivers_california_", static_version, ".csv"))
static_drivers[, `:=`(.geo = NULL, samp_id = NULL, `system:index` = NULL,
                      rumple_index = surf_area / proj_area,
                      road_density_mpha = (road_length_m) / (proj_area / 10000),
                      surf_area = NULL, road_length_m = NULL)]
static_drivers <- static_drivers[did %in% target_fires_did, ]

fluc_drivers <- data.table::fread(paste0("data/out/ee/FIRED-daily-fluctuating-drivers_california_", fluc_version, ".csv"))
# LCMS Landcovers 2 and 6 are only in Alaska, so we'll remove them here
fluc_drivers[, `:=`(.geo = NULL, samp_id = NULL, `system:index` = NULL,
                    veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                    ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                    lcms_landcover_02 = NULL, lcms_landcover_06 = NULL)]
fluc_drivers <- fluc_drivers[did %in% target_fires_did, ]

# lcms_landcover_desc <-
#   tribble(~value, ~color, ~description,
#           "01",	"#005e00", "Trees",
#           "02",	"#008000", "Tall Shrubs & Trees Mix (SEAK Only)",
#           "03",	"#00cc00", "Shrubs & Trees Mix",
#           "04",	"#b3ff1a", "Grass/Forb/Herb & Trees Mix",
#           "05",	"#99ff99", "Barren & Trees Mix",
#           "06",	"#b30088", "Tall Shrubs (SEAK Only)",
#           "07",	"#e68a00", "Shrubs",
#           "08",	"#ffad33", "Grass/Forb/Herb & Shrubs Mix",
#           "09",	"#ffe0b3", "Barren & Shrubs Mix",
#           "10",	"#ffff00", "Grass/Forb/Herb",
#           "11",	"#AA7700", "Barren & Grass/Forb/Herb Mix",
#           "12",	"#d3bf9b", "Barren or Impervious",
#           "13",	"#ffffff", "Snow or Ice",
#           "14",	"#4780f3", "Water",
#           "15",	"#1B1716", "Non-Processing Area Mask")

# merge together the static and fluctating drivers

daily_drivers <- 
  merge(static_drivers, fluc_drivers, by = c("did", "date", "id")) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10*10)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / proj_area)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / proj_area)) %>% 
  dplyr::select(did, id, date, everything())

daily_drivers
summary(daily_drivers)

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
# end_time <- Sys.time()
# print(difftime(end_time, start_time, units = "mins"))

other_fires_summary <- 
  read.csv(file = "data/out/other-fires-summary.csv") %>%
  dplyr::filter(did %in% target_fires_did) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  as_tibble()

# Convert coverage of different landcovers to actual area
# Each data type needs a different multiplier to reflect the scale at which the pixel count was done in Earth Engine
# csp_ergo_landforms: scale was 10.2m (based on NED DEM) so multiply the pixel count by 10.2*10.2=104.04
# lcms (including lcms_change, lcms_landcover, lcms_landuse): scale was 30m (based on Landsat) so multiply the pixel count by 30*30=900 m^2
# We also calculate the proportion of the total area within each category

#### --- end daily drivers prep


#### --- join daily drivers, current fires/fires-to-date data, and era5 drivers summarized to daily time steps

other_fires_summary
era5_drivers_summarized
rtma_drivers_summarized
gridmet_drivers
daily_drivers
fired_daily
fired_events

out_sf <- 
  fired_daily %>% 
  dplyr::select(-samp_id, -area_ha) %>% 
  dplyr::left_join(other_fires_summary, by = c("did", "id", "date")) %>% 
  dplyr::left_join(era5_drivers_summarized, by = c("did", "id", "date")) %>% 
  dplyr::left_join(rtma_drivers_summarized, by = c("did", "id", "date")) %>% 
  dplyr::left_join(gridmet_drivers, by = c("did", "id", "date")) %>% 
  dplyr::left_join(daily_drivers, by = c("did", "id", "date")) %>%
  dplyr::left_join(fired_daily_biggest_poly_centroids, by = c("did", "id", "date"))

out <- 
  out_sf %>% 
  sf::st_drop_geometry() %>% 
  as.data.table()



# Version 4 fixes an issue created in joining the daily scale driver data to the daily scale fire data
# where the event-scale fire data were mistakenly joined instead. This led to an issue where each
# day's area of increase, for instance, actually represented the maximum area of increase for the
# whole fire event, copied over for each day of that fire. We also use the Resolve biomes to separate
# out the different fires (based on which biome the whole fire footprint overlapped the most) which
# comes into play when calculating the area of increase residual (that is, the model is based on
# estimating area of increase as a function of cumulative area, and we fit a smooth for each biome
# now instead of each lc_name from MODIS, which seems to be not the correct data)

# Version 5 re-implements the area of increase residual model to *not* account for across-fire
# variation with a random intercept offset of FIRED id. 

# Version 6 uses the latest refactoring of Earth Engine code to extract "static" and "fluctating"
# drivers as separate products, without the need to mix and match the data that require info
# about the ignition year (e.g., NDVI) and data that do not (e.g., proportion of valleys)
# Version 6 also gets the ERA5 and Gridmet data as separate processes, and uses the centroids
# of the largest polygon as the site of interest for data extraction rather than the centroid
# of the overall fire polygon that day. This avoids the cases where multipolygons represent
# burning at the very edges of very large fires and the fire centroid is someplace in the middle
# where perhaps no burning is happening. E.g., late during the August Complex burning, some of
# the individual polygons within the multipolygon are 100 km apart

# Version 7 uses bilinear resampling for all of the weather variables. We also get the RTMA hourly
# weather data at a 2.5 km spatial resolution for fires that overlap that data product's
# temporal extent (2011 and onward). Data now go in data/out rather than data/out/analysis-ready
# because there is one additional step to processing the drivers that accounts for the 
# fire-independent scaling relationships

# version 8 uses LCMS data from previous year instead of fire year


sf::st_write(obj = out_sf, dsn = paste0("data/out/FIRED-daily-scale-drivers_california_", driver_version, ".gpkg"), 
             delete_dsn = file.exists(paste0("data/out/FIRED-daily-scale-drivers_california_", driver_version, ".gpkg")))
data.table::fwrite(x = out, file = paste0("data/out/FIRED-daily-scale-drivers_california_", driver_version, ".csv"))
