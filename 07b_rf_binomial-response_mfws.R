# Random Forest predicting EWE or not based on adjusted fuel/topography/human factors

library(dplyr)
library(sf)
library(data.table)
library(spatialRF)

dir.create(file.path("data", "out", "rf"), recursive = TRUE, showWarnings = FALSE)

# biome_shortname <- "tcf"
biome_shortname <- "mfws"
# biome_shortname <- "tgss"
# biome_shortname <- "dxs"

system2(command = "aws", args = "s3 cp s3://california-megafires/data/out/fired_daily_ca_response-vars.csv  data/out/fired_daily_ca_response-vars.csv", stdout = TRUE)  

system2(command = "aws", args = paste0("s3 cp s3://california-megafires/data/out/analysis-ready/FIRED-daily-scale-drivers_california_", biome_shortname, "_v5.csv  data/out/analysis-ready/FIRED-daily-scale-drivers_california_", biome_shortname, "_v5.csv"), stdout = TRUE)  

system2(command = "aws", args = "s3 cp s3://california-megafires/data/out/fired_events_ca_epsg3310_2003-2020.gpkg  data/out/fired_events_ca_epsg3310_2003-2020.gpkg", stdout = TRUE)  

system2(command = "aws", args = "s3 cp s3://california-megafires/tables/driver-descriptions.csv  tables/driver-descriptions.csv", stdout = TRUE)  

fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

driver_descriptions <- read.csv(file = "tables/driver-descriptions.csv")

target_event_ids <-
  sf::read_sf("data/out/fired_events_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  dplyr::filter(area_ha >= 121.406) %>% 
  dplyr::pull(id)

fires <- 
  data.table::fread(paste0("data/out/analysis-ready/FIRED-daily-scale-drivers_california_", biome_shortname, "_v5.csv")) %>% 
  dplyr::filter(id %in% target_event_ids) %>% 
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"),
                -starts_with("raw")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))

idx <- substr(names(fires), start = 1, stop = 4) == "adj_"
names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]

fires <-
  fires %>% 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10),
                ewe = ifelse(area_log10_pct >= 0.95, yes = 1, no = 0)) %>% # 95th percentile growth for temperate conifer forests is 1683.266 ha per day
  dplyr::select(id, did, ewe, date, x_biggest_poly_3310, y_biggest_poly_3310, 
                area_log10, barriers_to_spread, change_diversity, 
                starts_with("csp_ergo_landforms"), elevation, flat, friction, 
                friction_walking_only, landcover_diversity, landform_diversity, 
                starts_with("lcms_change"), starts_with("lcms_landcover"), 
                lower_slope, ndvi, road_density_mpha, rumple_index, upper_slope, 
                valleys, veg_structure_rumple, npl, npl_at_ignition, 
                concurrent_fires, wind_anisotropy, wind_terrain_anisotropy, 
                wind_terrain_alignment, max_wind_speed_pct, min_wind_speed_pct, 
                max_rh_pct, min_rh_pct, max_temp_pct, min_temp_pct, 
                max_soil_water_pct, min_soil_water_pct, max_vpd_pct, 
                min_vpd_pct, starts_with("spei"), pdsi_z, erc_pct, bi_pct, 
                fm100_pct, fm1000_pct, sqrt_aoi_tm1)

fires <-
  fires %>% 
  dplyr::filter(ewe == 1) %>% 
  dplyr::group_by(id) %>% 
  dplyr::arrange(date) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  rbind(fires[fires$ewe == 0, ]) %>% 
  as.data.frame()

human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy",
                     "max_temp_pct", "min_temp_pct",
                     "max_rh_pct", "min_rh_pct",
                     "max_vpd_pct", "min_vpd_pct",
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z",
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

# No NAs
apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])

fires <- fires[!(fires$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]

fires <- 
  fires %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires
distance_thresholds = c(0, 1000, 2000, 4000, 8000, 16000, 32000, 64000)

random_seed <- 2203
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

data_sf <-
  data %>%
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

dist_mat <-
  sf::st_distance(x = data_sf,
                  y = data_sf) %>%
  units::drop_units()

### ----- FEATURE ENGINEERING

# (start_time <- Sys.time())
# ftr_eng <- spatialRF::the_feature_engineer(data = data,
#                                            dependent.variable.name = "ewe",
#                                            predictor.variable.names = predictor.variable.names_reduced,
#                                            xy = xy)
# (end_time <- Sys.time())
# (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Fitting and evaluating a model without interactions.
# Testing 21 candidate interactions.
# No promising interactions found. 
# 
# > (end_time <- Sys.time())
# [1] "2022-07-12 11:43:58 MDT"
# > (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 3.999531 mins

(start_time <- Sys.time())
biome_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "ewe",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

# version 1 of the binomial response model predicts whether daily area of increase is in top 95th percentile of daily area of increase
# version 2 of the binomial response model predicts whether daily area of increase is in top 95th percentile of daily area of increase as a regression problem
# version 3 of the binomial response model predicts whether daily AOI is in top 95th percentile, but only uses the first time that happens for each fire
# using "adjusted" variables, some of which are scale-dependent regardless of fire process
readr::write_rds(x = biome_nonspatial, file = file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial_v3.rds")))
system2(command = "aws", args = paste0("s3 cp data/out/rf/rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial_v3.rds s3://california-megafires/data/out/rf/rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial_v3.rds"), stdout = TRUE)  

biome_spatial <- biome_nonspatial

(start_time <- Sys.time())
biome_spatial <- spatialRF::rf_spatial(
  model = biome_nonspatial,
  method = "mem.moran.sequential", #default method
  verbose = TRUE,
  seed = random_seed
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

readr::write_rds(x = biome_spatial, file = file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_binomial-response-95th-pct-ewe_spatial_v3.rds")))
system2(command = "aws", args = paste0("s3 cp data/out/rf/rf_", biome_shortname, "_binomial-response-95th-pct-ewe_spatial_v3.rds s3://california-megafires/data/out/rf/rf_", biome_shortname, "_binomial-response-95th-pct-ewe_spatial_v3.rds"), stdout = TRUE)
