library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyr)
library(terra)
library(spatialRF)
library(vegan)
library(randomForestExplainer)
library(pdp)

# read in fire data
system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/  data/out/", stdout = TRUE)  

fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

#### ---- Temperate Conifer Forests ---- ####
fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v2.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))

fires <- 
  fires %>% 
  as.data.frame()


human_drivers <- c("npl", "npl_at_ignition", "concurrent_fires", "road_density_mpha", "friction")
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

### Set 7
# human_drivers <- c("npl", "concurrent_fires", "road_density_mpha", "friction")
# weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
#                      "max_temp_pct", "min_temp_pct", 
#                      "max_rh_pct", "min_rh_pct", 
#                      "max_vpd_pct", "min_vpd_pct", 
#                      "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
#                      "fm100_pct", "fm1000_pct")
# topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
# fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")
### Set 6
# human_drivers <- c("npl", "concurrent_fires", "friction")
# weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
#                      "max_temp_pct", "min_temp_pct", 
#                      "max_rh_pct", "min_rh_pct", 
#                      "max_vpd_pct", "min_vpd_pct", 
#                      "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
#                      "fm100_pct", "fm1000_pct")
# topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
# fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

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
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

tcf_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

tcf_dist_mat <- 
  sf::st_distance(x = tcf_sf, 
                  y = tcf_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_log10",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = tcf_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tcf_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tcf_nonspatial_response_curves_accentuate_gg
