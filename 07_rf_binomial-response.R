# Random Forest predicting EWE or not based on adjusted fuel/topography/human factors

library(dplyr)
library(sf)
library(data.table)
library(spatialRF)

fires <- 
  data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v4.csv") %>% 
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
                ewe = ifelse(area_log10_pct >= 0.95, yes = 1, no = 0)) %>% 
  dplyr::select(id, did, ewe, area_log10, barriers_to_spread, change_diversity, starts_with("csp_ergo_landforms"), elevation, flat, friction, friction_walking_only, landcover_diversity, landform_diversity, starts_with("lcms_change"), starts_with("lcms_landcover"), lower_slope, ndvi, road_density_mpha, rumple_index, upper_slope, valleys, veg_structure_rumple, npl, npl_at_ignition, concurrent_fires, wind_anisotropy, wind_terrain_anisotropy, wind_terrain_alignment, max_wind_speed_pct, min_wind_speed_pct, max_rh_pct, min_rh_pct, max_temp_pct, min_temp_pct, max_soil_water_pct, min_soil_water_pct, max_vpd_pct, min_vpd_pct, starts_with("spei"), pdsi_z, erc_pct, bi_pct, fm100_pct, fm1000_pct, sqrt_aoi_tm1) %>% 
  as.data.frame()

human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct",
                     "max_temp_pct", "min_temp_pct",
                     "max_rh_pct", "min_rh_pct",
                     "max_vpd_pct", "min_vpd_pct",
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z",
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_alignment", "bi_pct", "erc_pct")

# human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
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
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 55)]

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

cor.mat <- cor(fires[, predictor.variable.names])
sort(cor.mat[, colnames(cor.mat) == "sqrt_aoi_tm1"])
sort(cor.mat[, colnames(cor.mat) == "erc_pct"])

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

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "ewe",
  predictor.variable.names = predictor.variable.names_reduced,
  ranger.arguments = list(classification = TRUE),
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

fires_for_amy <- data[, c("did", "ewe", predictor.variable.names_reduced$selected.variables)]
dim(fires_for_amy)

data.table::fwrite(x = fires_for_amy, file = "data/out/rf-binomial-response_amy-check.csv")