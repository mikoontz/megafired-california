# One location for prepping analysis-ready data for the RF model

library(dplyr)

prep_fires <- function(fires_all, biome_shortname) {
  
  fires <- as.data.frame(fires_all)
  fires <- fires[fires$biome_shortname == biome_shortname, ]
  
  human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
  topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm", "peak_ridge", "peak_ridge_cool", "mountain_divide", "cliff", "upper_slope_warm", "upper_slope", "upper_slope_cool", "lower_slope_warm", "lower_slope", "lower_slope_cool", "valley", "valley_narrow", "landform_diversity")
  weather_drivers <- c("wind_anisotropy_rtma", "max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct", "max_wind_filled_gust_rtma_pct", "min_wind_filled_gust_rtma_pct",
                       "max_temp_rtma_pct", "min_temp_rtma_pct", 
                       "max_rh_rtma_pct", "min_rh_rtma_pct", "max_vpd_rtma_pct", "min_vpd_rtma_pct",
                       "bi_pct", "erc_pct", "fm100_pct", "fm1000_pct", "pdsi_z",
                       "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y")
  
  fuel_lcms_change_tm01 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm01")
  fuel_lcms_change_tm02 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm02")
  fuel_lcms_change_tm03 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm03")
  fuel_lcms_change_tm04 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm04")
  fuel_lcms_change_tm05 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm05")
  
  fuel_lcms_landcover_tm01 <- paste0(c("trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren", "landcover_diversity"), "_tm01")
  
  fuel_drivers <- c("ndvi", "veg_structure_rumple", fuel_lcms_change_tm01, fuel_lcms_change_tm02, fuel_lcms_change_tm03, fuel_lcms_change_tm04, fuel_lcms_change_tm05, fuel_lcms_landcover_tm01)
  
  interacting_drivers <- c("wind_terrain_anisotropy_rtma", "wind_terrain_alignment_rtma")
  
  fire_drivers <- "sqrt_aoi_tm1"
  
  predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, fire_drivers)
  
  # no columnns with 0 variance (rounded to 4 decimal places)
  zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var, na.rm = TRUE), 4) == 0]
  
  predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
  
  fires <-
    fires %>%
    dplyr::select(-all_of(zero_variance_columns))

  # Which variables have lots of NAs?
  apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))
  
  # Drop all rows that have an NA in any column
  na_dids <- fires[!complete.cases(fires), "did"]
  
  fires <- fires[complete.cases(fires), ]
  
  data <- fires[, c("did", "ewe", "biome_name_daily", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
  data$npl <- factor(data$npl, levels = 1:5)
  data$concurrent_fires <- as.numeric(data$concurrent_fires)
  
  return(list(data = data, 
              predictor.variable.names = predictor.variable.names, 
              human_drivers = human_drivers,
              topography_drivers = topography_drivers,
              weather_drivers = weather_drivers,
              fuel_drivers = fuel_drivers,
              interacting_drivers = interacting_drivers,
              fire_drivers = fire_drivers,
              dropped_cols = zero_variance_columns,
              dropped_rows = na_dids))
}
