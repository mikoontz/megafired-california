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
library(patchwork)
library(readr)

# read in fire data
system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/  data/out/", stdout = TRUE)  

# read in driver descriptions
system2(command = "aws", args = "s3 cp s3://california-megafires/tables/driver-descriptions.csv  tables/driver-descriptions.csv", stdout = TRUE)  

fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

driver_descriptions <- read.csv(file = "tables/driver-descriptions.csv")

run_rf_for_biome <- function(fired_drivers_fname,
                             biome_shortname,
                             distance_thresholds = c(0, 1000, 5000, 10000, 25000, 50000),
                             random_seed = 1848,
                             spatial = FALSE) {
  fires <-
    data.table::fread(fired_drivers_fname) %>%
    dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                  -cumu_count, -cumu_area_ha,
                  -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                  -starts_with("adj"),
                  -ends_with("rtma"), -ends_with("rtma_pct"),
                  -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
    dplyr::left_join(fired_daily_response) %>% 
    dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha),
                  area_log10_pct = ecdf(area_log10)(area_log10),
                  ewe = ifelse(area_log10_pct >= 0.95, yes = 1, no = 0)) %>% 
    as.data.frame()
  
  idx <- substr(names(fires), start = 1, stop = 4) == "raw_"
  names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]
  
  # ### Set 10
  human_drivers <- c("npl", "concurrent_fires")
  weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy",
                       "max_temp_pct", "min_temp_pct",
                       "max_rh_pct", "min_rh_pct",
                       "max_vpd_pct", "min_vpd_pct",
                       "max_soil_water_pct", "min_soil_water_pct",
                       "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z",
                       "fm100_pct", "fm1000_pct")
  topography_drivers <- c("rumple_index")
  fuel_drivers <- c("ndvi", "veg_structure_rumple")
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
    "ndvi", "veg_structure_rumple",
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
  
  data_sf <-
    data %>%
    sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

  dist_mat <-
    sf::st_distance(x = data_sf,
                    y = data_sf) %>%
    units::drop_units()
  
  (start_time <- Sys.time())
  biome_nonspatial <- spatialRF::rf(
    data = data,
    dependent.variable.name = "area_log10",
    predictor.variable.names = predictor.variable.names_reduced,
    distance.matrix = dist_mat,
    distance.thresholds = distance_thresholds,
    xy = xy, #not needed by rf, but other functions read it from the model
    seed = random_seed,
    verbose = TRUE
  )
  (end_time <- Sys.time())
  (difftime(time1 = end_time, time2 = start_time, units = "mins"))
 
  readr::write_rds(x = biome_nonspatial, file = file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_nonspatial.rds")))

    biome_spatial <- biome_nonspatial
  
  if (spatial) {
  biome_spatial <- spatialRF::rf_spatial(
    model = biome_nonspatial,
    method = "mem.moran.sequential", #default method
    verbose = FALSE,
    seed = random_seed
  )
  readr::write_rds(x = biome_spatial, file = file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_spatial.rds")))
  system2(command = "aws", args = paste0("s3 cp data/out/rf/rf_", biome_shortname, "_spatial.rds s3://california-megafires/data/out/rf/rf_", biome_shortname, "_spatial.rds"), stdout = TRUE)  
}
  
  biome_response_curves_gg <-
    spatialRF::plot_response_curves(
      biome_spatial,
      quantiles = c(0.5),
      ncol = 5
    )

  system2(command = "aws", args = paste0("s3 cp data/out/rf/rf_", biome_shortname, "_nonspatial.rds s3://california-megafires/data/out/rf/rf_", biome_shortname, "_nonspatial.rds"), stdout = TRUE)  
  
  out <- list(model = biome_spatial, nonspatial_model = biome_nonspatial, plot = biome_response_curves_gg)
  
  return(out)
}

#### 
tcf <- run_rf_for_biome(fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v3.csv",
                        biome_shortname = "tcf",
                        distance_thresholds = c(0, 1000, 2000, 4000, 8000, 16000, 32000, 64000),
                        random_seed = 1240)
mfws <- run_rf_for_biome(fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v3.csv",
                         biome_shortname = "mfws",
                         distance_thresholds = c(0, 1000, 2000, 4000, 8000, 16000, 32000, 64000),
                         random_seed = 1240)
tgss <- run_rf_for_biome(fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v3.csv",
                         biome_shortname = "tgss",
                         distance_thresholds = c(0, 1000, 2000, 4000, 8000, 16000, 32000, 64000),
                         random_seed = 1240)
dxs <- run_rf_for_biome(fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v3.csv",
                        biome_shortname = "dxs",
                        distance_thresholds = c(0, 1000, 2000, 4000, 8000, 16000, 32000, 64000),
                        random_seed = 1240)


response_tcf <- patchwork::patchworkGrob(x = tcf$plot + patchwork::plot_annotation(title = paste0("Temperate Conifer Forests (oob R^2 = ", round(tcf$model$r.squared, 2), ")")) & theme(legend.position = "none"))
response_mfws <-  patchwork::patchworkGrob(x = mfws$plot + patchwork::plot_annotation(title = paste0("Mediterranean Forests, Woodlands & Scrub (oob R^2 = ", round(mfws$model$r.squared, 2), ")")) & theme(legend.position = "none"))
response_tgss <- patchwork::patchworkGrob(x = tgss$plot + patchwork::plot_annotation(title = paste0("Temperate Grasslands, Savannas & Shrublands (oob R^2 = ", round(tgss$model$r.squared, 2), ")")) & theme(legend.position = "none"))
response_dxs <- patchwork::patchworkGrob(x = dxs$plot + patchwork::plot_annotation(title = paste0("Deserts & Xeric Shrublands (oob R^2 = ", round(dxs$model$r.squared, 2), ")")) & theme(legend.position = "none"))

response_curve_patchwork <- (patchwork::wrap_elements(full = response_tcf) + patchwork::wrap_elements(full = response_mfws)) /  (patchwork::wrap_elements(full = response_tgss) + patchwork::wrap_elements(full = response_dxs))


imp_tcf <- spatialRF::plot_importance(tcf$model) + ggtitle(label = "Temperate Conifer Forests", subtitle = "Permutation importance computed on the out-of-bag data")
imp_mfws <- spatialRF::plot_importance(mfws$model) + ggtitle(label = "Mediterranean Forests, Woodlands & Scrub", subtitle = "Permutation importance computed on the out-of-bag data")
imp_tgss <- spatialRF::plot_importance(tgss$model) + ggtitle(label = "Temperate Grasslands, Savannas & Shrublands", subtitle = "Permutation importance computed on the out-of-bag data")
imp_dxs <- spatialRF::plot_importance(dxs$model) + ggtitle(label = "Deserts & Xeric Shrublands", subtitle = "Permutation importance computed on the out-of-bag data")

imp_patchwork <- (imp_tcf + imp_mfws) / (imp_tgss + imp_dxs)
