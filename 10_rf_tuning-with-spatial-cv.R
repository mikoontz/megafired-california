library(dplyr)
library(party)
library(permimp)
library(vita)
library(purrr)
library(yardstick)
library(spatialsample)
library(sf)
library(tidyr)
library(pbapply)
library(data.table)
library(vip)

analysis_ready_nonspatial_version <- "v1"
analysis_ready_nonspatial_fname <- paste0("data/out/analysis-ready/FIRED-daily-scale-drivers_california_", analysis_ready_nonspatial_version, ".csv")

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Read analysis-ready data
fires_all <- data.table::fread(input = analysis_ready_nonspatial_fname)

# Here is where we can exclude any fires we don't want in the analysis

# Remove fires that never reached more than 121 hectares (300 acres)
target_event_ids <-
  sf::read_sf("data/out/fired_events_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  dplyr::filter(area_ha >= 121.406) %>% 
  dplyr::pull(id)

fires_all <- fires_all[id %in% target_event_ids,]

# Remove all event days that occur after the first EWE for that event
# fires <-
#   fires %>%
#   dplyr::filter(cumu_ewe <= 1)

# Just include eco regions that have experienced more than 10 EWE's
# target_eco_names <- 
#   fires_all %>% 
#   group_by(eco_name_daily, ewe) %>% 
#   tally() %>% 
#   filter(ewe == 1, n > 10) %>% 
#   pull(eco_name_daily)
# 
# fires_all <-
#   fires_all %>% 
#   dplyr::filter(eco_name_daily %in% target_eco_names)

for(counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  
  fires <- as.data.frame(fires_all)
  fires <- fires[fires$biome_shortname == biome_shortname, ]
  
  # version 17 uses LCMS change and landcover data from 1 year prior to fire and systematic tuning
  
  human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
  topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm", "peak_ridge", "peak_ridge_cool", "mountain_divide", "cliff", "upper_slope_warm", "upper_slope", "upper_slope_cool", "lower_slope_warm", "lower_slope", "lower_slope_cool", "valley", "valley_narrow", "landform_diversity")
  weather_drivers <- c("wind_anisotropy", "max_wind_speed_pct", "min_wind_speed_pct",
                       "max_temp_pct", "min_temp_pct", "bi_pct", "erc_pct",
                       "max_rh_pct", "min_rh_pct", "max_vpd_pct", "min_vpd_pct", "max_soil_water_pct", "min_soil_water_pct",
                       "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "fm100_pct", "fm1000_pct")
  
  fuel_lcms_change_tm01 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm01")
  fuel_lcms_change_tm02 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm02")
  fuel_lcms_change_tm03 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm03")
  fuel_lcms_change_tm04 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm04")
  fuel_lcms_change_tm05 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm05")
  
  fuel_lcms_landcover_tm01 <- paste0(c("trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren", "landcover_diversity"), "_tm01")
  
  fuel_drivers <- c("ndvi", "veg_structure_rumple", fuel_lcms_change_tm01, fuel_lcms_landcover_tm01)
  
  interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment")
  
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
  
  # BEGIN TUNING of the conditional random forest using spatial cross validation
  # Build a cforest() model with the best spatially cross-validated AUC value
  data <- fires[, c("did", "ewe", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
  # data$ewe <- factor(data$ewe, levels = c(0, 1))
  
  # https://spatialsample.tidymodels.org/articles/spatialsample.html
  folds <-
    data %>%
    sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
    spatialsample::spatial_leave_location_out_cv(group = eco_name_daily)
  
  # folds <-
  #   data %>% 
  #   sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>% 
  #   spatialsample::spatial_clustering_cv(v = 10)
  
  # splits <- folds$splits[[1]]
  
  # autoplot(folds)
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  tune.df <- expand.grid(mtry = seq(3, length(predictor.variable.names), by = 3), 
                         ntree = c(500, 750, 1000, 1100, 1250), 
                         classification_thresh = seq(0.2, 0.5, by = 0.05))
  
  
  
  spatial_cv_party <- function(splits, mtry = 5, ntree = 500) {
    
    analysis_data <- sf::st_drop_geometry(rsample::analysis(splits))
    assessment_data <- sf::st_drop_geometry(rsample::assessment(splits))
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # fit the model to the analysis set
    # cforest_unbiased() uses replace = FALSE, testat = "quad", and testtype = "Univ" as suggested by Strobl et al., 2007
    # to avoid biased random forests that favor variables that have lots of unique values
    party_model <- party::cforest(formula = rf_formula,
                                  data = analysis_data[, c("ewe", predictor.variable.names)],
                                  weights = class.wgts[analysis_data$ewe + 1],
                                  control = cforest_unbiased(mtry = mtry, ntree = ntree))
    
    # return the assessment set, with true and predicted price
    tibble::tibble(
      o = assessment_data$ewe,
      p = predict(party_model, newdata = assessment_data, type = "response")[, "ewe"]
    )
  }
  
  # spat_cv_party_out <-
  #   folds %>%
  #   slice(1) %>%
  #   mutate(.preds = purrr::map(splits, spatial_cv_party, mtry = 5, ntree = 500)) %>%
  #   tidyr::unnest(.preds)
  
  spat_cv_party_tune <- function(mtry, ntree, classification_thresh) {
    spat_cv_party_out <- 
      folds %>%
      mutate(.preds = purrr::map(splits, spatial_cv_party, mtry = mtry, ntree = ntree)) %>% 
      tidyr::unnest(.preds)
    
    spat_cv_auc <- 
      spat_cv_party_out %>%
      dplyr::mutate(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1))) %>% 
      dplyr::group_by(id) %>%
      yardstick::roc_auc(truth = o_fac, estimate = p, estimator = "binary", event_level = "second")
    
    # spat_cv_auc
    # mean(spat_cv_auc$.estimate)
    
    spat_cv_fscore <- 
      spat_cv_party_out %>%
      dplyr::mutate(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1))) %>% 
      dplyr::group_by(id) %>%
      yardstick::f_meas(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second")
    
    # spat_cv_fscore
    # mean(spat_cv_fscore$.estimate, na.rm = TRUE)
    # 
    spat_cv_precision <- 
      spat_cv_party_out %>%
      dplyr::mutate(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1))) %>% 
      dplyr::group_by(id) %>%
      yardstick::precision(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second")
    
    # spat_cv_precision
    # mean(spat_cv_precision$.estimate, na.rm = TRUE)
    # 
    spat_cv_recall <- 
      spat_cv_party_out %>%
      dplyr::mutate(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1))) %>% 
      dplyr::group_by(id) %>%
      yardstick::recall(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second")
    
    # spat_cv_recall
    # mean(spat_cv_recall$.estimate)
    # 
    spat_cv_rmse <- 
      spat_cv_party_out %>%
      dplyr::mutate(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1))) %>% 
      dplyr::group_by(id) %>%
      yardstick::rmse(truth = as.numeric(as.character(o)), estimate = p, estimator = "binary", event_level = "second")
    
    # spat_cv_rmse
    
    results <- rbind(spat_cv_auc,
                     spat_cv_fscore,
                     spat_cv_precision,
                     spat_cv_recall,
                     spat_cv_rmse) %>% 
      dplyr::mutate(mtry = mtry,
                    ntree = ntree,
                    classification_thresh = classification_thresh)
    
    return(results)
  }
  
  out <- pblapply(X = 1:nrow(tune.df), FUN = function(i) {
    return(spat_cv_party_tune(mtry = tune.df$mtry[i],
                              ntree = tune.df$ntree[i], 
                              classification_thresh = tune.df$classification_thresh[i]))
  })
  
  out_all <- data.table::rbindlist(out)
  
  data.table::fwrite(x = out_all, file = paste0("data/out/rf/rf-spat-cv-tuning_", biome_shortname, ".csv"))
  
  # out_all %>% 
  #   filter(.metric == "f_meas") %>% 
  #   group_by(classification_thresh, mtry, num.trees) %>% 
  #   summarize(f = mean(.estimate)) %>% 
  #   arrange(desc(f))
  
}

