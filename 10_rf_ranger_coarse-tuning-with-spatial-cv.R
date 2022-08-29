library(dplyr)
# library(party)
# library(permimp)
# library(vita)
library(purrr)
library(yardstick)
library(spatialsample)
library(sf)
library(tidyr)
library(pbapply)
library(data.table)
# library(vip)
library(ranger)

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

spatial_cv <- function(splits, 
                       mtry = floor(sqrt(length(predictor.variable.names))), 
                       num.trees = 500, 
                       sample.fraction = 0.632, 
                       alpha = 0.5, 
                       minprop = 0.1,
                       min.node.size = 5,
                       class.wgts = 1) {
  
  analysis_data <- sf::st_drop_geometry(rsample::analysis(splits))
  assessment_data <- sf::st_drop_geometry(rsample::assessment(splits))
  
  if (!class.wgts) {
    class.wgts <- c(1, 1)
  } else {
    class.wgts <- 1 / table(analysis_data$ewe)
  }
  
  # fit the model to the analysis set
  # ranger() uses replace = FALSE and splitrule = "maxstat"
  # to avoid biased random forests that favor variables with lots of unique values
  fitted_model <- ranger::ranger(formula = rf_formula,
                                 data = analysis_data[, c("ewe", predictor.variable.names)],
                                 mtry = mtry,
                                 num.trees = num.trees,
                                 sample.fraction = sample.fraction,
                                 replace = FALSE,
                                 splitrule = "maxstat",
                                 alpha = alpha,
                                 minprop = minprop,
                                 min.node.size = min.node.size,
                                 case.weights = class.wgts[analysis_data$ewe + 1],
                                 num.threads = 10)
  
  tibble::tibble(
    o = assessment_data$ewe,
    p = predict(fitted_model, data = assessment_data, type = "response")$predictions
  )
}

spatial_cv_tune <- function(folds, mtry, num.trees, sample.fraction, alpha, minprop, min.node.size, classification_thresh, class.wgts, iter = 10) {
  
  results <- vector(mode = "list", length = iter)
  
  for (i in 1:iter) {
    spat_cv_out <- 
      folds %>%
      dplyr::mutate(.preds = purrr::map(splits, 
                                        spatial_cv, 
                                        mtry = mtry, 
                                        num.trees = num.trees, 
                                        sample.fraction = sample.fraction,
                                        alpha = alpha, 
                                        minprop = minprop, 
                                        min.node.size = min.node.size,
                                        class.wgts = class.wgts)) %>% 
      dplyr::mutate(assessment_ewe_n = purrr::map_int(splits, function(k) nrow(rsample::assessment(k))),
                    assessment_ewe_1 = purrr::map_int(splits, function(k) sum(rsample::assessment(k)$ewe)),
                    assessment_ewe_0 = assessment_ewe_n - assessment_ewe_1) %>% 
      tidyr::unnest(.preds)
    
    results[[i]] <- 
      spat_cv_out %>% 
      dplyr::select(id, o, p, id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1) %>% 
      dplyr::mutate(mtry = mtry,
                    num.trees = num.trees,
                    sample.fraction = sample.fraction,
                    classification_thresh = classification_thresh,
                    alpha = alpha,
                    minprop = minprop,
                    min.node.size = min.node.size,
                    class.wgts = class.wgts,
                    iter = i)
  }
  
  results_out <- data.table::rbindlist(results)
  
  return(results_out)
}

counter <- 4

for(counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
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
  
  fuel_drivers <- c("ndvi", "veg_structure_rumple", fuel_lcms_change_tm01, fuel_lcms_change_tm02, fuel_lcms_change_tm03, fuel_lcms_change_tm04, fuel_lcms_change_tm05, fuel_lcms_landcover_tm01)
  
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
  data$npl <- factor(data$npl, levels = 1:5)
  # data$ewe <- factor(data$ewe, levels = c(0, 1))
  
  # https://spatialsample.tidymodels.org/articles/spatialsample.html
  if (biome_shortname == "tgss") {
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_clustering_cv(v = 5)
    
  } else {
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_leave_location_out_cv(group = eco_name_daily)
  }
  
  # splits <- folds$splits[[1]]
  
  # autoplot(folds)
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # sample.fraction_vec <- c(seq(0.632 - 0.15, 0.632, length.out = 3), seq(0.632, 0.632 + 0.15, length.out = 3)[-1])
  sample.fraction_vec <- c(0.55, 0.632, 0.75)
  
  mtry_vec <- seq(from = floor(sqrt(length(predictor.variable.names))) - 3,
                  to = floor(length(predictor.variable.names) / 2),
                  by = 5)
  
  tune.df <- expand.grid(mtry = mtry_vec, 
                         num.trees = 500, 
                         sample.fraction = sample.fraction_vec,
                         # classification_thresh = c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55),
                         classification_thresh = 0.5,
                         alpha = c(0.1, 0.5, 0.9),
                         minprop = c(0, 0.1, 0.25), 
                         min.node.size = c(1, 3, 10),
                         class.wgts = TRUE,
                         iter = 5
  )
  
  
  out <- pblapply(X = (1:nrow(tune.df)), FUN = function(i) {
    return(spatial_cv_tune(folds = folds,
                           mtry = tune.df$mtry[i],
                           num.trees = tune.df$num.trees[i], 
                           sample.fraction = tune.df$sample.fraction[i],
                           classification_thresh = tune.df$classification_thresh[i],
                           alpha = tune.df$alpha[i],
                           minprop = tune.df$minprop[i],
                           min.node.size = tune.df$min.node.size[i],
                           class.wgts = tune.df$class.wgts[i],
                           iter = tune.df$iter[i]))
  })
  
  out_all <- data.table::rbindlist(out)
  data.table::fwrite(x = out_all, file = paste0("data/out/rf/rf_ranger_spatial-cv-coarse-tuning_", biome_shortname, ".csv"))
}


# Take the spatial CV results (observation for held out fold and predictions) and calculate various model skill metrics
pbapply::pblapply(X = biome_shortnames, FUN = function(biome_shortname) {
  biome_tune <- data.table::fread(file.path("data", "out", "rf", paste0("rf_ranger_spatial-cv-coarse-tuning_", biome_shortname, ".csv")))
  biome_tune[, `:=`(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1)))]
  
  spat_cv_fscore <- 
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::f_meas(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_auc <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::roc_auc(truth = o_fac, estimate = p, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_precision <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::precision(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_recall <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::recall(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_rmse <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::rmse(truth = o, estimate = p, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_accuracy <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::accuracy(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_specificity <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::specificity(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  results <- 
    rbind(spat_cv_fscore,
          spat_cv_auc,
          spat_cv_precision,
          spat_cv_recall,
          spat_cv_rmse,
          spat_cv_accuracy,
          spat_cv_specificity) %>% 
    dplyr::mutate(biome = biome_shortname) %>% 
    dplyr::select(biome, dplyr::everything())
  
  data.table::fwrite(x = results, file = paste0("data/out/rf/rf_ranger_spatial-cv-coarse-tuning-metrics_", biome_shortname, ".csv"))
})

# Look at the coarse tuning results and see how we can fine tune the hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/rf_ranger_spatial-cv-coarse-tuning-metrics_", biome_shortname, ".csv"))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)

tuning_metrics %>%
  filter(.metric == "f_meas") %>% 
  group_by(biome) %>% 
  arrange(desc(mean_estimate)) %>% 
  slice(1:10) %>% 
  print(n = 40)

# Begin the fine tuning using the same approach as above for the coarse tuning, just adjusting the data frame
# that describes the grid of hyperparameter combinations to test to reflect what we learned from the 
# coarse tuning (e.g., all the best hyperparameter combos had a minprop= argument of 0, so let's call that one
# tuned, and no need to try different values)

for (counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  fires <- as.data.frame(fires_all)
  fires <- fires[fires$biome_shortname == biome_shortname, ]
  
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
  
  fuel_drivers <- c("ndvi", "veg_structure_rumple", fuel_lcms_change_tm01, fuel_lcms_change_tm02, fuel_lcms_change_tm03, fuel_lcms_change_tm04, fuel_lcms_change_tm05, fuel_lcms_landcover_tm01)
  
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
  data$npl <- factor(data$npl, levels = 1:5)
  # data$ewe <- factor(data$ewe, levels = c(0, 1))
  
  # https://spatialsample.tidymodels.org/articles/spatialsample.html
  if (biome_shortname == "tgss") {
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_clustering_cv(v = 5)
    
  } else {
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_leave_location_out_cv(group = eco_name_daily)
  }
  
  # splits <- folds$splits[[1]]
  
  # autoplot(folds)
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # tcf
  # mtry values were mostly high so keep the variation high
  # best sample.fraction was 0.75 (the highest value in coarse tuning) so let it possibly be as high as 0.8 for this round
  # lots of variation in best alpha values, so keep that variation
  # minprop was always 0 for best models; don't test other values
  # min.node size fluctuated, so keep some variation here
  # this one is a pretty big model (high sample size), so stick with 5 iterations to dial in our expectation of the f measure
  if (biome_shortname == "tcf") {
    fine.tune.df <- expand.grid(mtry = 36:43,
                                num.trees = 500,
                                sample.fraction = c(0.75, 0.8),
                                classification_thresh = 0.5,
                                alpha = c(0.5, 0.75, 0.9, 1.0),
                                minprop = 0,
                                min.node.size = c(1, 2, 3, 5, 10),
                                class.wgts = TRUE,
                                iter = 5)
    # mfws
    # mtry values were mostly high, so start high and let them get higher
    # many sample.fractions were default 0.632 with a couple at 0.55 
    # best alpha values were equally 0.9 and 0.5, so try an in between (and also 1)
    # minprop was always 0 for best models; don't test other values
    # min.node size fluctuated, so keep some variation here
    # this one is a medium-sized model (lower sample size), so use 10 iterations to dial in our expectation of the f measure
  } else if (biome_shortname == "mfws") {
    fine.tune.df <- expand.grid(mtry = 38:45,
                                num.trees = 500,
                                sample.fraction = c(0.55, 0.6, 0.632),
                                classification_thresh = 0.5,
                                alpha = c(0.5, 0.75, 0.9, 1.0),
                                minprop = 0,
                                min.node.size = c(3, 5, 10),
                                class.wgts = TRUE,
                                iter = 10)
    # tgss
    # mtry values were mostly high so keep the variation high
    # best sample.fraction was 0.75 (the highest value in coarse tuning) so let it possibly be as high as 0.8 for this round
    # lots of variation in best alpha values, so keep that variation
    # minprop was always 0 for best models; don't test other values
    # min.node size fluctuated, so keep some variation here
    # this one is a pretty small model (lower sample size), so use 10 iterations to dial in our expectation of the f measure
  } else if (biome_shortname == "tgss") {
    fine.tune.df <- expand.grid(mtry = 14:21,
                                num.trees = 500,
                                sample.fraction = c(0.75, 0.8),
                                classification_thresh = 0.5,
                                alpha = c(0.5, 0.75, 0.9, 1.0),
                                minprop = 0,
                                min.node.size = c(1, 2, 3, 5, 10),
                                class.wgts = TRUE,
                                iter = 10)
    # dxs
    # mtry values were in the middle of the range
    # best sample.fraction was 0.55 (the lowest value in coarse tuning) so let it possibly be as low as 0.5 for this round
    # best alpha values were high, so try only high values
    # minprop was always 0 for best models; don't test other values
    # min.node size fluctuated, so keep some variatino here
    # this one is a pretty small model (lower sample size), so use 20 iterations to dial in our expectation of the f measure
  } else if (biome_shortname == "dxs") {
    
    fine.tune.df <- expand.grid(mtry = 4:13,
                                num.trees = 500,
                                sample.fraction = c(0.5, 0.55, 0.6),
                                classification_thresh = 0.5,
                                alpha = c(0.8, 0.9, 1.0),
                                minprop = 0,
                                min.node.size = c(1, 2, 3, 5, 10),
                                class.wgts = TRUE,
                                iter = 20)
  }
  
  
  
  out <- pblapply(X = (1:nrow(fine.tune.df)), FUN = function(i) {
    return(spatial_cv_tune(folds = folds,
                           mtry = fine.tune.df$mtry[i],
                           num.trees = fine.tune.df$num.trees[i], 
                           sample.fraction = fine.tune.df$sample.fraction[i],
                           classification_thresh = fine.tune.df$classification_thresh[i],
                           alpha = fine.tune.df$alpha[i],
                           minprop = fine.tune.df$minprop[i],
                           min.node.size = fine.tune.df$min.node.size[i],
                           class.wgts = fine.tune.df$class.wgts[i],
                           iter = fine.tune.df$iter[i]))
  })
  
  out_all <- data.table::rbindlist(out)
  data.table::fwrite(x = out_all, file = paste0("data/out/rf/rf_ranger_spatial-cv-fine-tuning_", biome_shortname, ".csv"))
}

pbapply::pblapply(X = biome_shortnames, FUN = function(biome_shortname) {
  
  biome_tune <- data.table::fread(input = paste0("data/out/rf/rf_ranger_spatial-cv-fine-tuning_", biome_shortname, ".csv"))
  biome_tune[, `:=`(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1)))]
  
  spat_cv_fscore <- 
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::f_meas(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_auc <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::roc_auc(truth = o_fac, estimate = p, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_precision <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::precision(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_recall <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::recall(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_rmse <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::rmse(truth = o, estimate = p, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_accuracy <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::accuracy(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  spat_cv_specificity <-
    biome_tune %>% 
    dplyr::group_by(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter) %>%
    yardstick::specificity(truth = o_fac, estimate = p_fac, estimator = "binary", event_level = "second") %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric, iter) %>% 
    dplyr::summarize(.estimate = weighted.mean(x = .estimate, w = assessment_ewe_1)) %>% 
    dplyr::group_by(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>%
    dplyr::summarize(mean_estimate = mean(.estimate, na.rm = TRUE),
                     sd_estimate = sd(.estimate, na.rm = TRUE),
                     n = n(),
                     min_estimate = min(.estimate),
                     max_estimate = max(.estimate),
                     lwr = mean_estimate - sd_estimate,
                     upr = mean_estimate + sd_estimate)
  
  results <- 
    rbind(spat_cv_fscore,
          spat_cv_auc,
          spat_cv_precision,
          spat_cv_recall,
          spat_cv_rmse,
          spat_cv_accuracy,
          spat_cv_specificity) %>% 
    dplyr::mutate(biome = biome_shortname) %>% 
    dplyr::select(biome, dplyr::everything())
  
  results %>%
    filter(.metric == "f_meas") %>% 
    group_by(biome) %>% 
    arrange(desc(lwr)) %>% 
    slice(1:10) %>% 
    print(n = 40)
  
  biome  mtry num.trees sample.fraction classification_thresh alpha minprop min.node.size class.wgts .metric mean_esti…¹ sd_es…²     n min_e…³ max_e…⁴   lwr   upr
  <chr> <int>     <int>           <dbl>                 <dbl> <dbl>   <dbl>         <int> <lgl>      <chr>         <dbl>   <dbl> <int>   <dbl>   <dbl> <dbl> <dbl>
    1 dxs       6       500           0.55                    0.5   0.9       0             3 TRUE       f_meas        0.879 0.00422     5   0.873   0.882 0.874 0.883
  2 dxs       6       500           0.632                   0.5   0.9       0             3 TRUE       f_meas        0.878 0.00250     5   0.876   0.882 0.876 0.881
  3 dxs      41       500           0.75                    0.5   0.5       0            10 TRUE       f_meas        0.874 0.00676     5   0.864   0.880 0.868 0.881
  4 dxs       6       500           0.55                    0.5   0.9       0             1 TRUE       f_meas        0.874 0.00684     5   0.863   0.882 0.867 0.881
  5 dxs      11       500           0.55                    0.5   0.9       0             1 TRUE       f_meas        0.874 0.00595     5   0.867   0.882 0.868 0.880
  6 dxs       6       500           0.632                   0.5   0.9       0             1 TRUE       f_meas        0.874 0.0108      5   0.863   0.888 0.863 0.884
  7 dxs      11       500           0.632                   0.5   0.1       0             3 TRUE       f_meas        0.873 0           5   0.873   0.873 0.873 0.873
  8 dxs       6       500           0.55                    0.5   0.9       0            10 TRUE       f_meas        0.872 0.00751     5   0.863   0.882 0.865 0.880
  9 dxs      11       500           0.55                    0.5   0.5       0            10 TRUE       f_meas        0.872 0.00735     5   0.863   0.882 0.864 0.879