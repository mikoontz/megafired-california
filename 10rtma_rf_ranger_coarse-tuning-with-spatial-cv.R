library(dplyr)
library(purrr)
library(yardstick)
library(spatialsample)
library(sf)
library(tidyr)
library(pbapply)
library(data.table)
library(ranger)
library(PRROC)
# library(mlr3measures)
# library(MLmetrics)

analysis_ready_nonspatial_version <- "v2"
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

# Subset to just years where there are RTMA data (2011 and later)
fires_all <-
  fires_all %>%
  dplyr::filter(date >= as.Date("2011-01-01"))

# drop ERA5 columns in favor of RTMA columns for the weather variables
fires_all <-
  fires_all %>% 
  dplyr::select(-contains("era5"))


# Remove all event days that occur after the first EWE for that event
# fires_all <-
#   fires_all %>%
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
                       class.wgts = FALSE) {
  
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
                                 num.threads = 11)
  
  tibble::tibble(
    o = assessment_data$ewe,
    p = predict(fitted_model, data = assessment_data, type = "response")$predictions
  )
}

spatial_cv_tune <- function(folds, mtry, num.trees, sample.fraction, alpha, minprop, min.node.size, class.wgts, iter = 10) {
  
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
                    alpha = alpha,
                    minprop = minprop,
                    min.node.size = min.node.size,
                    class.wgts = class.wgts,
                    iter = i)
  }
  
  results_out <- data.table::rbindlist(results)
  
  return(results_out)
}

for(counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  fires <- as.data.frame(fires_all)
  fires <- fires[fires$biome_shortname == biome_shortname, ]
  
  human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
  topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm", "peak_ridge", "peak_ridge_cool", "mountain_divide", "cliff", "upper_slope_warm", "upper_slope", "upper_slope_cool", "lower_slope_warm", "lower_slope", "lower_slope_cool", "valley", "valley_narrow", "landform_diversity")
  weather_drivers <- c("wind_anisotropy_rtma", "max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct", "max_wind_filled_gust_rtma_pct", "min_wind_filled_gust_rtma_pct",
                       "max_temp_rtma_pct", "min_temp_rtma_pct", 
                       "max_rh_rtma_pct", "min_rh_rtma_pct", "max_vpd_rtma_pct", "min_vpd_rtma_pct",
                       "bi_pct", "erc_pct", "fm100_pct", "fm1000_pct",
                       "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y")
  
  fuel_lcms_change_tm01 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm01")
  fuel_lcms_change_tm02 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm02")
  fuel_lcms_change_tm03 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm03")
  fuel_lcms_change_tm04 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm04")
  fuel_lcms_change_tm05 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm05")
  
  fuel_lcms_landcover_tm01 <- paste0(c("trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren", "landcover_diversity"), "_tm01")
  
  fuel_drivers <- c("ndvi", "veg_structure_rumple", fuel_lcms_change_tm01, fuel_lcms_change_tm02, fuel_lcms_change_tm03, fuel_lcms_change_tm04, fuel_lcms_change_tm05, fuel_lcms_landcover_tm01)
  
  interacting_drivers <- c("wind_terrain_anisotropy_rtma", "wind_terrain_alignment_rtma", "min_wind_terrain_alignment_rtma", "max_wind_terrain_alignment_rtma")
  
  predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")
  
  # No NAs
  apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))
  
  # Some NAs in the RTMA data, so we'll have to drop those rows
  # # Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
  # na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]
  # 
  # predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
  # 
  # fires <-
  #   fires %>%
  #   dplyr::select(-all_of(na_cols))
  
  # Now drop all rows that have an NA in any column
  fires <- fires[complete.cases(fires), ]
  
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
  sample.fraction_vec <- c(0.5, 0.6, 0.7, 0.8)
  
  mtry_vec <- seq(from = floor(sqrt(length(predictor.variable.names))) - 3,
                  to = floor(length(predictor.variable.names) / 1.75),
                  by = 2)
  
  tune.df <- expand.grid(mtry = mtry_vec, 
                         num.trees = 500, 
                         sample.fraction = sample.fraction_vec,
                         alpha = c(0.8, 0.9, 1.0),
                         minprop = c(0, 0.1), 
                         min.node.size = c(1, 5, 10),
                         class.wgts = TRUE,
                         iter = 5
  )
  
  
  out <- pblapply(X = (1:nrow(tune.df)), FUN = function(i) {
    return(spatial_cv_tune(folds = folds,
                           mtry = tune.df$mtry[i],
                           num.trees = tune.df$num.trees[i], 
                           sample.fraction = tune.df$sample.fraction[i],
                           alpha = tune.df$alpha[i],
                           minprop = tune.df$minprop[i],
                           min.node.size = tune.df$min.node.size[i],
                           class.wgts = tune.df$class.wgts[i],
                           iter = tune.df$iter[i]))
  })
  
  out_all <- data.table::rbindlist(out)
  data.table::fwrite(x = out_all, file = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-coarse-tuning_rtma_", biome_shortname, ".csv"))
}

# We can further tune the "classification threshold" for saying something is an EWE or not
# No need to fit more models for this step, we merely adjust our classification threshold, convert the
# pseudo-probability predictions from the models already fit to crisp 0 or 1 values, then calculate the 
# model skill metrics.

# Take the spatial CV results (observation for held out fold and predictions) and calculate various model skill metrics
pbapply::pblapply(X = biome_shortnames, FUN = function(biome_shortname) {
  # biome_shortname <- "tcf"
  biome_tune <- data.table::fread(file.path("data", "out", "rf", "tuning", paste0("rf_ranger_spatial-cv-coarse-tuning_rtma_", biome_shortname, ".csv")))
  
  classification_thresh_vec <- seq(from = 0.05, to = 0.95, by = 0.05)
  biome_tune[, classification_thresh := list(classification_thresh_vec)]
  
  biome_tune <- 
    biome_tune[, unlist(classification_thresh), 
               by = list(id, o, p, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, alpha, minprop, min.node.size, class.wgts, iter)]
  
  colnames(biome_tune)[colnames(biome_tune) == "V1"] <- "classification_thresh"
  
  biome_tune[, `:=`(o_fac = factor(o, levels = c(0, 1)),
                    p_fac = factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(0, 1)))]
  
  # For ROC-AUC: Youden WJ (1950). “Index for rating diagnostic tests.” Cancer, 3(1), 32–35. doi:10.1002/1097-0142(1950)3:1<32::aid-cncr2820030106>3.0.co;2-3.
  # For PR-AUC: Davis J, Goadrich M (2006). “The relationship between precision-recall and ROC curves.” In Proceedings of the 23rd International Conference on Machine Learning. ISBN 9781595933836.
  # For PR-AUC: J. Grau, I. Grosse, and J. Keilwagen. PRROC: computing and visualizing precision-recall and receiver operating characteristic curves in R. Bioinformatics, 31(15):2595-2597, 2015.
  # For MCC: Chicco, D., & Jurman, G. (2020). The advantages of the Matthews correlation coefficient (MCC) over F1 score and accuracy in binary classification evaluation. BMC Genomics, 21(1), 6. https://doi. org/10.1186/s12864-019-6413-7
  # Also for MCC: Chicco, D., Tötsch, N., & Jurman, G. (2021). The Matthews correlation coefficient (MCC) is more reliable than balanced accuracy, bookmaker informedness, and markedness in two-class confusion matrix evaluation. BioData Mining, 14, 13. https://doi.org/10.1186/s13040-021-00244-z
  metrics <- c("tp", "fp", "fn", "tn", "accuracy", "rmse", "logloss", "roc_auc", "pr_auc", "precision", "recall", "specificity", "f_meas", "informedness", "mcc")
  
  results <-
    biome_tune[, .(tp = as.numeric(length(which(o == 1 & p_fac == "1"))),
                   fp = as.numeric(length(which(o == 0 & p_fac == "1"))),
                   fn = as.numeric(length(which(o == 1 & p_fac == "0"))),
                   tn = as.numeric(length(which(o == 0 & p_fac == "0"))),
                   accuracy = mean(o_fac == p_fac),
                   rmse = sqrt(mean((o - p)^2)),
                   # logloss code from {MLmetrics} package
                   logloss = -mean(o * log(pmax(pmin(p, 1 - 1e-15), 1e-15)) + (1 - o) * log(1 - pmax(pmin(p, 1 - 1e-15), 1e-15))),
                   # roc_auc code from {mlr3measures} package
                   roc_auc = (mean(rank(p, ties.method = "average")[which(o == 1)]) - (as.numeric(length(which(o == 1))) + 1)/2) / as.numeric(length(which(o == 0))),
                   pr_auc = PRROC::pr.curve(scores.class0 = p, weights.class0 = o)[[2]]),
               by = .(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter)]
  
  results <- 
    results[, `:=`(precision = tp / (tp + fp),
                   recall = tp / (tp + fn),
                   specificity = tn / (tn + fp))]
  
  results <-
    results[, `:=`(f_meas = 2 * (precision * recall / (precision + recall)),
                   informedness = (tp / (tp + fn)) + (tn / (tn + fp)) - 1,
                   mcc = ((tp * tn) - (fn * fp)) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)))]
  
  # https://stackoverflow.com/questions/35618545/applying-a-function-to-all-columns-of-a-data-table-together-with-a-group-by
  results <-
    results[, lapply(.SD, FUN = weighted.mean, w = assessment_ewe_1),
            by = .(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, iter), .SDcols = metrics]
  
  # Melt data to get to long form for next pieces
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
  results <- data.table::melt(results, 
                              id.vars = c("mtry", "num.trees", "sample.fraction", "classification_thresh", "alpha", "minprop", "min.node.size", "class.wgts", "iter"),
                              measure.vars = metrics,
                              variable.name = ".metric",
                              value.name = ".estimate")
  results <-
    results[, .(mean_estimate = mean(.estimate, na.rm = TRUE),
                sd_estimate = sd(.estimate, na.rm = TRUE),
                n = .N,
                min_estimate = min(.estimate),
                max_estimate = max(.estimate)),
            by = .(mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric)]
  
  results <-
    results[, `:=`(lwr = mean_estimate - sd_estimate,
                   upr = mean_estimate + sd_estimate,
                   biome = biome_shortname)]
  
  data.table::fwrite(x = results, file = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, ".csv"))
})

# Look at the coarse tuning results and see how we can fine tune the hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-coarse-tuning-metrics_rtma_", biome_shortname, ".csv"))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)

tuning_metrics %>%
  filter(.metric == "f_meas") %>% 
  group_by(biome) %>% 
  arrange(desc(mean_estimate)) %>% 
  slice(1:5) %>% 
  print(n = 20)


