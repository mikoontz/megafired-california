library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3spatiotempcv)
library(dplyr)
library(purrr)
library(yardstick)
library(spatialsample)
library(sf)
library(tidyr)
library(pbapply)
library(data.table)
library(cpi)
# library(ranger)

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
# data$npl <- factor(data$npl, levels = 1:5)

class.wgts <- 1 / table(data$ewe)

# https://github.com/mlr-org/mlr3/issues/563
data$wt <- as.numeric(class.wgts[data$ewe + 1])

# Define the learner to be a {ranger} regression and give it the tuned hyperparameters
learner_ewe <- mlr3::lrn("classif.glmnet", predict_type = "prob", lambda = 0.1)

task_ewe <- mlr3::as_task_classif(x = data[, c("ewe", "eco_name_daily", predictor.variable.names)], 
                                  target = "ewe", 
                                  id = "ewe")

# weight column is the "wt" column, to help with class imbalance
task_ewe$set_col_roles(cols = "wt", roles = "weight")
# eco_name_daily becomes a grouping column for the spatial cross validation
task_ewe$set_col_roles(cols = "eco_name_daily", roles = "group")

# Next we build a resampler (we want to continue doing spatial cross validation to check on our variable importance)
resampling_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = 5)

cl <- parallel::makeCluster(parallel::detectCores() - 1)  
doParallel::registerDoParallel(cl)  

out <- cpi::cpi(task = task_ewe, learner = learner_ewe, measure = "classif.logloss", resampling = resampling_ewe, test = "fisher")

parallel::stopCluster(cl = cl)

library(glmnet)

fm1 <- glmnet::glmnet(x = as.matrix(data[, predictor.variable.names]), y = data$ewe, family = "binomial", weights = data$wt, nlambda = 200)
fm1 <- glmnet::cv.glmnet(x = as.matrix(data[, predictor.variable.names]), 
                         y = data$ewe, 
                         family = "binomial", 
                         weights = data$wt, 
                         nlambda = 100, 
                         nfolds = 5,
                         type.measure = "auc")

plot(fm1)


library(mlr3verse)
# https://stackoverflow.com/questions/66696405/how-to-repeat-hyperparameter-tuning-alpha-and-or-lambda-of-glmnet-in-mlr3

(start_time <- Sys.time())
# define train task
train.task <- mlr3::as_task_classif(x = data[, c("ewe", "wt", "eco_name_daily", predictor.variable.names)], 
                                    target = "ewe", 
                                    id = "ewe")

# weight column is the "wt" column, to help with class imbalance
train.task$set_col_roles(cols = "wt", roles = "weight")
# eco_name_daily becomes a grouping column for the spatial cross validation
train.task$set_col_roles(cols = "eco_name_daily", roles = "group")

# create elastic net regression
glmnet_lrn <- lrn("classif.glmnet", predict_type = "prob")

# turn to learner
learner <- as_learner(glmnet_lrn)

# make search space
search_space <-
  paradox::ps(
    alpha = paradox::p_dbl(lower = 0, upper = 1),
    lambda = paradox::p_dbl(lower = 0.01, upper = 100, logscale = TRUE)
  )

# Next we build a resampler (we want to continue doing spatial cross validation to check on our variable importance)
resampling_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = 5)

# tune the learner
at <- AutoTuner$new(
  learner = learner,
  resampling = resampling_ewe,
  measure = msr("classif.fbeta"),
  search_space = search_space,
  terminator = terminator,
  tuner=tuner)

at <- 
  mlr3tuning::auto_tuner(method = mlr3tuning::tnr("grid_search", resolution = 20),
                         learner = mlr3::lrn("classif.glmnet", predict_type = "prob"),
                         resampling = resampling_ewe,
                         measure = mlr3::msr("classif.logloss"),
                         search_space = search_space,
                         terminator = mlr3tuning::trm("evals", n_evals = 200))


tuned <- at$train(task = train.task)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time))

out <- coef(tuned$model$learner$model, 
            alpha = tuned$tuning_result$alpha, 
            s = exp(tuned$tuning_result$lambda))

out %>% as.matrix() %>% as.data.frame() %>% arrange(desc(abs(s1)))

tuned$tuning_result
# task_ewe <- mlr3::as_task_regr(data[, c("ewe", "wt", "eco_name_daily", predictor.variable.names)], 
#                                target = "ewe", 
#                                id = "ewe")
# 
# # weight column is the "wt" column, to help with class imbalance
# task_ewe$set_col_roles(cols = "wt", roles = "weight")
# # eco_name_daily becomes a grouping column for the spatial cross validation
# task_ewe$set_col_roles(cols = "eco_name_daily", roles = "group")


fitted_mod <- resample(task = task_ewe, learner = learner_ewe, resampling = resampler_ewe, store_models = TRUE)

parallel::stopCluster(cl = cl)





























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

for(counter in 1:4) {
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
  slice(1:5) %>% 
  print(n = 20)


