library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
# library(cpi)
library(rsample)
library(dplyr)
library(data.table)
# remotes::install_github("kormama1/seqknockoff")
library(seqknockoff)
library(BEST)

cpi <- function(task, learner, 
                resampling = NULL,
                test_data = NULL,
                measure = NULL,
                test = "t",
                log = FALSE,
                B = 1999,
                alpha = 0.05, 
                x_tilde = NULL,
                knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
                groups = NULL,
                verbose = FALSE) {
  
  # Set verbose level (and save old state)
  old_logger_treshold <- lgr::get_logger("mlr3")$threshold
  if (verbose) {
    lgr::get_logger("mlr3")$set_threshold("info")
  } else {
    lgr::get_logger("mlr3")$set_threshold("warn")
  }
  
  if (is.null(measure)) {
    if (task$task_type == "regr") {
      measure <- msr("regr.mse")
    } else if (task$task_type == "classif") {
      measure <- msr("classif.logloss")
    } else {
      stop("Unknown task type.")
    }
  }
  if (is.character(measure)) {
    measure <- msr(measure)
  }
  
  if (!(measure$id %in% c("regr.mse", "regr.mae", "classif.ce", "classif.logloss", "classif.bbrier"))) {
    stop("Currently only implemented for 'regr.mse', 'regr.mae', 'classif.ce', 'classif.logloss' and 'classif.bbrier' measures.")
  }
  if (!(test %in% c("t", "fisher", "bayes", "wilcox", "binom"))) {
    stop("Unknown test in 'test' argument.")
  }
  if (test == "bayes") {
    if (!requireNamespace("BEST", quietly = TRUE)) {
      stop("Package \"BEST\" needed for Bayesian testing. Please install it.",
           call. = FALSE)
    }
  }
  
  if (task$task_type == "classif" & measure$id %in% c("classif.logloss", "classif.bbrier")) {
    if (learner$predict_type != "prob") {
      stop("The selected loss function requires probability support. Try predict_type = 'prob' when creating the learner.")
    }
  }
  
  # Check group argument
  if (!is.null(groups)) {
    if (!is.list(groups)) {
      stop("Argument 'groups' is expected to be a (named) list with feature numbers, see examples.")
    }
    if (max(unlist(groups)) > length(task$feature_names) | any(unlist(groups) < 1)) {
      stop("Feature numbers in argument 'groups' not in 1:p, where p is the number of features.")
    }
  }
  
  # Check knockoffs
  if (any(task$feature_types$type == "factor") && is.null(x_tilde) && is.function(knockoff_fun) && deparse(knockoff_fun)[2] == "knockoff::create.second_order(as.matrix(x))") {
    stop("Gaussian knockoffs cannot handle factor features. Consider using sequential knockoffs (see examples) or recoding factors.")
  }
  
  # Create resampling instance
  if (is.null(resampling)) {
    if (is.null(test_data)) {
      stop("Either resampling or test_data argument required.")
    }
  } else if (inherits(resampling, "Resampling")) {
    resampling$instantiate(task)
  } else if (resampling %in% c("none", "oob")) {
    # Do nothing
  } else {
    stop("Unknown resampling value.")
  }
  
  # Fit learner and compute performance
  fit_full <- fit_learner(learner = learner, task = task, resampling = resampling, 
                          measure = measure, test_data = test_data, verbose = verbose)
  pred_full <- predict_learner(fit_full, task, resampling = resampling, test_data = test_data)
  err_full <- compute_loss(pred_full, measure)
  
  # Generate knockoff data
  if (is.null(x_tilde)) {
    if (is.null(test_data)) {
      x_tilde <- knockoff_fun(task$data(cols = task$feature_names))
    } else {
      test_data_x_tilde <- knockoff_fun(test_data[, task$feature_names])
    }
  } else if (is.matrix(x_tilde) | is.data.frame(x_tilde)) {
    if (is.null(test_data)) {
      if (any(dim(x_tilde) != dim(task$data(cols = task$feature_names)))) {
        stop("Size of 'x_tilde' must match dimensions of data.")
      }
    } else {
      if (any(dim(x_tilde) != dim(test_data[, task$feature_names]))) {
        stop("Size of 'x_tilde' must match dimensions of data.")
      }
      test_data_x_tilde <- x_tilde
    }
  } else {
    stop("Argument 'x_tilde' must be a matrix, data.frame or NULL.")
  }
  
  # For each feature, fit reduced model and return difference in error
  cpi_fun <- function(i) {
    if (is.null(test_data)) {
      reduced_test_data <- NULL
      reduced_data <- as.data.frame(task$data())
      reduced_data[, task$feature_names[i]] <- x_tilde[, task$feature_names[i]]
      if (task$task_type == "regr") {
        reduced_task <- mlr3::as_task_regr(reduced_data, target = task$target_names)
      } else if (task$task_type == "classif") {
        reduced_task <- mlr3::as_task_classif(reduced_data, target = task$target_names)
      } else {
        stop("Unknown task type.")
      }
    } else {
      reduced_task <- NULL
      reduced_test_data <- test_data
      reduced_test_data[, task$feature_names[i]] <- test_data_x_tilde[, task$feature_names[i]]
    }
    
    # Predict with knockoff data
    pred_reduced <- predict_learner(fit_full, reduced_task, resampling = resampling, test_data = reduced_test_data)
    err_reduced <- compute_loss(pred_reduced, measure)
    if (log) {
      dif <- log(err_reduced / err_full)
    } else {
      dif <- err_reduced - err_full
    }
    cpi <- mean(dif, na.rm = TRUE)
    se <- sd(dif, na.rm = TRUE) / sqrt(length(which(!is.na(dif))))
    
    if (is.null(groups)) {
      res <- data.frame(Variable = task$feature_names[i],
                        CPI = unname(cpi), 
                        SE = unname(se),
                        test = unname(test),
                        stringsAsFactors = FALSE)
    } else {
      res <- data.frame(Group = paste(i, collapse = ","),
                        CPI = unname(cpi), 
                        SE = unname(se),
                        test = unname(test),
                        stringsAsFactors = FALSE)
    }
    
    # Statistical testing
    if (cpi == 0) {
      # No test if CPI==0
      if (test != "bayes") {
        if (test %in% c('t', 'wilcox', 'binom')) {
          res$statistic <- 0
          res$estimate <- 0
        }
        res$p.value <- 1
        res$ci.lo <- 0
      }
    } else if (test == "fisher") {
      orig_mean <- mean(dif)
      # B permutations
      perm_means <- replicate(B, {
        signs <- sample(c(-1, 1), length(dif), replace = TRUE)
        mean(signs * dif)
      })
      res$p.value <- (sum(perm_means >= orig_mean) + 1)/(B + 1)
      res$ci.lo <- orig_mean - quantile(perm_means, 1 - alpha)
    } else if (test == "bayes") {
      res <- list(BEST::BESTmcmc(dif, parallel = FALSE, verbose = FALSE))
      names(res) <- task$feature_names[i]
    } else if (test %in% c('t', 'wilcox', 'binom')) {
      if (test == "t") {
        test_result <- t.test(dif, alternative = 'greater', 
                              conf.level = 1 - alpha)
      } else if (test == "wilcox") {
        test_result <- wilcox.test(dif, alternative = 'greater', conf.int = TRUE,
                                   conf.level = 1 - alpha)
      } else if (test == "binom") {
        test_result <- binom.test(sum(dif > 0), length(dif), alternative = 'greater', 
                                  conf.level = 1 - alpha)
      }
      res$statistic <- test_result$statistic
      res$estimate <- test_result$estimate
      res$p.value <- test_result$p.value
      res$ci.lo <- test_result$conf.int[1]
    } else {
      stop("Unknown test.")
    }
    res
  }
  
  # Different return value for Bayesian testing
  if (test == "bayes") {
    .combine = c
  } else {
    .combine = rbind
  }
  
  # If group CPI, iterate over groups
  if (is.null(groups)) {
    idx <- seq_len(length(task$feature_names))
  } else {
    idx <- groups
  }
  
  # Run in parallel if a parallel backend is registered
  j <- NULL
  if (foreach::getDoParRegistered()) {
    ret <- foreach(j = idx, .combine = .combine) %dopar% cpi_fun(j)
  } else {
    ret <- foreach(j = idx, .combine = .combine) %do% cpi_fun(j)
  }
  
  # If group CPI, rename groups
  if (!is.null(groups) & !is.null(names(groups))) {
    ret$Group <- names(groups)
  }
  
  # Reset to old logging threshold
  lgr::get_logger("mlr3")$set_threshold(old_logger_treshold)
  
  # Return CPI for all features/groups
  ret
}

fit_learner <- function(learner, task, resampling = NULL, measure = NULL, test_data = NULL, verbose = FALSE) {
  if (!is.null(test_data)) {
    # Compute error on test data
    mod <- learner$train(task)
  } else if (inherits(resampling, "Resampling")) {
    # Full model resampling
    mod <- resample(task, learner, resampling, store_models = TRUE)
  } else if (is.character(resampling) && resampling %in% c("none", "oob")) {
    # Compute error on training data
    mod <- learner$train(task)
  } else {
    stop("Unknown value for 'resampling'.")
  }
  mod
}

# Internal function to predict and compute prediction error
predict_learner <- function(mod, task, resampling = NULL, test_data = NULL) {
  if (!is.null(test_data)) {
    # Compute error on test data
    pred <- mod$predict_newdata(test_data)
  } else if (inherits(resampling, "Resampling")) {
    # Full model resampling
    pred <- lapply(seq_along(mod$learners), function(i) {
      mod$learners[[i]]$predict(task, row_ids = resampling$test_set(i))
    })
  } else if (resampling == "none") {
    # Compute error on training data
    pred <- mod$predict_newdata(task$data())
  } else if (resampling == "oob") {
    # Use OOB predictions if available
    if (inherits(mod$model, "ranger")) {
      # ranger
      # In-sample predictions will be overriden below
      pred_data <- as.data.table(mod$predict_newdata(task$data()))
      if (is.null(mod$model$inbag.counts)) {
        stop("No inbag information available. Set 'keep.inbag = TRUE' in ranger.")
      }
      preds <- predict(mod$model, task$data(), predict.all = TRUE)$predictions
      oob_idx <- ifelse(simplify2array(mod$model$inbag.counts) == 0, TRUE, NA)
      if (length(dim(preds)) == 3) {
        # Probability forest
        for (i in 1:dim(preds)[2]) {
          preds[, i, ] <- oob_idx * preds[, i, ]
        }
        y_hat <- apply(preds, 1:2, mean, na.rm = TRUE)
        colnames(y_hat) <- mod$model$forest$levels[mod$model$forest$class.values]
        pred_data[, paste("prob", colnames(y_hat), sep = ".")] <- y_hat
        pred_data$response <- factor(colnames(y_hat)[max.col(y_hat)], 
                                     levels = levels(pred_data$response))
        pred <- as_prediction_classif(pred_data)
      } else if (mod$model$treetype == "Classification") {
        # Classification forest
        apply(oob_idx * preds, 1, which.max)
        y_hat <- apply(oob_idx * preds, 1, function(x) {
          which.max(table(x, useNA = "no"))
        })
        y_hat <- mod$model$forest$levels[y_hat]
        y_hat <- factor(y_hat, levels = mode$model$forest$levels)
        pred_data$response <- y_hat
        pred <- as_prediction_classif(pred_data)
      } else {
        # Regression forest
        y_hat <- rowMeans(oob_idx * preds, na.rm = TRUE)
        pred_data$response <- y_hat
        pred <- as_prediction_regr(pred_data)
      }
    } else {
      stop("OOB error not available for this learner.")
    }
  } else {
    stop("Unknown value for 'resampling'.")
  }
  pred
}


compute_loss <- function(pred, measure) {
  if (inherits(pred, "Prediction")) {
    truth <- pred$truth
    response <- pred$response
    prob <- pred$prob
  } else {
    truth <- do.call(c, lapply(pred, function(x) x$truth))
    response <- do.call(c, lapply(pred, function(x) x$response))
    prob <- do.call(rbind, lapply(pred, function(x) x$prob))
  }
  
  if (measure$id == "regr.mse") {
    # Squared errors
    loss <- (truth - response)^2
  } else if (measure$id == "regr.mae") {
    # Absolute errors
    loss <- abs(truth - response)
  } else if (measure$id == "classif.logloss") {
    # Logloss 
    eps <- 1e-15
    ii <- match(as.character(truth), colnames(prob))
    p <- prob[cbind(seq_len(nrow(prob)), ii)]
    p <- pmax(eps, pmin(1 - eps, p))
    loss <- -log(p)
  } else if (measure$id == "classif.ce") {
    # Misclassification error
    loss <- 1*(truth != response)
  } else if (measure$id == "classif.bbrier") {
    # Brier score
    # First level is positive class
    y <- as.numeric(as.numeric(truth) == 1)
    loss <- (y - prob[, 1])^2
  } else {
    stop("Unknown measure.")
  }
  
  loss
}


biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Tuned hyperparameters
# Look at the coarse tuning results and see how we can fine tune the hyperparameters
fine_tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/rf_ranger_spatial-cv-fine-tuning-metrics_", biome_shortname, ".csv"))
})

fine_tuning_metrics <- data.table::rbindlist(fine_tuning_metrics_l)

tuned_hyperparameters <-
  fine_tuning_metrics %>% 
  filter(.metric == "f_meas") %>% 
  group_by(biome) %>% 
  arrange(desc(lwr)) %>% 
  slice(1)

# Get data
analysis_ready_nonspatial_version <- "v1"
analysis_ready_nonspatial_fname <- paste0("data/out/analysis-ready/FIRED-daily-scale-drivers_california_", analysis_ready_nonspatial_version, ".csv")

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

counter = 1

for (counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  
  biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]
  
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
  
  fires <-
    fires %>%
    dplyr::select(-all_of(zero_variance_columns))
  
  predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
  
  data <- fires[, c("did", "ewe", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
  # data$npl <- factor(data$npl, levels = 1:5)
  
  data$npl <- as.numeric(data$npl)
  data$concurrent_fires <- as.numeric(data$concurrent_fires)
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # # This is the core model that we will want to fit, which has been tuned
  split_data <- rsample::initial_split(data = data)
  analysis_data <- rsample::analysis(split_data)
  assessment_data <- rsample::assessment(split_data)

  class.wgts <- 1 / table(analysis_data$ewe)

  # fitted_model <- ranger::ranger(formula = rf_formula,
  #                                data = analysis_data[, c("ewe", predictor.variable.names)],
  #                                mtry = biome_tuned_hyperparameters$mtry,
  #                                num.trees = 1,
  #                                sample.fraction = biome_tuned_hyperparameters$sample.fraction,
  #                                replace = FALSE,
  #                                splitrule = "maxstat",
  #                                alpha = biome_tuned_hyperparameters$alpha,
  #                                minprop = biome_tuned_hyperparameters$minprop,
  #                                min.node.size = biome_tuned_hyperparameters$min.node.size,
  #                                # case.weights = as.numeric(class.wgts[analysis_data$ewe + 1]),
  #                                seed = 1,
  #                                keep.inbag = TRUE)
  
  # 
  # model_check_ranger <- 
  #   data.frame(o = assessment_data$ewe, 
  #              p = predict(object = fitted_model, 
  #                          data = assessment_data, 
  #                          type = "response")$predictions) %>% 
  #   mutate(o_fac = factor(o, levels = 0:1),
  #          p_fac = factor(ifelse(p > 0.5, yes = 1, no = 0), levels = 0:1))
  # 
  # yardstick::f_meas(model_check_ranger, truth = o_fac, estimate = p_fac)
  # yardstick::precision(model_check_ranger, truth = o_fac, estimate = p_fac)
  # yardstick::recall(model_check_ranger, truth = o_fac, estimate = p_fac)
  # yardstick::specificity(model_check_ranger, truth = o_fac, estimate = p_fac)
  # yardstick::accuracy(model_check_ranger, truth = o_fac, estimate = p_fac)
  
  # mlr3 approach
  
  class.wgts <- 1 / table(data$ewe)
  
  # https://github.com/mlr-org/mlr3/issues/563
  data$wt <- as.numeric(class.wgts[data$ewe + 1])
  
  # Define the learner to be a {ranger} regression and give it the tuned hyperparameters
  learner_ewe <- mlr3::lrn("regr.ranger", 
                           mtry = biome_tuned_hyperparameters$mtry,
                           num.trees = biome_tuned_hyperparameters$num.trees,
                           sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                           replace = FALSE,
                           splitrule = "maxstat",
                           alpha = biome_tuned_hyperparameters$alpha,
                           minprop = biome_tuned_hyperparameters$minprop,
                           min.node.size = biome_tuned_hyperparameters$min.node.size,
                           num.threads = 11,
                           keep.inbag = TRUE)
  
  # if (biome_shortname != "tgss") {
    # Define the "task" as a regression problem called "ewe" with "ewe" as the target (i.e., response)
    # only include relevant columns! That is, the response, weight column, grouping column, and the features
    task_ewe <- mlr3::as_task_regr(data[, c("ewe", "wt", predictor.variable.names)], 
                                   target = "ewe", 
                                   id = "ewe")
    
    # weight column is the "wt" column, to help with class imbalance
    task_ewe$set_col_roles(cols = "wt", roles = "weight")
    
    # # Define the "task" as a regression problem called "ewe" with "ewe" as the target (i.e., response)
    # # only include relevant columns! That is, the response, weight column, grouping column, and the features
    # task_ewe <- mlr3::as_task_regr(data[, c("ewe", "wt", "eco_name_daily", predictor.variable.names)],
    #                                target = "ewe",
    #                                id = "ewe")
    # 
    # # weight column is the "wt" column, to help with class imbalance
    # task_ewe$set_col_roles(cols = "wt", roles = "weight")
    # # eco_name_daily becomes a grouping column for the spatial cross validation
    # task_ewe$set_col_roles(cols = "eco_name_daily", roles = "group")
    # 
    # Next we build a resampler (we want to continue doing spatial cross validation to check on our variable importance)
    # resampler_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = 10)
    
    # # Next we can train the model
    # learner_ewe$train(task = task_ewe)
    
    # # Here's how we can make sure our resampler is working right
    # spat_cv_ewe <- 
    #   mlr3::resample(
    #     task = task_ewe, learner = learner_ewe,
    #     resampling = resampler_ewe)
    # 
    # spat_cv_ewe$aggregate(measures = msr("regr.rmse"))
    
    
  # } else if(biome_shortname == "tgss") {
  #   # match the spatial folds to the ones from {spatialsample} that we used for tuning
  #   folds <-
  #     data %>%
  #     sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
  #     spatialsample::spatial_clustering_cv(v = 5) %>% 
  #     purrr::pmap(.f = function(id, splits) {
  #       data.frame(did = rsample::assessment(splits)$did, spatial_fold = id)
  #     }) %>% 
  #     data.table::rbindlist()
  #   
  #   data <- merge(x = data, y = folds, by = "did")
  #   # first define the "task" as a regression problem called "ewe" with "ewe" as the target (i.e., response)
  #   # only include relevant columns! That is, the response, weight column, grouping column, and the features
  #   task_ewe <- mlr3::as_task_regr(data[, c("ewe", "wt", "spatial_fold", predictor.variable.names)], 
  #                                  target = "ewe", 
  #                                  id = "ewe")
  #   
  #   # weight column is the "wt" column, to help with class imbalance
  #   task_ewe$set_col_roles(cols = "wt", roles = "weight")
  #   # eco_name_daily becomes a grouping column for the spatial cross validation
  #   task_ewe$set_col_roles(cols = "spatial_fold", roles = "group")
  #   
  #   # Here's how we would build our task in the typical {mlr3} fashion if we weren't trying to mimic the spatial 
  #   # clustering of {spatialsample}
  #   
  #   # task_ewe <- mlr3spatiotempcv::as_task_regr_st(data[, c("ewe", "wt", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)], 
  #   #                                   target = "ewe", 
  #   #                                   id = "ewe",
  #   #                                   coordinate_names = c("x_biggest_poly_3310", "y_biggest_poly_3310"),
  #   #                                   crs = sf::st_crs(3310),
  #   #                                   coords_as_features = FALSE)
  #   
  #   
  #   # Next we build a resampler (we want to continue doing spatial cross validation to check on our variable importance)
  #   # A key difference for TGSS biome is that we want to use the coordinates themselves
  #   # Here's how we do it typically in {mlr3}
  #   # resampler_ewe <- mlr3::rsmp(.key = "repeated_spcv_coords", folds = 5, repeats = 10)
  #   
  #   # But we want to use the same spatial fold generating method that we used to tune the model (i.e., from {spatialsample})
  #   resampler_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$spatial_fold)), repeats = 10)
  #   
  # }
  
  # Here's what we're really after, since we already have a tuned model and spatially cross-validated performance/skill metrics
  cl <- parallel::makeCluster(parallel::detectCores() - 1)  
  doParallel::registerDoParallel(cl)  
  
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_fisher <- cpi::cpi(task = task_ewe,
  #                        learner = learner_ewe,
  #                        knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                        measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                        resampling = "oob",
  #                        test = "fisher", # nonparametric significance test
  #                        B = 1999)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  
  out_fisher <- cpi::cpi(task = task_ewe,
                         learner = learner_ewe,
                         knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
                         measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                         resampling = "oob",
                         test = "fisher", # nonparametric significance test
                         B = 1999)
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  ###
  
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  # out_grouped_fisher <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                                measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = "oob",
  #                                test = "fisher", # nonparametric significance test
  #                                B = 1999)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  out_grouped_fisher <- cpi::cpi(task = task_ewe,
                                 learner = learner_ewe,
                                 knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
                                 measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                                 groups = list(human = which(task_ewe$feature_names %in% human_drivers),
                                               topography = which(task_ewe$feature_names %in% topography_drivers),
                                               weather = which(task_ewe$feature_names %in% weather_drivers),
                                               fuel = which(task_ewe$feature_names %in% fuel_drivers),
                                               wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
                                               fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
                                 resampling = "oob",
                                 test = "fisher", # nonparametric significance test
                                 B = 1999)
  
  (end_time <- Sys.time())
  print(paste0("Finished grouped conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  parallel::stopCluster(cl = cl)
  
  data.table::fwrite(x = out_fisher, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_oob-classif-accuracy_fisher_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped_fisher, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_oob-classif-accuracy_grouped_fisher_", biome_shortname, ".csv"))

}

# Warning message during cpi() run for dxs biome: 
# Warning message:
# from glmnet C++ code (error code -96); Convergence for 96th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned