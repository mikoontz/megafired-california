library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
# detach("package:cpi", unload=TRUE)
# remove.packages("cpi")
# remotes::install_github(repo = "mikoontz/cpi") # install from my patch
# install.packages("cpi") # install from CRAN
library(cpi)
library(rsample)
library(dplyr)
library(data.table)
# remotes::install_github("kormama1/seqknockoff")
library(seqknockoff)
library(foreach)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Tuned hyperparameters
# fine_tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
#   data.table::fread(input = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-fine-tuning-metrics_", biome_shortname, ".csv"))
# })
# 
# fine_tuning_metrics <- data.table::rbindlist(fine_tuning_metrics_l)
biome_shortname <- "tcf"
fine_tuning_metrics <- data.table::fread(input = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, ".csv"))

tuned_hyperparameters <-
  fine_tuning_metrics %>% 
  filter(.metric == "informedness") %>% 
  group_by(biome) %>% 
  arrange(desc(lwr)) %>% 
  slice(1)

# Get data
analysis_ready_nonspatial_version <- "v2"
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

# Subset to just years where there are RTMA data (2011 and later)
fires_all <-
  fires_all %>%
  dplyr::filter(date >= as.Date("2011-01-01"))

# drop ERA5 columns in favor of RTMA columns for the weather variables
fires_all <-
  fires_all %>% 
  dplyr::select(-contains("era5"))

for (counter in (1:4)) {
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
  
  # Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
  na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]
  
  predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
  
  fires <-
    fires %>%
    dplyr::select(-all_of(na_cols))
  
  # # Now drop all fires that have an NA in any column
  # bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])
  # fires <- fires[!(fires$id %in% bad_fires), ]
  
  # Now drop all rows that have an NA in any column
  fires <- fires[complete.cases(fires), ]
  
  # no columnns with 0 variance (rounded to 4 decimal places)
  zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]
  
  predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
  
  fires <-
    fires %>%
    dplyr::select(-all_of(zero_variance_columns))
  
  data <- fires[, c("did", "ewe", "biome_name_daily", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
  data$npl <- factor(data$npl, levels = 1:5)
  data$concurrent_fires <- as.numeric(data$concurrent_fires)
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # mlr3 approach
  
  # First set up spatial folds
  # https://spatialsample.tidymodels.org/articles/spatialsample.html
  if (biome_shortname == "tgss") {
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_clustering_cv(v = 5)
    
    lookup_table <-
      folds %>% 
      purrr::pmap(.f = function(id, splits) {
        return(data.frame(spatial_fold = id, 
                          eco_name_daily = rsample::assessment(splits)$eco_name_daily, 
                          did = rsample::assessment(splits)$did))
      }) %>% 
      data.table::rbindlist()
    
  } else {
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_leave_location_out_cv(group = eco_name_daily)
    
    lookup_table <-
      folds %>% 
      purrr::pmap(.f = function(id, splits) {
        return(data.frame(spatial_fold = id,
                          eco_name_daily = rsample::assessment(splits)$eco_name_daily,
                          did = rsample::assessment(splits)$did))
      }) %>% 
      data.table::rbindlist()
  }
  
  # number of repeats of the spatial cross validation to account for stochastic nature of the model fitting
  n_repeats <- 10
  
  # repeat the spatial folds so we can re-fit the model (account for the stochastic nature of RF)
  folds_repeat <- 
    lapply(X = 1:n_repeats, FUN = function(i) {
      return(dplyr::mutate(folds, iter = i))
    }) %>% 
    do.call(what = "rbind", args = .)
  
  # Iterate through each split, using the resample::analysis(split) data as training data for the mlr3 task
  # and rsample::assessment(split) data as the test_data for the call to the cpi function 
  cpi_mcc <- function(i) {
    analysis_data <- 
      folds_repeat$splits[[i]] %>% 
      rsample::analysis() %>% 
      sf::st_drop_geometry()
    
    assessment_data <- 
      folds_repeat$splits[[i]] %>% 
      rsample::assessment() %>% 
      sf::st_drop_geometry()
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # https://github.com/mlr-org/mlr3/issues/563
    analysis_data$wt <- as.numeric(class.wgts[analysis_data$ewe + 1])
    
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
                             num.threads = 1,
                             keep.inbag = TRUE)
    
    task_ewe <- mlr3::as_task_regr(analysis_data[, c("ewe", "wt", predictor.variable.names)], 
                                   target = "ewe", 
                                   id = "ewe")
    
    # weight column is the "wt" column, to help with class imbalance
    task_ewe$set_col_roles(cols = "wt", roles = "weight")
    
    # When we use the matthews correlation coefficient metric for our model skill
    # It's just a single value from the whole observed/predicted set, rather than a value for
    # each observation, like with the other metrics. So the statistical tests implemented in {cpi} are useless here. But
    # because there isn't an option to just not do a statistical test, we'll use the fisher test with only 1 replication to
    # speed things up.
    out_mcc <- cpi::cpi(task = task_ewe,
                          learner = learner_ewe,
                          knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                          measure = "classif.mcc",
                          test_data = assessment_data,
                          test = "fisher",
                          B = 1,
                          response_is_prob = TRUE,
                          classification_thresh = biome_tuned_hyperparameters$classification_thresh)
    
    
    out <- 
      out_mcc %>% 
      dplyr::mutate(id = folds_repeat$id[i], iter = folds_repeat$iter[i], biome = biome_shortname)
    
    return(out)
  }
  
  ## Grouped version
  # Iterate through each split, using the resample::analysis(split) data as training data for the mlr3 task
  # and rsample::assessment(split) data as the test_data for the call to the cpi function 
  cpi_mcc_grouped <- function(i) {
    analysis_data <- 
      folds_repeat$splits[[i]] %>% 
      rsample::analysis() %>% 
      sf::st_drop_geometry()
    
    assessment_data <- 
      folds_repeat$splits[[i]] %>% 
      rsample::assessment() %>% 
      sf::st_drop_geometry()
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # https://github.com/mlr-org/mlr3/issues/563
    analysis_data$wt <- as.numeric(class.wgts[analysis_data$ewe + 1])
    
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
                             num.threads = 1,
                             keep.inbag = TRUE)
    
    task_ewe <- mlr3::as_task_regr(analysis_data[, c("ewe", "wt", predictor.variable.names)], 
                                   target = "ewe", 
                                   id = "ewe")
    
    # weight column is the "wt" column, to help with class imbalance
    task_ewe$set_col_roles(cols = "wt", roles = "weight")
    
    # When we use the fbeta metric, it's just a single value from the whole observed/predicted set, rather than a value for
    # each observation, like with the other metrics. So the statistical tests implemented in {cpi} are useless here. But
    # because there isn't an option to just not do a statistical test, we'll use the fisher test with only 1 replication to
    # speed things up.
    out_mcc <- cpi::cpi(task = task_ewe,
                          learner = learner_ewe,
                          knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor,
                          measure = "classif.fbeta", # we'll use the classification error as the best measure for our problem
                          groups = list(human = which(task_ewe$feature_names %in% human_drivers),
                                        topography = which(task_ewe$feature_names %in% topography_drivers),
                                        weather = which(task_ewe$feature_names %in% weather_drivers),
                                        fuel = which(task_ewe$feature_names %in% fuel_drivers),
                                        wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
                                        fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
                          test_data = assessment_data,
                          test = "fisher",
                          B = 1,
                          response_is_prob = TRUE,
                          classification_thresh = biome_tuned_hyperparameters$classification_thresh)
    
    
    out <- 
      out_mcc %>% 
      dplyr::mutate(id = folds_repeat$id[i], iter = folds_repeat$iter[i], biome = biome_shortname)
    
    return(out)
  }
  
  
  # Here's what we're really after, since we already have a tuned model and spatially cross-validated performance/skill metrics
  cl <- parallel::makeCluster(parallel::detectCores() - 1)  
  doParallel::registerDoParallel(cl)  
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", start_time, "..."))
  
  out <- 
    foreach(i = (1:nrow(folds_repeat)), 
            .combine = rbind, 
            .packages = c("dplyr", "cpi", "mlr3", "mlr3learners", "rsample", "sf"),
            .errorhandling = "remove") %dopar% 
    cpi_mcc(i)
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  # Group across folds (summarizing across iterations) to see CPI of each variable for each fold
  out %>%
    dplyr::group_by(Variable, id, biome) %>%
    dplyr::summarize(CPI = mean(CPI),
                     n = n())
  
  # Group across iterations (summarizing across folds), then group across folds (summarizing CPI per variable for whole biome)
  # in order to 
  out %>%
    dplyr::group_by(Variable, iter, biome) %>%
    dplyr::summarize(CPI = mean(CPI)) %>% 
    dplyr::group_by(Variable, biome) %>% 
    dplyr::summarize(CPI = mean(CPI)) %>% 
    dplyr::arrange(desc(CPI)) %>% 
    print(n = 30)
  
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", start_time, "..."))
  
  out_grouped <- 
    foreach(i = (1:nrow(folds_repeat)), 
            .combine = rbind, 
            .packages = c("dplyr", "cpi", "mlr3", "mlr3learners", "rsample", "sf"),
            .errorhandling = "remove") %dopar% 
    cpi_mcc_grouped(i)
  
  
  (end_time <- Sys.time())
  print(paste0("Finished grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  # Group across folds (summarizing across iterations) to see CPI of each variable for each fold
  out_grouped %>%
    dplyr::group_by(Group, id, biome) %>%
    dplyr::summarize(CPI = mean(CPI),
                     n = n())
  
  # Group across iterations (summarizing across folds), then group across folds (summarizing CPI per variable for whole biome)
  # in order to 
  out_grouped %>%
    dplyr::group_by(Group, iter, biome) %>%
    dplyr::summarize(CPI = mean(CPI)) %>% 
    dplyr::group_by(Group, biome) %>% 
    dplyr::summarize(CPI = mean(CPI)) %>% 
    dplyr::arrange(desc(CPI)) %>% 
    print(n = 30)
  
  ###
  
  parallel::stopCluster(cl = cl)
  
  data.table::fwrite(x = out, file = paste0("data/out/rf/rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped, paste0(file = "data/out/rf/rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, ".csv"))
  
}
