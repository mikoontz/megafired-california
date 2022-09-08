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

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Tuned hyperparameters
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
  data$npl <- factor(data$npl, levels = 1:5)
  
  # data$npl <- as.numeric(data$npl)
  data$concurrent_fires <- as.numeric(data$concurrent_fires)
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # # This is the core model that we will want to fit, which has been tuned
  # split_data <- rsample::initial_split(data = data)
  # analysis_data <- rsample::analysis(split_data)
  # assessment_data <- rsample::assessment(split_data)
  # 
  # class.wgts <- 1 / table(analysis_data$ewe)
  # 
  # fitted_model <- ranger::ranger(formula = rf_formula,
  #                                data = analysis_data[, c("ewe", predictor.variable.names)],
  #                                mtry = biome_tuned_hyperparameters$mtry,
  #                                num.trees = biome_tuned_hyperparameters$num.trees,
  #                                sample.fraction = biome_tuned_hyperparameters$sample.fraction,
  #                                replace = FALSE,
  #                                splitrule = "maxstat",
  #                                alpha = biome_tuned_hyperparameters$alpha,
  #                                minprop = biome_tuned_hyperparameters$minprop,
  #                                min.node.size = biome_tuned_hyperparameters$min.node.size,
  #                                case.weights = as.numeric(class.wgts[analysis_data$ewe + 1]),
  #                                seed = 1)
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
  
  # number of repeats of the spatial cross validation to account for stochastic nature of the model fitting
  n_repeats <- 10
  
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
                           num.threads = 1,
                           keep.inbag = TRUE)
  
  if (biome_shortname != "tgss") {
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
    resampling_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = n_repeats)
    
    # # Next we can train the model
    # learner_ewe$train(task = task_ewe)
    
    # # Here's how we can make sure our resampler is working right
    # spat_cv_ewe <- 
    #   mlr3::resample(
    #     task = task_ewe, learner = learner_ewe,
    #     resampling = resampling_ewe)
    # 
    # spat_cv_ewe$aggregate(measures = msr("regr.rmse"))
    
    
  } else if(biome_shortname == "tgss") {
    # match the spatial folds to the ones from {spatialsample} that we used for tuning
    folds <-
      data %>%
      sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310) %>%
      spatialsample::spatial_clustering_cv(v = 5) %>% 
      purrr::pmap(.f = function(id, splits) {
        data.frame(did = rsample::assessment(splits)$did, spatial_fold = id)
      }) %>% 
      data.table::rbindlist()
    
    data <- merge(x = data, y = folds, by = "did")
    # first define the "task" as a regression problem called "ewe" with "ewe" as the target (i.e., response)
    # only include relevant columns! That is, the response, weight column, grouping column, and the features
    task_ewe <- mlr3::as_task_regr(data[, c("ewe", "wt", "spatial_fold", predictor.variable.names)], 
                                   target = "ewe", 
                                   id = "ewe")
    
    # weight column is the "wt" column, to help with class imbalance
    task_ewe$set_col_roles(cols = "wt", roles = "weight")
    # eco_name_daily becomes a grouping column for the spatial cross validation
    task_ewe$set_col_roles(cols = "spatial_fold", roles = "group")
    
    # Here's how we would build our task in the typical {mlr3} fashion if we weren't trying to mimic the spatial 
    # clustering of {spatialsample}
    
    # task_ewe <- mlr3spatiotempcv::as_task_regr_st(data[, c("ewe", "wt", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)], 
    #                                   target = "ewe", 
    #                                   id = "ewe",
    #                                   coordinate_names = c("x_biggest_poly_3310", "y_biggest_poly_3310"),
    #                                   crs = sf::st_crs(3310),
    #                                   coords_as_features = FALSE)
    
    
    # Next we build a resampler (we want to continue doing spatial cross validation to check on our variable importance)
    # A key difference for TGSS biome is that we want to use the coordinates themselves
    # Here's how we do it typically in {mlr3}
    # resampling_ewe <- mlr3::rsmp(.key = "repeated_spcv_coords", folds = 5, repeats = 10)
    
    # But we want to use the same spatial fold generating method that we used to tune the model (i.e., from {spatialsample})
    resampling_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$spatial_fold)), repeats = n_repeats)
    
  }
  
  # Here's what we're really after, since we already have a tuned model and spatially cross-validated performance/skill metrics
  cl <- parallel::makeCluster(parallel::detectCores() - 1)  
  doParallel::registerDoParallel(cl)  
  
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional predictive impact calculations based on the F score for ", biome_shortname, " at ", start_time, "..."))
  
  # When we use the fbeta metric, it's just a single value from the whole observed/predicted set, rather than a value for
  # each observation, like with the other metrics. So the statistical tests implemented in {cpi} are useless here. But
  # because there isn't an option to just not do a statistical test, we'll use the fisher test with only 1 replication to
  # speed things up.
  out_fbeta <- cpi::cpi(task = task_ewe,
                        learner = learner_ewe,
                        knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                        measure = "classif.fbeta",
                        resampling = resampling_ewe,
                        test = "fisher",
                        B = 1,
                        response_is_prob = TRUE)
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional predictive impact calculations based on the F score for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations based on the logloss metric with a fisher test for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_fisher <- cpi::cpi(task = task_ewe,
  #                        learner = learner_ewe,
  #                        knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                        measure = "classif.logloss", # we'll use the classification error as the best measure for our problem
  #                        resampling = resampling_ewe,
  #                        test = "fisher", # nonparametric significance test
  #                        B = 1999, 
  #                        response_is_prob = TRUE)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations based on the logloss metric with a fisher test for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # out_fisher %>% arrange(desc(CPI)) %>% filter(p.value <= 0.05)
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_wilcox <- cpi::cpi(task = task_ewe,
  #                        learner = learner_ewe,
  #                        knockoff_fun = seqknockoff::knockoffs_seq,
  #                        measure = "classif.logloss", # we'll use the classification error as the best measure for our problem
  #                        resampling = resampling_ewe,
  #                        test = "wilcox")
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations (t test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_t <- cpi::cpi(task = task_ewe, 
  #                   learner = learner_ewe, 
  #                   knockoff_fun = seqknockoff::knockoffs_seq,
  #                   measure = "classif.logloss", # we'll use the log loss error as the best measure for our problem
  #                   resampling = resampling_ewe,
  #                   test = "t",
  #                   response_is_prob = TRUE)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations (t test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations (binomial test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_t <- cpi::cpi(task = task_ewe, 
  #                   learner = learner_ewe, 
  #                   knockoff_fun = seqknockoff::knockoffs_seq,
  #                   measure = "classif.logloss", # we'll use the log loss error as the best measure for our problem
  #                   resampling = resampling_ewe,
  #                   test = "binomial",
  #                   response_is_prob = TRUE)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations (binomial test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional predictive impact calculations based on the F score for ", biome_shortname, " at ", start_time, "..."))
  out_grouped_fbeta <- cpi::cpi(task = task_ewe,
                                learner = learner_ewe,
                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor,
                                measure = "classif.fbeta", # we'll use the classification error as the best measure for our problem
                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
                                              topography = which(task_ewe$feature_names %in% topography_drivers),
                                              weather = which(task_ewe$feature_names %in% weather_drivers),
                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
                                resampling = resampling_ewe,
                                test = "fisher", # nonparametric significance test
                                B = 1,
                                response_is_prob = TRUE)
  
  (end_time <- Sys.time())
  print(paste0("Finished grouped conditional predictive impact calculations based on the F score for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  # out_grouped_fisher <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
  #                                measure = "classif.logloss", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = resampling_ewe,
  #                                test = "fisher", # nonparametric significance test
  #                                B = 1999,
  #                                response_is_prob = TRUE)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional predictive impact calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_grouped_wilcox <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                                measure = "classif.logloss", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = resampling_ewe,
  #                                test = "wilcox",
  #                                response_is_prob = TRUE)
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (t test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_grouped_t <- cpi::cpi(task = task_ewe,
  #                           learner = learner_ewe,
  #                           knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                           measure = "classif.logloss", # we'll use the classification error as the best measure for our problem
  #                           groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                         topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                         weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                         fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                         wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                         fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                           resampling = resampling_ewe,
  #                           test = "t",
  #                           response_is_prob = TRUE)
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional predictive impact calculations (t test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (binomial test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_grouped_binomial <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                                measure = "classif.logloss", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = resampling_ewe,
  #                                test = "binomial",
  #                                response_is_prob = TRUE)
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional predictive impact calculations (binomial test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  parallel::stopCluster(cl = cl)
  
  data.table::fwrite(x = out_fbeta, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-fbeta_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped_fbeta, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-fbeta_grouped_", biome_shortname, ".csv"))
  
  # data.table::fwrite(x = out_fisher, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_fisher_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_wilcox, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_wilcox_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_t, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_t_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_binomial, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_binomial_", biome_shortname, ".csv"))
  
  # data.table::fwrite(x = out_grouped_fisher, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_grouped_fisher_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_wilcox, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_grouped_wilcox_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_t, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_grouped_t_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_binomial, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-logloss_grouped_binomial_", biome_shortname, ".csv"))
  
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_fisher <- cpi::cpi(task = task_ewe,
  #                        learner = learner_ewe,
  #                        knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                        measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                        resampling = resampling_ewe,
  #                        test = "fisher", # nonparametric significance test
  #                        B = 1999)
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_wilcox <- cpi::cpi(task = task_ewe,
  #                        learner = learner_ewe,
  #                        knockoff_fun = seqknockoff::knockoffs_seq,
  #                        measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                        resampling = resampling_ewe,
  #                        test = "wilcox")
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ##
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional predictive impact calculations (t test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_t <- cpi::cpi(task = task_ewe, 
  #                        learner = learner_ewe, 
  #                        knockoff_fun = seqknockoff::knockoffs_seq,
  #                        measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                        resampling = resampling_ewe,
  #                        test = "t")
  # 
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations (t test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  # out_grouped_fisher <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
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
  # print(paste0("Finished grouped conditional predictive impact calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_grouped_wilcox <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                                measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = resampling_ewe,
  #                                test = "wilcox")
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional predictive impact calculations (wilcox test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # ###
  # 
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional predictive impact calculations (t test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_grouped_t <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                                measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = resampling_ewe,
  #                                test = "t")
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional predictive impact calculations (t test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  # 
  # parallel::stopCluster(cl = cl)
  # 
  # data.table::fwrite(x = out_fisher, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_fisher_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_wilcox, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_wilcox_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_t, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_t_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_fisher, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_fisher_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_wilcox, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_wilcox_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_t, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_t_", biome_shortname, ".csv"))
  # 
}

# Warning message during cpi() run for dxs biome: 
# Warning message:
# from glmnet C++ code (error code -96); Convergence for 96th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned