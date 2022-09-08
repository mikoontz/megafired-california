# devtools::install_github("https://github.com/clementbenard/ranger/tree/sobol-mda")
# devtools::install_gitlab(repo = "drti/sobolmda")
library(sobolMDA)
# library(mlr3)
# library(mlr3learners)
# library(mlr3pipelines)
library(ranger)
# library(cpi)
library(rsample)
library(dplyr)
library(data.table)
# remotes::install_github("kormama1/seqknockoff")
# library(seqknockoff)
# library(BEST)


# devtools::install_gitlab(repo = "drti/sobolmda")
# library(sobolMDA)
# library(ranger)

rg.iris <- ranger::ranger(Species ~ ., data = iris, importance = "impurity")
rg.iris$variable.importance

rg.iris <- sobolMDA::ranger(Species ~ ., data = iris, importance = "sobolMDA", num.trees = 10, seed = 2)
rg.iris$variable.importance


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
# for (counter in 1:4) {
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
  
  class.wgts <- 1 / table(data$ewe)
  
  (start_time <- Sys.time())
  fitted_model <- sobolMDA::ranger(formula = rf_formula,
                                 data = data[, c("ewe", predictor.variable.names)],
                                 mtry = biome_tuned_hyperparameters$mtry,
                                 num.trees = biome_tuned_hyperparameters$num.trees,
                                 sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                                 replace = FALSE,
                                 splitrule = "maxstat",
                                 importance = "sobolMDA",
                                 alpha = biome_tuned_hyperparameters$alpha,
                                 minprop = biome_tuned_hyperparameters$minprop,
                                 min.node.size = biome_tuned_hyperparameters$min.node.size,
                                 # case.weights = as.numeric(class.wgts[data$ewe + 1]),
                                 seed = 1)
  
  (end_time <- Sys.time())
  (difftime(time1 = end_time, time2 = start_time))
  
  sort(fitted_model$variable.importance, decreasing = TRUE)
  
  

  # This is the core model that we will want to fit, which has been tuned
  split_data <- rsample::initial_split(data = data)
  analysis_data <- rsample::analysis(split_data)
  assessment_data <- rsample::assessment(split_data)

  class.wgts <- 1 / table(analysis_data$ewe)

  fitted_model <- ranger::ranger(formula = rf_formula,
                                 data = analysis_data[, c("ewe", predictor.variable.names)],
                                 mtry = biome_tuned_hyperparameters$mtry,
                                 num.trees = biome_tuned_hyperparameters$num.trees,
                                 sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                                 replace = FALSE,
                                 splitrule = "maxstat",
                                 importance = "sobolMDA",
                                 alpha = biome_tuned_hyperparameters$alpha,
                                 minprop = biome_tuned_hyperparameters$minprop,
                                 min.node.size = biome_tuned_hyperparameters$min.node.size,
                                 case.weights = as.numeric(class.wgts[analysis_data$ewe + 1]),
                                 seed = 1)
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
    resampler_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = 10)
    
    # # Next we can train the model
    # learner_ewe$train(task = task_ewe)
    
    # # Here's how we can make sure our resampler is working right
    # spat_cv_ewe <- 
    #   mlr3::resample(
    #     task = task_ewe, learner = learner_ewe,
    #     resampling = resampler_ewe)
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
    # resampler_ewe <- mlr3::rsmp(.key = "repeated_spcv_coords", folds = 5, repeats = 10)
    
    # But we want to use the same spatial fold generating method that we used to tune the model (i.e., from {spatialsample})
    resampler_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$spatial_fold)), repeats = 10)
    
  }
  
  # Here's what we're really after, since we already have a tuned model and spatially cross-validated performance/skill metrics
  cl <- parallel::makeCluster(parallel::detectCores() - 1)  
  doParallel::registerDoParallel(cl)  
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  
  out_fisher <- cpi::cpi(task = task_ewe,
                         learner = learner_ewe,
                         knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                         measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                         resampling = resampler_ewe,
                         test = "fisher", # nonparametric significance test
                         B = 1999)
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional permutation importance calculations (wilcox test) for ", biome_shortname, " at ", start_time, "..."))
  
  out_wilcox <- cpi::cpi(task = task_ewe,
                         learner = learner_ewe,
                         knockoff_fun = seqknockoff::knockoffs_seq,
                         measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                         resampling = resampler_ewe,
                         test = "wilcox")
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional permutation importance calculations (wilcox test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ##
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional permutation importance calculations (t test) for ", biome_shortname, " at ", start_time, "..."))
  
  out_t <- cpi::cpi(task = task_ewe, 
                    learner = learner_ewe, 
                    knockoff_fun = seqknockoff::knockoffs_seq,
                    measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                    resampling = resampler_ewe,
                    test = "t")
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional permutation importance calculations (t test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  # (start_time <- Sys.time())
  # print(paste0("Starting conditional permutation importance calculations (bayes test) for ", biome_shortname, " at ", start_time, "..."))
  #
  # out_bayes <- cpi::cpi(task = task_ewe,
  #                        learner = learner_ewe,
  #                        knockoff_fun = seqknockoff::knockoffs_seq,
  #                        measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                        resampling = resampler_ewe,
  #                        test = "bayes")
  #
  # (end_time <- Sys.time())
  # print(paste0("Finished conditional permutation importance calculations (bayes test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ##
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional permutation importance calculations (fisher test) for ", biome_shortname, " at ", start_time, "..."))
  out_grouped_fisher <- cpi::cpi(task = task_ewe,
                                 learner = learner_ewe,
                                 knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
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
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional permutation importance calculations (wilcox test) for ", biome_shortname, " at ", start_time, "..."))
  
  out_grouped_wilcox <- cpi::cpi(task = task_ewe,
                                 learner = learner_ewe,
                                 knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                                 measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                                 groups = list(human = which(task_ewe$feature_names %in% human_drivers),
                                               topography = which(task_ewe$feature_names %in% topography_drivers),
                                               weather = which(task_ewe$feature_names %in% weather_drivers),
                                               fuel = which(task_ewe$feature_names %in% fuel_drivers),
                                               wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
                                               fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
                                 resampling = resampler_ewe,
                                 test = "wilcox")
  (end_time <- Sys.time())
  print(paste0("Finished grouped conditional permutation importance calculations (wilcox test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional permutation importance calculations (t test) for ", biome_shortname, " at ", start_time, "..."))
  
  out_grouped_t <- cpi::cpi(task = task_ewe,
                            learner = learner_ewe,
                            knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                            measure = "classif.ce", # we'll use the classification error as the best measure for our problem
                            groups = list(human = which(task_ewe$feature_names %in% human_drivers),
                                          topography = which(task_ewe$feature_names %in% topography_drivers),
                                          weather = which(task_ewe$feature_names %in% weather_drivers),
                                          fuel = which(task_ewe$feature_names %in% fuel_drivers),
                                          wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
                                          fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
                            resampling = resampler_ewe,
                            test = "t")
  (end_time <- Sys.time())
  print(paste0("Finished grouped conditional permutation importance calculations (t test) for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  # (start_time <- Sys.time())
  # print(paste0("Starting grouped conditional permutation importance calculations (bayes test) for ", biome_shortname, " at ", start_time, "..."))
  # 
  # out_grouped_bayes <- cpi::cpi(task = task_ewe,
  #                                learner = learner_ewe,
  #                                knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
  #                                measure = "classif.ce", # we'll use the classification error as the best measure for our problem
  #                                groups = list(human = which(task_ewe$feature_names %in% human_drivers),
  #                                              topography = which(task_ewe$feature_names %in% topography_drivers),
  #                                              weather = which(task_ewe$feature_names %in% weather_drivers),
  #                                              fuel = which(task_ewe$feature_names %in% fuel_drivers),
  #                                              wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
  #                                              fire = which(task_ewe$feature_names %in% "sqrt_aoi_tm1")),
  #                                resampling = resampler_ewe,
  #                                test = "bayes")
  # (end_time <- Sys.time())
  # print(paste0("Finished grouped conditional permutation importance calculations (bayes test) for ", biome_shortname, " at ", end_time, ". "))
  # print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  parallel::stopCluster(cl = cl)
  
  data.table::fwrite(x = out_fisher, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_fisher_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_wilcox, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_wilcox_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_t, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_t_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_bayes, file = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_bayes_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped_fisher, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_fisher_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped_wilcox, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_wilcox_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped_t, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_t_", biome_shortname, ".csv"))
  # data.table::fwrite(x = out_grouped_bayes, paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-accuracy_grouped_bayes_", biome_shortname, ".csv"))
  
# }

# Warning message during cpi() run for dxs biome: 
# Warning message:
# from glmnet C++ code (error code -96); Convergence for 96th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned