library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
# detach("package:cpi", unload=TRUE)
# remove.packages("cpi")
# remotes::install_github(repo = "mikoontz/cpi@megafired-california") # install from my patch
# install.packages("cpi") # install from CRAN
library(cpi)
library(rsample)
library(dplyr)
library(data.table)
# remotes::install_github("kormama1/seqknockoff")
library(seqknockoff)
library(foreach)

dir.create("data/out/rf/conditional-predictive-impact/", showWarnings = FALSE, recursive = TRUE)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_n <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- 
      read.csv(paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv")) %>% 
      dplyr::group_by(biome_shortname, eco_name_daily, spatial_fold) %>% 
      dplyr::tally() %>% 
      dplyr::arrange(spatial_fold)
  })

# Tuned hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, ".csv"))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)

tuned_hyperparameters <-
  tuning_metrics %>% 
  dplyr::filter(.metric == "informedness") %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>% 
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean_informedness = weighted.mean(x = mean, w = assessment_ewe_n, na.rm = TRUE),
            lwr_informedness = weighted.mean(x = lwr, w = assessment_ewe_n, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::arrange(desc(lwr_informedness)) %>% 
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

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_lookup_tables <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- 
      read.csv(paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv")) %>% 
      dplyr::arrange(spatial_fold)
  }) %>% 
  do.call("rbind", .)

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_n <- 
  fold_lookup_tables %>% 
  dplyr::group_by(biome_name_daily, biome_shortname, eco_name_daily, spatial_fold) %>% 
  dplyr::tally()

for (counter in (1:4)) {
  biome_shortname <- biome_shortnames[counter]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  fold_assignments <- fold_lookup_tables[fold_lookup_tables$biome_shortname == biome_shortname, ]
  
  # Any folds in this biome that have very low sample size? Spoiler: it's TCF Great Basin Montane Forests
  low_n_folds <- 
    fold_assignments %>% 
    dplyr::group_by(eco_name_daily, spatial_fold) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n < 40)
  
  biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]
  
  # prep_fires() function comes from previous script
  ranger_ready <- prep_fires(fires_all = fires_all, biome_shortname = biome_shortname)  
  data <- ranger_ready$data
  predictor.variable.names <- ranger_ready$predictor.variable.names
  
  human_drivers <- ranger_ready$human_drivers
  topography_drivers <- ranger_ready$topography_drivers
  weather_drivers <- ranger_ready$weather_drivers
  fuel_drivers <- ranger_ready$fuel_drivers
  interacting_drivers <- ranger_ready$interacting_drivers
  fire_drivers <- ranger_ready$fire_drivers
  
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # mlr3 approach
  
  fold_ids <- unique(fold_assignments$spatial_fold)
  
  splits <- 
    lapply(fold_ids, FUN = function(spatial_fold) {
      
      analysis_dids <- fold_assignments$did[fold_assignments$spatial_fold != spatial_fold]
      assessment_dids <- fold_assignments$did[fold_assignments$spatial_fold == spatial_fold]
      
      analysis_data <- data[data$did %in% analysis_dids, ]
      assessment_data <- data[data$did %in% assessment_dids, ]
      
      split <- rsample::make_splits(x = analysis_data, assessment = assessment_data)
      
      return(split)
    })
  
  folds <- 
    rsample::manual_rset(splits = splits, ids = fold_ids) %>% 
    dplyr::filter(id != low_n_folds$spatial_fold)

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
    
    # When we use the matthews correlation coefficient metric, 
    # it's just a single value from the whole observed/predicted set, rather than a value for
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
                                      fire = which(task_ewe$feature_names %in% fire_drivers)),
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
  
  out %>% 
    dplyr::group_by(Variable, spatial_fold = id, biome) %>%
    dplyr::summarize(cpi = mean(CPI),
                     count = n(),
                     se = sd(CPI) / sqrt(count),
                     lwr = cpi - se) %>%
    dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>% 
    dplyr::group_by(Variable, biome) %>% 
    dplyr::summarize(n_pos = length(which(lwr > 0)),
                     n_neg = length(which(lwr < 0)),
                     n_0 = length(which(lwr == 0)),
                     cpi = weighted.mean(x = cpi, w = n), n = n()) %>% 
    dplyr::arrange(desc(cpi)) %>%
    dplyr::filter(n_pos >= 1 & n_pos >= n_neg) %>% 
    print(n = 40)
  
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
  # out_grouped %>%
  #   dplyr::group_by(Group, id, biome) %>%
  #   dplyr::summarize(CPI = mean(CPI),
  #                    n = n())
  # 
  # Group across iterations (summarizing across folds), then group across folds (summarizing CPI per variable for whole biome)
  # in order to 
  out_grouped %>% 
    dplyr::group_by(Group, spatial_fold = id, biome) %>%
    dplyr::summarize(cpi = mean(CPI),
                     count = n(),
                     se = sd(CPI) / sqrt(count),
                     lwr = cpi - se) %>%
    dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>% 
    dplyr::group_by(Group, biome) %>% 
    dplyr::summarize(n_pos = length(which(lwr > 0)),
                     n_neg = length(which(lwr < 0)),
                     n_0 = length(which(lwr == 0)),
                     cpi = weighted.mean(x = cpi, w = n), n = n()) %>% 
    dplyr::arrange(desc(cpi)) %>%
    dplyr::filter(n_pos >= 1 & n_pos >= n_neg) %>% 
    print(n = 40)
  
  # out_grouped %>%
  #   dplyr::group_by(Group, iter, biome) %>%
  #   dplyr::summarize(CPI = mean(CPI)) %>% 
  #   dplyr::group_by(Group, biome) %>% 
  #   dplyr::summarize(CPI = mean(CPI)) %>% 
  #   dplyr::arrange(desc(CPI)) %>% 
  #   print(n = 30)
  
  ###
  
  parallel::stopCluster(cl = cl)
  
  data.table::fwrite(x = out, file = paste0("data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped, paste0(file = "data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, ".csv"))
  
}
