library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
# detach("package:cpi", unload=TRUE)
# remove.packages("cpi")
# remotes::install_github(repo = "mikoontz/cpi@megafired-california") # install from my patch
# remotes::install_github(repo = "mikoontz/cpi@modify-probability") # install from my patch
# remotes::install_github(repo = "mikoontz/cpi@dev") # install from my dev
# install.packages("cpi") # install from CRAN
library(cpi)
library(rsample)
library(dplyr)
library(data.table)
# remotes::install_github("kormama1/seqknockoff")
library(seqknockoff)
library(foreach)

latest_rf_tuning_date <- sort(list.files(path = here::here("data", "out", "rf", "tuning", "late")), 
                              decreasing = TRUE)[1]
latest_ard_date <- sort(list.files(path = here::here("data", "ard", "late")), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", "late", latest_ard_date)

latest_rf_tuning_dir <- here::here("data", "out", "rf", "tuning", "late", latest_rf_tuning_date)
rf_cpi_out_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", "late", latest_rf_tuning_date)
rf_cpi_figs_dir <- here::here("figs", "rf", "conditional-predictive-impact", "late", lubridate::today())

dir.create(rf_cpi_out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(rf_cpi_figs_dir, showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws")

# Tuned hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = here::here(latest_rf_tuning_dir, paste0("rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, "_late.csv")))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)

tuned_hyperparameters <-
  tuning_metrics %>% 
  dplyr::filter(.metric == "mcc") %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>%
  # Mean MCC across iterations for each spatial fold
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean_mcc = mean(x = mean, na.rm = TRUE),
            lwr_mcc = mean(x = lwr, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::arrange(desc(mean_mcc)) %>% 
  slice(1)

# Tuned with informedness
# tuned_hyperparameters <-
#   tuning_metrics %>% 
#   dplyr::filter(.metric == "informedness") %>% 
#   group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>% 
#   summarize(n = n(),
#             n_not_missing = sum(!is.na(mean)),
#             mean_informedness = mean(x = mean, na.rm = TRUE),
#             lwr_informedness = mean(x = lwr, na.rm = TRUE)) %>%
#   dplyr::filter((n_not_missing / n >= 0.5)) %>% 
#   dplyr::group_by(biome) %>% 
#   dplyr::arrange(desc(mean_informedness)) %>% 
#   slice(1)

driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

# Function that can modify the truth/response/prob output from the model
# predictions. Used here to implement a classification threshold different 
# from 0.5

rescale_prob <- function(truth, response, prob, classification_thresh) {
  
  rescale <- function(x, old_max, old_min, new_max, new_min) {
    return(((new_max - new_min) / (old_max - old_min)) * (x - old_max) + new_max)
  }
  
  classes <- levels(truth)
  response <- ifelse(prob[, classes[1]] >= classification_thresh, 
                     yes = classes[1], no = classes[2])
  response <- factor(response, levels = classes)
  
  prob[, classes[1]] <- ifelse(prob[, classes[1]] <= classification_thresh,
                               yes = rescale(x = prob[, classes[1]],
                                             old_max = classification_thresh,
                                             old_min = 0,
                                             new_max = 0.5,
                                             new_min = 0),
                               no = rescale(x = prob[, classes[1]],
                                            old_max = 1,
                                            old_min = classification_thresh,
                                            new_max = 1,
                                            new_min = 0.5))
  
  prob[, classes[2]] <- 1 - prob[, classes[1]]
  
  return(list(truth = truth, response = response, prob = prob))
}

mcc <- function(truth, response, prob) {
  
  classes <- levels(truth)
  pos_class <- classes[1]
  neg_class <- classes[2]
  
  tp <- as.numeric(length(which(truth == pos_class & response == pos_class)))
  fp <- as.numeric(length(which(truth == neg_class & response == pos_class)))
  tn <- as.numeric(length(which(truth == neg_class & response == neg_class)))
  fn <- as.numeric(length(which(truth == pos_class & response == neg_class)))
  
  mcc <- ((tp * tn) - (fn * fp)) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  
  loss <- 1 - mcc
} 

for (biome_idx in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[biome_idx]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]
  
  data <- 
    data.table::fread(here::here(latest_ard_dir, paste0("daily-drivers-of-california-megafires_", biome_shortname, "_late.csv"))) |>
    dplyr::mutate(ewe = factor(ewe, levels = c(1, 0))) |>
    as.data.frame()
  
  predictor.variable.names <- 
    names(data)[names(data) %in% full_predictor_variable_names]
  
  human_drivers <- 
    names(data)[names(data) %in% driver_descriptions[driver_descriptions$type == "human", "variable"]]
  
  topography_drivers <- 
    names(data)[names(data) %in% driver_descriptions[driver_descriptions$type == "topography", "variable"]]
  
  weather_drivers <- 
    names(data)[names(data) %in% driver_descriptions[driver_descriptions$type == "weather", "variable"]]
  
  fuel_drivers <- 
    names(data)[names(data) %in% driver_descriptions[driver_descriptions$type == "fuel", "variable"]]
  
  interacting_drivers <- 
    names(data)[names(data) %in% driver_descriptions[driver_descriptions$type == "interacting", "variable"]]
  
  fire_drivers <- 
    names(data)[names(data) %in% driver_descriptions[driver_descriptions$type == "fire", "variable"]]
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # Code to use built in resampling approaches
  # class.wgts <- 1 / table(data$ewe)
  # 
  # # https://github.com/mlr-org/mlr3/issues/563
  # data$wt <- as.numeric(class.wgts[data$ewe])
  # 
  # # Define the learner to be a {ranger} regression and give it the tuned hyperparameters
  # learner_ewe <- mlr3::lrn("classif.ranger", 
  #                          mtry = biome_tuned_hyperparameters$mtry,
  #                          num.trees = biome_tuned_hyperparameters$num.trees,
  #                          sample.fraction = biome_tuned_hyperparameters$sample.fraction,
  #                          replace = FALSE,
  #                          splitrule = "hellinger",
  #                          min.node.size = biome_tuned_hyperparameters$min.node.size,
  #                          num.threads = 11,
  #                          keep.inbag = TRUE,
  #                          predict_type = "prob")
  # 
  # task_ewe <- 
  #   mlr3spatiotempcv::as_task_classif_st(data[, c("ewe", "wt", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)], 
  #                                        target = "ewe",
  #                                        id = "ewe",
  #                                        coordinate_names = c("x_biggest_poly_3310", "y_biggest_poly_3310"),
  #                                        crs = sf::st_crs(3310),
  #                                        coords_as_features = FALSE)
  # 
  # # weight column is the "wt" column, to help with class imbalance
  # task_ewe$set_col_roles(cols = "wt", roles = "weight")
  # 
  # resampling_ewe <- mlr3::rsmp(.key = "repeated_spcv_coords", folds = 10, repeats = 5)
  
  # When we use the matthews correlation coefficient metric for our model skill
  # It's just a single value from the whole observed/predicted set, rather than 
  # a value for each observation, like with the other metrics. So the statistical 
  # tests implemented in {cpi} are useless here. But because there isn't an 
  # option to just not do a statistical test, we'll use the fisher test with 
  # only 1 replication to speed things up.
  # (start <- Sys.time())
  # out <- 
  #   cpi::cpi(task = task_ewe,
  #            learner = learner_ewe,
  #            measure = "classif.logloss",
  #            resampling = resampling_ewe,
  #            test = "t",
  #            modify_trp = rescale_prob,
  #            classification_thresh = biome_tuned_hyperparameters$classification_thresh)
  # (end <- Sys.time())
  # (difftime(time1 = end, time2 = start, units = "mins"))
  # 
  # out %>% arrange(desc(CPI)) %>% filter(p.value <= 0.05)
  
  # mlr3 approach
  
  fold_ids <- unique(data$spatial_fold)
  
  splits <- 
    lapply(fold_ids, FUN = function(spatial_fold) {
      
      analysis_data <- data[data$spatial_fold != spatial_fold, ]
      assessment_data <- data[data$spatial_fold == spatial_fold, ]
      
      split <- rsample::make_splits(x = analysis_data, assessment = assessment_data)
      
      return(split)
    }) 
  
  folds <- 
    rsample::manual_rset(splits = splits, ids = fold_ids) %>% 
    dplyr::mutate(biome_shortname = biome_shortname)
  
  # number of repeats of the spatial cross validation to account for stochastic nature of the model fitting
  n_repeats <- 50
  
  # repeat the spatial folds so we can re-fit the model (account for the stochastic nature of RF)
  folds_repeat <- 
    lapply(X = 1:n_repeats, FUN = function(i) {
      return(dplyr::mutate(folds, iter = i))
    }) %>% 
    do.call(what = "rbind", args = .)
  
  # Iterate through each split, using the resample::analysis(split) data as training data for the mlr3 task
  # and rsample::assessment(split) data as the test_data for the call to the cpi function 
  cpi_mcc <- function(idx) {
    analysis_data <- 
      folds_repeat$splits[[idx]] %>% 
      rsample::analysis() %>% 
      sf::st_drop_geometry()
    
    assessment_data <- 
      folds_repeat$splits[[idx]] %>% 
      rsample::assessment() %>% 
      sf::st_drop_geometry()
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # # https://github.com/mlr-org/mlr3/issues/563
    # analysis_data$wt <- as.numeric(class.wgts[analysis_data$ewe])
    
    # Define the learner to be a {ranger} regression and give it the tuned hyperparameters
    learner_ewe <- mlr3::lrn("classif.ranger", 
                             mtry = biome_tuned_hyperparameters$mtry,
                             num.trees = biome_tuned_hyperparameters$num.trees,
                             sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                             replace = FALSE,
                             splitrule = "hellinger",
                             min.node.size = biome_tuned_hyperparameters$min.node.size,
                             num.threads = 11,
                             keep.inbag = TRUE,
                             predict_type = "prob",
                             class.weights = class.wgts)
    
    task_ewe <- 
      mlr3::as_task_classif(analysis_data[, c("ewe", predictor.variable.names)], 
                            target = "ewe",
                            id = "ewe")
    
    # # weight column is the "wt" column, to help with class imbalance
    # task_ewe$set_col_roles(cols = "wt", roles = "weight")
    
    # When we use the matthews correlation coefficient metric for our model skill
    # It's just a single value from the whole observed/predicted set, rather than 
    # a value for each observation, like with the other metrics. So the statistical 
    # tests implemented in {cpi} are useless here. But because there isn't an 
    # option to just not do a statistical test, we'll use the fisher test with 
    # only 1 replication to speed things up.
    out_mcc <- cpi::cpi(task = task_ewe,
                        learner = learner_ewe,
                        # knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                        measure = mcc,
                        test_data = assessment_data,
                        test = "fisher",
                        B = 1,
                        modify_trp = rescale_prob,
                        classification_thresh = biome_tuned_hyperparameters$classification_thresh)
    
    out <- 
      out_mcc %>% 
      dplyr::mutate(id = folds_repeat$id[idx], 
                    iter = folds_repeat$iter[idx], 
                    biome = biome_shortname)
    
    return(out)
  }
  
  ## Grouped version
  # Iterate through each split, using the resample::analysis(split) data as training data for the mlr3 task
  # and rsample::assessment(split) data as the test_data for the call to the cpi function
  cpi_mcc_grouped <- function(idx) {
    analysis_data <-
      folds_repeat$splits[[idx]] %>%
      rsample::analysis() %>%
      sf::st_drop_geometry()
    
    assessment_data <-
      folds_repeat$splits[[idx]] %>%
      rsample::assessment() %>%
      sf::st_drop_geometry()
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # Define the learner to be a {ranger} regression and give it the tuned hyperparameters
    learner_ewe <- mlr3::lrn("classif.ranger", 
                             mtry = biome_tuned_hyperparameters$mtry,
                             num.trees = biome_tuned_hyperparameters$num.trees,
                             sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                             replace = FALSE,
                             splitrule = "hellinger",
                             min.node.size = biome_tuned_hyperparameters$min.node.size,
                             num.threads = 11,
                             keep.inbag = TRUE,
                             predict_type = "prob",
                             class.weights = class.wgts)
    
    task_ewe <- 
      mlr3::as_task_classif(analysis_data[, c("ewe", predictor.variable.names)], 
                            target = "ewe",
                            id = "ewe")
    
    # When we use the matthews correlation coefficient metric for our model skill
    # It's just a single value from the whole observed/predicted set, rather than 
    # a value for each observation, like with the other metrics. So the statistical 
    # tests implemented in {cpi} are useless here. But because there isn't an 
    # option to just not do a statistical test, we'll use the fisher test with 
    # only 1 replication to speed things up.
    out_mcc <- cpi::cpi(task = task_ewe,
                        learner = learner_ewe,
                        # knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                        measure = mcc,
                        groups = list(human = which(task_ewe$feature_names %in% human_drivers),
                                      topography = which(task_ewe$feature_names %in% topography_drivers),
                                      weather = which(task_ewe$feature_names %in% weather_drivers),
                                      fuel = which(task_ewe$feature_names %in% fuel_drivers),
                                      wind_terrain = which(task_ewe$feature_names %in% interacting_drivers),
                                      fire = which(task_ewe$feature_names %in% fire_drivers)),
                        test_data = assessment_data,
                        test = "fisher",
                        B = 1,
                        modify_trp = rescale_prob,
                        classification_thresh = biome_tuned_hyperparameters$classification_thresh)
    
    out <-
      out_mcc %>%
      dplyr::mutate(id = folds_repeat$id[idx], iter = folds_repeat$iter[idx], biome = biome_shortname)
    
    return(out)
  }
  
  
  # Here's what we're really after, since we already have a tuned model and spatially cross-validated performance/skill metrics
  cl <- parallel::makeCluster(parallel::detectCores() - 1)  
  doParallel::registerDoParallel(cl)  
  
  (start_time <- Sys.time())
  print(paste0("Starting conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", start_time, "..."))
  
  if(!file.exists(here::here(rf_cpi_out_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, "_late.csv")))) { 
    out <- 
      foreach(idx = (1:nrow(folds_repeat)), 
              .combine = rbind, 
              .packages = c("dplyr", "cpi", "mlr3", "mlr3learners", "rsample", "sf"),
              .errorhandling = "remove") %dopar% 
      cpi_mcc(idx)
    
    data.table::fwrite(x = out, file = here::here(rf_cpi_out_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, "_late.csv")))
  }
  
  (end_time <- Sys.time())
  print(paste0("Finished conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", start_time, "..."))
  
  if(!file.exists(here::here(rf_cpi_out_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, "_late.csv")))) {
    out_grouped <-
      foreach(idx = (1:nrow(folds_repeat)),
              .combine = rbind,
              .packages = c("dplyr", "cpi", "mlr3", "mlr3learners", "rsample", "sf"),
              .errorhandling = "remove") %dopar%
      cpi_mcc_grouped(idx)
    
    
    data.table::fwrite(x = out_grouped, file = here::here(rf_cpi_out_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, "_late.csv")))
    
    (end_time <- Sys.time())
  }
  
  print(paste0("Finished grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  parallel::stopCluster(cl = cl)
  
  
}

# [1] "Starting the tcf biome at 2023-06-22 19:19:17"
# [1] "Starting conditional predictive impact calculations based on the Matthews Correlation Coefficient for tcf at 2023-06-22 19:19:20..."
# [1] "Finished conditional predictive impact calculations based on the Matthews Correlation Coefficient for tcf at 2023-06-22 19:26:48. "
# [1] "Time elapsed: 7.5 minutes."
# [1] "Starting grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for tcf at 2023-06-22 19:26:48..."
# [1] "Finished grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for tcf at 2023-06-22 19:29:24. "
# [1] "Time elapsed: 2.6 minutes."
# [1] "Starting the mfws biome at 2023-06-22 19:29:24"
# [1] "Starting conditional predictive impact calculations based on the Matthews Correlation Coefficient for mfws at 2023-06-22 19:29:27..."
# [1] "Finished conditional predictive impact calculations based on the Matthews Correlation Coefficient for mfws at 2023-06-22 19:33:18. "
# [1] "Time elapsed: 3.9 minutes."
# [1] "Starting grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for mfws at 2023-06-22 19:33:18..."
# [1] "Finished grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for mfws at 2023-06-22 19:34:36. "
# [1] "Time elapsed: 1.3 minutes."