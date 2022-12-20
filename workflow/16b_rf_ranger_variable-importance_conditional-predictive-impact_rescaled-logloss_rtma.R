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

dir.create("data/out/rf/conditional-predictive-impact/", showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

# Tuned hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, ".csv"))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)

tuned_hyperparameters <-
  tuning_metrics %>% 
  dplyr::filter(.metric == "informedness") %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>% 
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean_informedness = weighted.mean(x = mean, w = assessment_ewe_n, na.rm = TRUE),
            lwr_informedness = weighted.mean(x = lwr, w = assessment_ewe_n, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::arrange(desc(lwr_informedness)) %>% 
  slice(1)

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
  
  fold_assignments <- fold_lookup_tables[fold_lookup_tables$biome_shortname == biome_shortname, ]
  biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]
  
  data <- 
    data.table::fread(paste0("data/ard/daily-drivers-of-california-megafires_", biome_shortname, ".csv")) |>
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
  
  # # Code to use built in resampling approaches
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
  # resampling_ewe <- mlr3::rsmp(.key = "repeated_spcv_coords", folds = 10, repeats = 10)
  # 
  # # When we use the matthews correlation coefficient metric for our model skill
  # # It's just a single value from the whole observed/predicted set, rather than 
  # # a value for each observation, like with the other metrics. So the statistical 
  # # tests implemented in {cpi} are useless here. But because there isn't an 
  # # option to just not do a statistical test, we'll use the fisher test with 
  # # only 1 replication to speed things up.
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
  
  # out %>% arrange(desc(CPI)) %>% filter(p.value <= 0.05)
  
  # # mlr3 approach
  
  
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
    dplyr::mutate(biome_shortname = biome_shortname) %>%
    dplyr::left_join(fold_n, by = c(id = "spatial_fold", "biome_shortname")) %>%
    dplyr::filter(n >= 40) %>%
    dplyr::select(splits, id)
  
  # number of repeats of the spatial cross validation to account for stochastic nature of the model fitting
  n_repeats <- 1
  
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
    
    # Sometimes there aren't representative levels of all the NPL categories in the test data, so we reduce the possible
    # number of levels of NPL to just the represented ones
    # Our knockoff generating approach throws a warning that we are in "dangerous ground" when our categorical variable
    # has fewer than 8 observations per level of the categorical variable. Here, we drop rows in the assessment
    # dataset if their NPL level is part of a representation across the whole assessment dataset of fewer than 8
    # other instances. That is, if there are only 2 rows with an NPL of 3, then both of those rows get dropped
    
    # This code is set up to handle multiple factors in the dataset, even though there is only one for now (NPL)
    factor_cols <- setdiff(which(sapply(assessment_data, is.factor)), which(names(assessment_data) == "ewe"))
    
    if (length(factor_cols) > 0) {
      factor_col_tallies <- lapply(assessment_data[factor_cols], table)
      
      for(factor_col_idx in seq_along(names(factor_col_tallies))) {
        # the name of the column that is the factor
        factor_col <- names(factor_col_tallies)[factor_col_idx]
        # the names of the factor levels that have poor representation in the data (fewer than 5 rows)
        low_n_levels <- names(factor_col_tallies[[factor_col_idx]])[factor_col_tallies[[factor_col_idx]] <= 5]
        # the row numbers of the data corresponding to rows belonging to one of the poorly represented factor levels
        low_n_idx <- which(assessment_data[[factor_col]] %in% low_n_levels)
        
        if (length(low_n_idx) > 0) {
          # drop all rows when factor levels are poorly represented
          assessment_data <- assessment_data[-low_n_idx, ]
        }
        
        # re-establish the possible factor levels in the data
        assessment_data[, factor_col] <- factor(assessment_data[, factor_col], levels = unique(assessment_data[[factor_col]]))
      }
    }
    # sequential knockoffs also can't handle when there is zero variance in a feature
    # in the future, we will check the individual spatial folds for this criteria, and drop features ahead
    # of the tuning step that have no variation in the feature values (we only do this for the whole dataset
    # in a biome, not at the spatial fold level; turns out it's important at the spatial fold level too)
    constant_cols <- setdiff(names(which(sapply(assessment_data[setdiff(predictor.variable.names, names(factor_cols))], var) == 0)), "ewe")
    
    if (length(constant_cols) > 0) {
      assessment_data[constant_cols] <-
        assessment_data[constant_cols] + rnorm(n = nrow(assessment_data),
                                               mean = 0,
                                               sd = abs(mean(assessment_data[, constant_cols]) / 100))
    }
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # https://github.com/mlr-org/mlr3/issues/563
    analysis_data$wt <- as.numeric(class.wgts[analysis_data$ewe])
    
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
                             predict_type = "prob")
    
    task_ewe <-
      mlr3::as_task_classif(analysis_data[, c("ewe", "wt", predictor.variable.names)],
                            target = "ewe",
                            id = "ewe")
    
    # weight column is the "wt" column, to help with class imbalance
    task_ewe$set_col_roles(cols = "wt", roles = "weight")
    
    # When we use the matthews correlation coefficient metric for our model skill
    # It's just a single value from the whole observed/predicted set, rather than
    # a value for each observation, like with the other metrics. So the statistical
    # tests implemented in {cpi} are useless here. But because there isn't an
    # option to just not do a statistical test, we'll use the fisher test with
    # only 1 replication to speed things up.
    out_mcc <- cpi::cpi(task = task_ewe,
                        learner = learner_ewe,
                        # knockoff_fun = seqknockoff::knockoffs_seq, # use sequential knockoffs to handle the npl factor
                        measure = "classif.logloss",
                        test_data = assessment_data,
                        test = "t",
                        modify_trp = rescale_prob,
                        classification_thresh = biome_tuned_hyperparameters$classification_thresh)
    
    out <-
      out_mcc %>%
      dplyr::mutate(id = folds_repeat$id[idx], iter = folds_repeat$iter[idx], biome = biome_shortname)
    (end <- Sys.time())
    (difftime(time1 = end, time2 = start, units = "mins"))
    
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
    
    # Sometimes there aren't representative levels of all the NPL categories in the test data, so we reduce the possible
    # number of levels of NPL to just the represented ones
    # Our knockoff generating approach throws a warning that we are in "dangerous ground" when our categorical variable
    # has fewer than 8 observations per level of the categorical variable. Here, we drop rows in the assessment
    # dataset if their NPL level is part of a representation across the whole assessment dataset of fewer than 8
    # other instances. That is, if there are only 2 rows with an NPL of 3, then both of those rows get dropped
    
    # This code is set up to handle multiple factors in the dataset, even though there is only one for now (NPL)
    factor_cols <- setdiff(which(sapply(assessment_data, is.factor)), which(names(assessment_data) == "ewe"))
    
    if (length(factor_cols) > 0) {
      factor_col_tallies <- lapply(assessment_data[factor_cols], table)
      
      for(factor_col_idx in seq_along(names(factor_col_tallies))) {
        # the name of the column that is the factor 
        factor_col <- names(factor_col_tallies)[factor_col_idx]
        # the names of the factor levels that have poor representation in the data (fewer than 5 rows)
        low_n_levels <- names(factor_col_tallies[[factor_col_idx]])[factor_col_tallies[[factor_col_idx]] <= 5]
        # the row numbers of the data corresponding to rows belonging to one of the poorly represented factor levels
        low_n_idx <- which(assessment_data[[factor_col]] %in% low_n_levels)
        
        if (length(low_n_idx) > 0) {
          # drop all rows when factor levels are poorly represented
          assessment_data <- assessment_data[-low_n_idx, ]
        }
        
        # re-establish the possible factor levels in the data
        assessment_data[, factor_col] <- factor(assessment_data[, factor_col], levels = unique(assessment_data[[factor_col]]))
      }
    }
    # sequential knockoffs also can't handle when there is zero variance in a feature
    # in the future, we will check the individual spatial folds for this criteria, and drop features ahead
    # of the tuning step that have no variation in the feature values (we only do this for the whole dataset
    # in a biome, not at the spatial fold level; turns out it's important at the spatial fold level too)
    constant_cols <- setdiff(names(which(sapply(assessment_data[setdiff(predictor.variable.names, names(factor_cols))], var) == 0)), "ewe")
    
    if (length(constant_cols) > 0) {
      assessment_data[constant_cols] <- 
        assessment_data[constant_cols] + rnorm(n = nrow(assessment_data), 
                                               mean = 0, 
                                               sd = abs(mean(assessment_data[, constant_cols]) / 100))
    }
    
    class.wgts <- 1 / table(analysis_data$ewe)
    
    # https://github.com/mlr-org/mlr3/issues/563
    analysis_data$wt <- as.numeric(class.wgts[analysis_data$ewe])
    
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
                             predict_type = "prob")
    
    task_ewe <- 
      mlr3::as_task_classif(analysis_data[, c("ewe", "wt", predictor.variable.names)], 
                            target = "ewe",
                            id = "ewe")
    
    # weight column is the "wt" column, to help with class imbalance
    task_ewe$set_col_roles(cols = "wt", roles = "weight")
    
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
                        test = "t",
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
  # print(paste0("Starting conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", start_time, "..."))
  print(paste0("Starting conditional predictive impact calculations based on the logloss for ", biome_shortname, " at ", start_time, "..."))
  
  out <- 
    foreach(idx = (1:nrow(folds_repeat)), 
            .combine = rbind, 
            .packages = c("dplyr", "cpi", "mlr3", "mlr3learners", "rsample", "sf"),
            .errorhandling = "remove") %dopar% 
    cpi_mcc(idx)
  
  (end_time <- Sys.time())
  # print(paste0("Finished conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Finished conditional predictive impact calculations based on the log loss for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  out %>% filter(p.value <= 0.05) %>% arrange(desc(CPI))
  ###
  
  (start_time <- Sys.time())
  print(paste0("Starting grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", start_time, "..."))
  
  out_grouped <-
    foreach(idx = (1:nrow(folds_repeat)),
            .combine = rbind,
            .packages = c("dplyr", "cpi", "mlr3", "mlr3learners", "rsample", "sf"),
            .errorhandling = "remove") %dopar%
    cpi_mcc_grouped(idx)
  
  
  (end_time <- Sys.time())
  print(paste0("Finished grouped conditional predictive impact calculations based on the Matthews Correlation Coefficient for ", biome_shortname, " at ", end_time, ". "))
  print(paste0("Time elapsed: ", round(difftime(time1 = end_time, time2 = start_time, units = "mins"), 1), " minutes."))
  
  ###
  
  parallel::stopCluster(cl = cl)
  
  data.table::fwrite(x = out, file = paste0("data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))
  data.table::fwrite(x = out_grouped, paste0(file = "data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, ".csv"))
  
}

# out %>% 
#   dplyr::group_by(Variable, spatial_fold = id, biome) %>%
#   dplyr::summarize(cpi = mean(CPI),
#                    count = n(),
#                    se = sd(CPI) / sqrt(count),
#                    lwr = cpi - se) %>%
#   dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>% 
#   dplyr::group_by(Variable, biome) %>% 
#   dplyr::summarize(n_pos = length(which(cpi > 0)),
#                    n_neg = length(which(cpi < 0)),
#                    n_0 = length(which(cpi == 0)),
#                    cpi = median(x = cpi)) %>% 
#   dplyr::arrange(desc(cpi)) %>%
#   # dplyr::filter(n_pos >= 1 & n_pos >= n_neg) %>%
#   print(n = 40)

# out2 <-
#   out %>% 
#   dplyr::group_by(Variable, biome) %>%
#   dplyr::summarize(cpi = mean(CPI),
#                    count = n(),
#                    se = sd(CPI) / sqrt(count),
#                    lwr = cpi - se) %>% 
#   dplyr::arrange(desc(cpi)) %>%
#   # dplyr::filter(n_pos >= 1 & n_pos >= n_neg) %>%
#   print(n = 40)

# out %>% 
#   dplyr::group_by(Variable, spatial_fold = id, biome) %>%
#   dplyr::summarize(cpi = mean(CPI),
#                    count = n(),
#                    se = sd(CPI) / sqrt(count),
#                    lwr = cpi - se) %>%
#   dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>% 
#   dplyr::group_by(Variable, biome) %>% 
#   dplyr::summarize(n_pos = length(which(cpi > 0)),
#                    n_neg = length(which(cpi < 0)),
#                    n_0 = length(which(cpi == 0)),
#                    cpi = weighted.mean(x = cpi, w = n), n = n()) %>% 
#   dplyr::arrange(desc(cpi)) %>%
#   dplyr::filter(n_pos >= 1 & n_pos >= n_neg) %>%
#   print(n = 40)

# # Group across folds (summarizing across iterations) to see CPI of each variable for each fold
# out_grouped %>%
#   dplyr::group_by(Group, spatial_fold = id, biome) %>%
#   dplyr::summarize(cpi = mean(CPI),
#                    count = n(),
#                    se = sd(CPI) / sqrt(count),
#                    lwr = cpi - se) %>%
#   dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>%
#   dplyr::group_by(Group, biome) %>%
#   dplyr::summarize(n_pos = length(which(cpi > 0)),
#                    n_neg = length(which(cpi < 0)),
#                    n_0 = length(which(cpi == 0)),
#                    cpi = weighted.mean(x = cpi, w = n), n = n()) %>%
#   dplyr::arrange(desc(cpi)) %>%
#   dplyr::filter(n_pos >= 1 & n_pos >= n_neg) %>%
#   print(n = 40)
# 
