library(dplyr)
library(purrr)
library(spatialsample)
library(sf)
library(tidyr)
library(pbapply)
library(data.table)
library(ranger)
library(PRROC)
library(ggplot2)
library(here)
# library(mlr3measures)
# library(MLmetrics)

latest_ard_date <- sort(list.files(path = here::here("data", "ard", "early")), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", "early", latest_ard_date)

local_out_dir <- here::here("data", "out", "rf", "tuning", "early", latest_ard_date)
dir.create(local_out_dir, recursive = TRUE, showWarnings = FALSE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

spatial_cv_tune <- function(i, predictor.variable.names, folds, tune.df, num.threads) {
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  mtry <- tune.df$mtry[i]
  num.trees <- tune.df$num.trees[i]
  sample.fraction <- tune.df$sample.fraction[i]
  min.node.size <- tune.df$min.node.size[i]
  class.wgts <- tune.df$class.wgts[i]
  iter <- tune.df$iter[i]
  
  results <- vector(mode = "list", length = nrow(folds))
  
  for(k in 1:nrow(folds)) {
    analysis_data <- sf::st_drop_geometry(rsample::analysis(folds$splits[[k]]))
    assessment_data <- sf::st_drop_geometry(rsample::assessment(folds$splits[[k]]))
    
    if (!class.wgts) {
      class.weights <- c(1, 1)
    } else {
      class.weights <- 1 / table(analysis_data$ewe)
    }
    
    # fit the model to the analysis set
    # ranger() uses replace = FALSE and splitrule = "maxstat"
    # to avoid biased random forests that favor variables with lots of unique values
    fitted_model <- ranger::ranger(formula = rf_formula,
                                   data = analysis_data[, c("ewe", predictor.variable.names)],
                                   num.trees = num.trees,
                                   mtry = mtry,
                                   classification = TRUE,
                                   probability = TRUE,
                                   sample.fraction = sample.fraction,
                                   replace = FALSE,
                                   splitrule = "hellinger",
                                   min.node.size = min.node.size,
                                   class.weights = class.weights,
                                   num.threads = num.threads,
                                   importance = "none")
    
    results[[k]] <- 
      tibble::tibble(
        id = folds$id[k],
        o = assessment_data$ewe,
        p = predict(fitted_model, data = assessment_data, type = "response")$predictions[, "1"],
        assessment_ewe_n = nrow(assessment_data),
        assessment_ewe_1 = length(which(assessment_data$ewe == 1)),
        assessment_ewe_0 = assessment_ewe_n - assessment_ewe_1,
        mtry = mtry,
        num.trees = num.trees,
        sample.fraction = sample.fraction,
        min.node.size = min.node.size,
        class.wgts = class.wgts,
        iter = iter
      )
  }
  
  results_out <- data.table::rbindlist(results)
  
  return(results_out)
}

(start_time <- Sys.time())
for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
  data <- read.csv(paste0(latest_ard_dir, "/daily-drivers-of-california-megafires_", biome_shortname,"_early.csv"))
  data$ewe <- factor(data$ewe, levels = c(1, 0))
  
  data$short_concurrent_fires <- as.numeric(data$short_concurrent_fires)
  predictor.variable.names <- names(data)[names(data) %in% full_predictor_variable_names]
  
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  # Set up analysis/assessment splits
  fold_ids <- unique(data$spatial_fold)
  
  splits <- 
    lapply(fold_ids, FUN = function(spatial_fold) {
      
      analysis_data <- data[!(data$spatial_fold %in% spatial_fold), ]
      assessment_data <- data[data$spatial_fold %in% spatial_fold, ]
      
      split <- rsample::make_splits(x = analysis_data, 
                                    assessment = assessment_data)
      
      return(split)
    }) 
  
  folds <- rsample::manual_rset(splits = splits, ids = fold_ids)
  
  # Now we'll tune our model by setting up a grid over 4 different variables we want to tune
  # Tuned variables: mtry, sample.fraction, min.node.size, num.trees
  # Non-tuned variables:  
  #   class.weights (always use them to upsample non-dominant class), replace (always sample without replacement),
  #   splitrule (always use 'hellinger' split rule)
  mtry_vec <- seq(from = floor(sqrt(length(predictor.variable.names))) - 3,
                  to = floor(length(predictor.variable.names) / 1.75),
                  by = 2)
  
  # tune.df <- expand.grid(mtry = mtry_vec, 
  #                        num.trees = c(1000), 
  #                        sample.fraction = c(0.5, (1 - 1/exp(1)), 0.7, 0.8),
  #                        min.node.size = c(1, 3, 5, 10, 25, 50),
  #                        class.wgts = TRUE,
  #                        iter = 1:10)
  
  tune.df <- expand.grid(mtry = mtry_vec, 
                         num.trees = c(1000), 
                         sample.fraction = c(0.4, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
                         min.node.size = c(1, 5, 10, 25, 50, 60),
                         class.wgts = TRUE,
                         iter = 1:10)
  
  
  out <- pblapply(X = (1:nrow(tune.df)), 
                  FUN = spatial_cv_tune, 
                  predictor.variable.names = predictor.variable.names,
                  folds = folds,
                  tune.df = tune.df,
                  num.threads = 11)
  
  out_all <- data.table::rbindlist(out)
  
  data.table::fwrite(x = out_all, file = paste0(local_out_dir, "/rf_ranger_spatial-cv-tuning_rtma_", biome_shortname, "_early.csv"))
}
(end_time <- Sys.time())
(difftime(end_time, start_time, units = "hours"))

# 12-core machine with 64GB of RAM for 2023-06-21 early fire version
# [1] "Starting the tcf biome at 2023-06-21 13:04:04"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=02h 16m 03s
# [1] "Starting the mfws biome at 2023-06-21 15:20:08"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=04h 56m 31s
# [1] "Starting the dxs biome at 2023-06-21 20:16:39"
|++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=26m 52s
# > (end_time <- Sys.time())
# [1] "2023-06-21 20:43:31 MDT"
# > (difftime(end_time, start_time, units = "hours"))
# Time difference of 7.657518 hours