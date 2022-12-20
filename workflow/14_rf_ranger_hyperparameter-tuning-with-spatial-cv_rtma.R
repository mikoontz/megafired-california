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
# library(mlr3measures)
# library(MLmetrics)

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


for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
  data <- read.csv(paste0("data/ard/daily-drivers-of-california-megafires_", biome_shortname,".csv"))
  data$npl <- factor(data$npl, levels = 1:5)
  data$ewe <- factor(data$ewe, levels = 0:1)
  
  data$concurrent_fires <- as.numeric(data$concurrent_fires)
  predictor.variable.names <- names(data)[names(data) %in% full_predictor_variable_names]
  
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
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
  
  # Save the assignments of the spatial folds so we can ensure they are the same later for the
  # conditional predictive impact step
  lookup_table <-
    folds %>% 
    purrr::pmap(.f = function(id, splits) {
      return(data.frame(biome_name_daily = rsample::assessment(splits)$biome_name_daily,
                        biome_shortname = biome_shortname,
                        eco_name_daily = rsample::assessment(splits)$eco_name_daily, 
                        did = rsample::assessment(splits)$did,
                        spatial_fold = id))
    }) %>% 
    data.table::rbindlist()
  
  data.table::fwrite(x = lookup_table, file = paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv"))
  
  # Now we'll tune our model by setting up a grid over 5 different variables we want to tune
  # Tuned variables: mtry, sample.fraction, alpha, minprop, min.node.size
  # Non-tuned variables: num.trees (always 500 because we're computationally constrained), 
  #   class.weights (always use them to upsample non-dominant class), replace (always sample without replacement),
  #   splitrule (always use 'maxstat' split rule)
  mtry_vec <- seq(from = floor(sqrt(length(predictor.variable.names))) - 3,
                  to = floor(length(predictor.variable.names) / 1.75),
                  by = 2)
  
  tune.df <- expand.grid(mtry = mtry_vec, 
                         num.trees = c(100, 250, 500, 1000), 
                         sample.fraction = c(0.5, (1 - 1/exp(1)), 0.7, 0.8),
                         min.node.size = c(1, 3, 5, 10, 25, 50),
                         class.wgts = TRUE,
                         iter = 1:10)
  
  # Here's what we're really after, since we already have a tuned model and spatially cross-validated performance/skill metrics
  
  # Tried a few different ways of parallelizing; looks like leaning on the internal-to-{ranger} approach is best
  
  # cl <- parallel::makeCluster(parallel::detectCores() - 1)
  # doParallel::registerDoParallel(cl)
  # parallel::clusterExport(cl, c("spatial_cv_tune", "predictor.variable.names", "folds", "tune.df"))
  # 
  # out <- pblapply(X = (1:nrow(tune.df)), cl = cl,
  #                 FUN = spatial_cv_tune,
  #                 predictor.variable.names = predictor.variable.names,
  #                 folds = folds,
  #                 tune.df = tune.df,
  #                 num.threads = 1)
  # 
  # parallel::stopCluster(cl = cl)
  
  out <- pblapply(X = (1:nrow(tune.df)), 
                  FUN = spatial_cv_tune, 
                  predictor.variable.names = predictor.variable.names,
                  folds = folds,
                  tune.df = tune.df,
                  num.threads = 11)
  
  out_all <- data.table::rbindlist(out)
  
  data.table::fwrite(x = out_all, file = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-tuning_rtma_", biome_shortname, ".csv"))
}

