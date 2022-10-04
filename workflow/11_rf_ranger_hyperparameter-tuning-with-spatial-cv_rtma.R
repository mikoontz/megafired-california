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

analysis_ready_nonspatial_version <- "v2"
analysis_ready_nonspatial_fname <- paste0("data/out/analysis-ready/FIRED-daily-scale-drivers_california_", analysis_ready_nonspatial_version, ".csv")

# Get the function to take the generic analysis ready data and prepare it for {ranger}
source("workflow/10_generic-analysis-ready-data-to-ranger-ready-data_function.R")

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

# Subset to just years where there are RTMA data (2011 and later)
fires_all <-
  fires_all %>%
  dplyr::filter(date >= as.Date("2011-01-01"))

# drop ERA5 columns in favor of RTMA columns for the weather variables
fires_all <-
  fires_all %>% 
  dplyr::select(-contains("era5"))

# Remove all event days that occur after the first EWE for that event
# fires_all <-
#   fires_all %>%
#   dplyr::filter(cumu_ewe <= 1)

# Just include eco regions that have experienced more than 10 EWE's
# target_eco_names <- 
#   fires_all %>% 
#   group_by(eco_name_daily, ewe) %>% 
#   tally() %>% 
#   filter(ewe == 1, n > 10) %>% 
#   pull(eco_name_daily)
# 
# fires_all <-
#   fires_all %>% 
#   dplyr::filter(eco_name_daily %in% target_eco_names)

spatial_cv_tune <- function(iter, predictor.variable.names, folds, tune.df, num.threads) {
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  mtry <- tune.df$mtry[iter]
  num.trees <- tune.df$num.trees[iter]
  sample.fraction <- tune.df$sample.fraction[iter]
  alpha <- tune.df$alpha[iter]
  minprop <- tune.df$minprop[iter]
  min.node.size <- tune.df$min.node.size[iter]
  class.wgts <- tune.df$class.wgts[iter]
  iter <- tune.df$iter[iter]
  
  results <- vector(mode = "list", length = nrow(folds))
  
  for(k in 1:nrow(folds)) {
    analysis_data <- sf::st_drop_geometry(rsample::analysis(folds$splits[[k]]))
    assessment_data <- sf::st_drop_geometry(rsample::assessment(folds$splits[[k]]))
    
    if (!class.wgts) {
      case.weights <- c(1, 1)
    } else {
      case.weights <- 1 / table(analysis_data$ewe)
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
                                   case.weights = case.weights[analysis_data$ewe + 1],
                                   num.threads = num.threads)
    
    results[[k]] <- 
      tibble::tibble(
        id = folds$id[k],
        o = assessment_data$ewe,
        p = predict(fitted_model, data = assessment_data, type = "response")$predictions,
        assessment_ewe_n = nrow(assessment_data),
        assessment_ewe_1 = sum(assessment_data$ewe),
        assessment_ewe_0 = assessment_ewe_n - assessment_ewe_1,
        mtry = mtry,
        num.trees = num.trees,
        sample.fraction = sample.fraction,
        alpha = alpha,
        minprop = minprop,
        min.node.size = min.node.size,
        class.wgts = class.wgts,
        iter = iter
      )
  }
  
  results_out <- data.table::rbindlist(results)
  
  return(results_out)
}

# Check the predictor variables that will be used for the models
# Only TGSS removes any predictors (because those columns have basically 0 variance) 
dropped_vars <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  prep_fires(fires_all, biome_shortname)$dropped_cols
})

# Some dropped rows for each biome (because of NAs in at least one variable)
dropped_rows <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  length(prep_fires(fires_all, biome_shortname)$dropped_rows)
})

for(counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  # prep_fires() function comes from previous script
  ranger_ready <- prep_fires(fires_all = fires_all, biome_shortname = biome_shortname)  
  data <- ranger_ready$data
  predictor.variable.names <- ranger_ready$predictor.variable.names
  
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
                         num.trees = 500, 
                         sample.fraction = c(0.5, 0.6, 0.7, 0.8),
                         alpha = c(0.8, 0.9, 1.0),
                         minprop = c(0, 0.1), 
                         min.node.size = c(1, 5, 10),
                         class.wgts = TRUE,
                         iter = 1:10
  )
  
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

