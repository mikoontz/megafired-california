library(dplyr)
library(ranger)
library(data.table)
library(readr)
library(parallel)
library(tidyr)

dir.create("data/out/rf/fitted", showWarnings = FALSE)
dir.create("tables/rf", recursive = TRUE, showWarnings = FALSE)

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
            mean_informedness = mean(x = mean, na.rm = TRUE),
            lwr_informedness = mean(x = lwr, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::arrange(desc(mean_informedness)) %>% 
  slice(1)

driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

mcc_results <-
  tuning_metrics %>% 
  dplyr::filter(.metric == "mcc") %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>% 
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean_mcc = mean(x = mean, na.rm = TRUE),
            lwr_mcc = mean(x = lwr, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  dplyr::select(biome, mtry, sample.fraction, classification_thresh, min.node.size, .metric, mean_mcc, lwr_mcc) %>% 
  tidyr::pivot_wider(names_from = ".metric", values_from = "mean_mcc")

model_skill_results <- merge(tuned_hyperparameters, mcc_results)
write.csv(x = model_skill_results, file = "tables/rf/model-skill-results.csv")

for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
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
  
  # This is the core model that we will want to fit, which has been tuned
  class.wgts <- 1 / table(data$ewe)
  
  fitted_model <- ranger::ranger(formula = rf_formula,
                                 data = analysis_data[, c("ewe", predictor.variable.names)],
                                 num.trees = biome_tuned_hyperparameters$num.trees,
                                 mtry = biome_tuned_hyperparameters$mtry,
                                 classification = TRUE,
                                 probability = TRUE,
                                 sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                                 replace = FALSE,
                                 splitrule = "hellinger",
                                 min.node.size = biome_tuned_hyperparameters$min.node.size,
                                 class.weights = class.wgts,
                                 num.threads = 11,
                                 importance = "none")
  
  fitted_model$biome <- biome_shortname
  fitted_model$forest$data <- data[, c("ewe", predictor.variable.names)]
  
  readr::write_rds(x = fitted_model, file = paste0("data/out/rf/fitted/rf_ranger_fitted-model_rtma_", biome_shortname, ".rds"))
}

# Get the models that were fit on the whole dataset for each biome
fitted_models_l <- lapply(X = biome_shortnames, FUN = function(biome_shortname) {
  return(readr::read_rds(file = paste0("data/out/rf/fitted/rf_ranger_fitted-model_rtma_", biome_shortname, ".rds")))
})

# For each biome, create a newdata dataframe with the conditional effect
# of each variable entrained within it (i.e., repeat all data a bunch of times
# and vary one target variable at a time along a regular grid, predicting the
# model output along the way)

# Later, we can just assemble a plot with the response curves
# using whichever variables we've identified as "key variables"

for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
  fitted_model <- fitted_models_l[[counter]]
  data <- fitted_model$forest$data
  
  indep_vars <- fitted_model$forest$independent.variable.names
  n_pred <- 50
  
  if(!file.exists(paste0("data/out/rf/predict/rf_ranger_predictions_rtma", biome_shortname, ".csv"))) {
    set.seed(1510)
    
    # n_rep <- 2000
    # idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
    idx <- seq_len(nrow(data))
    
    newdata <- lapply(seq_along(indep_vars), FUN = function(i) {
      other_vars <- indep_vars[-i]
      if (is.factor(data[[indep_vars[i]]])) {
        newdata_x <- levels(data[[indep_vars[i]]])
      } else {
        newdata_x <- seq(min(data[[indep_vars[i]]]), max(data[[indep_vars[i]]]), length.out = n_pred)
      }
      
      this_var_newdata <-
        lapply(seq_along(idx), FUN = function(j) {
          this_idx_newdata <-
            data.frame(target_var = indep_vars[i], group = j, iter = 1:length(newdata_x), x = as.numeric(newdata_x)) %>%
            cbind(data[idx[j], other_vars], row.names = "iter") %>%
            dplyr::mutate(iter = 1:length(newdata_x))
          
          names(this_idx_newdata)[names(this_idx_newdata) == "x"] <- indep_vars[i]
          
          this_idx_newdata <- this_idx_newdata[, c("target_var", "group", "iter", indep_vars)]
          
        }) %>%
        do.call(what = "rbind", args = .)
    }) |>
      do.call("rbind", args = _)
    
    newdata$ewe <- predict(fitted_model, data = newdata)$predictions[, 1]
    
    data.table::fwrite(x = newdata, file = paste0("data/out/rf/predict/rf_ranger_predictions_rtma_", biome_shortname, ".csv"))
  }