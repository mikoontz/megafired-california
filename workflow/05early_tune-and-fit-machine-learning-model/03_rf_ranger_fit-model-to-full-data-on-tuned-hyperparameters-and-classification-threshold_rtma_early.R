library(dplyr)
library(ranger)
library(data.table)
library(readr)
library(parallel)
library(tidyr)
library(here)

latest_ard_date <- sort(list.files(path = here::here("data", "ard", "early")), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", "early", latest_ard_date)
rf_tuning_dir <- here::here("data", "out", "rf", "tuning", "early", latest_ard_date)

rf_fitted_dir <- here::here("data", "out", "rf", "fitted", "early", latest_ard_date)
dir.create(rf_fitted_dir, showWarnings = FALSE, recursive = TRUE)

rf_tables_dir <- here::here("tables", "rf", "early", latest_ard_date)
dir.create(rf_tables_dir, showWarnings = FALSE, recursive = TRUE)

rf_predict_dir <- here::here("data", "out", "rf", "predict", "early", latest_ard_date)
dir.create(rf_predict_dir, showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

# Tuned hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = here::here(rf_tuning_dir, paste0("rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, "_early.csv")))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)


# Tune with informedness
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

# Tune with MCC
tuned_hyperparameters <-
  tuning_metrics %>% 
  dplyr::filter(.metric %in% c("mcc", "f_meas", "informedness")) %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>%
  # Mean MCC across iterations for each spatial fold
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean = mean(x = mean, na.rm = TRUE),
            lwr = mean(x = lwr, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  tidyr::pivot_wider(id_cols = c(biome, mtry, num.trees, 
                                 sample.fraction, classification_thresh, 
                                 min.node.size, class.wgts, n, n_not_missing),
                     names_from = .metric,
                     values_from = c("mean", "lwr")) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::arrange(desc(mean_mcc)) %>% 
  slice(1)

driver_descriptions <-
  read.csv(here::here("data", "out", "drivers", "driver-descriptions.csv")) %>%
  dplyr::as_tibble()

full_predictor_variable_names <- driver_descriptions$variable

write.csv(x = tuned_hyperparameters, file = here::here(rf_tables_dir, "model-skill-results_early.csv"))

for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
  biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]
  
  data <- 
    data.table::fread(here::here(latest_ard_dir, paste0("daily-drivers-of-california-megafires_", biome_shortname, "_early.csv"))) |>
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
                                 data = data[, c("ewe", predictor.variable.names)],
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
  
  readr::write_rds(x = fitted_model, file = here::here(rf_fitted_dir, paste0("rf_ranger_fitted-model_rtma_", biome_shortname, "_early.rds")))
}

# Get the models that were fit on the whole dataset for each biome
fitted_models_l <- lapply(X = biome_shortnames, FUN = function(biome_shortname) {
  return(readr::read_rds(file = here::here(rf_fitted_dir, paste0("rf_ranger_fitted-model_rtma_", biome_shortname, "_early.rds"))))
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
  
  if(!file.exists(here::here(rf_predict_dir, paste0("rf_ranger_predictions_rtma", biome_shortname, "_early.csv")))) {
    set.seed(1510)
    
    n_rep <- 1000
    idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
    # idx <- seq_len(nrow(data))
    
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
    
    data.table::fwrite(x = newdata, file = here::here(rf_predict_dir, paste0("rf_ranger_predictions_rtma_", biome_shortname, "_early.csv")))
  }
}

# Took about 15 minutes on a 12-core, 64 GB RAM machine