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

driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_n <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- 
      read.csv(paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv")) %>% 
      dplyr::group_by(biome_shortname, eco_name_daily, spatial_fold) %>% 
      dplyr::tally() %>% 
      dplyr::arrange(spatial_fold)
  }) %>% 
  do.call("rbind", .)

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

mcc_results <-
  tuning_metrics %>% 
  dplyr::filter(.metric == "mcc") %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>% 
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean_mcc = weighted.mean(x = mean, w = assessment_ewe_n, na.rm = TRUE),
            lwr_mcc = weighted.mean(x = lwr, w = assessment_ewe_n, na.rm = TRUE)) %>%
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
