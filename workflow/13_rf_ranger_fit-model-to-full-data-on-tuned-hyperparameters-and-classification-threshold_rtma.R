library(dplyr)
library(ranger)
library(data.table)
library(readr)
library(parallel)
library(tidyr)

dir.create("data/out/rf/fitted", showWarnings = FALSE)

# Get the function to take the generic analysis ready data and prepare it for {ranger}
source("workflow/10_generic-analysis-ready-data-to-ranger-ready-data_function.R")

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

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

# group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>% 
#   summarize(n = n(),
#             n_not_missing = sum(!is.na(mean)),
#             mean = weighted.mean(x = mean, w = assessment_ewe_n, na.rm = TRUE),
#             lwr = weighted.mean(x = lwr, w = assessment_ewe_n, na.rm = TRUE))

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

mcc_results <-
  tuning_metrics %>% 
  dplyr::filter(.metric == "mcc") %>% 
  group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, alpha, minprop, min.node.size, class.wgts, .metric) %>% 
  summarize(n = n(),
            n_not_missing = sum(!is.na(mean)),
            mean_mcc = weighted.mean(x = mean, w = assessment_ewe_n, na.rm = TRUE),
            lwr_mcc = weighted.mean(x = lwr, w = assessment_ewe_n, na.rm = TRUE)) %>%
  dplyr::filter((n_not_missing / n >= 0.5)) %>% 
  dplyr::select(biome, mtry, sample.fraction, classification_thresh, alpha, minprop, min.node.size, .metric, mean_mcc, lwr_mcc) %>% 
  tidyr::pivot_wider(names_from = ".metric", values_from = "mean_mcc")

model_skill_results <- merge(tuned_hyperparameters, mcc_results)

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

for (counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  print(paste0("Starting the ", biome_shortname, " biome at ", Sys.time()))
  
  biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]
  
  # prep_fires() function comes from previous script
  ranger_ready <- prep_fires(fires_all = fires_all, biome_shortname = biome_shortname)  
  data <- ranger_ready$data
  predictor.variable.names <- ranger_ready$predictor.variable.names
  
  rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))
  
  # This is the core model that we will want to fit, which has been tuned
  class.wgts <- 1 / table(data$ewe)
  
  fitted_model <- ranger::ranger(formula = rf_formula,
                                 data = data[, c("ewe", predictor.variable.names)],
                                 mtry = biome_tuned_hyperparameters$mtry,
                                 num.trees = biome_tuned_hyperparameters$num.trees,
                                 sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                                 replace = FALSE,
                                 splitrule = "maxstat",
                                 alpha = biome_tuned_hyperparameters$alpha,
                                 minprop = biome_tuned_hyperparameters$minprop,
                                 min.node.size = biome_tuned_hyperparameters$min.node.size,
                                 case.weights = as.numeric(class.wgts[data$ewe + 1]),
                                 seed = 1,
                                 num.threads = parallel::detectCores() - 1)
  
  fitted_model$biome <- biome_shortname
  fitted_model$forest$data <- data[, c("ewe", predictor.variable.names)]
  
  readr::write_rds(x = fitted_model, file = paste0("data/out/rf/fitted/rf_ranger_fitted-model_rtma_", biome_shortname, ".rds"))
}
