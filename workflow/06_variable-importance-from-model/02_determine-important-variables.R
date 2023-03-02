library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(ranger)

latest_ard_date <- sort(list.files(path = here::here("data", "ard")), 
                        decreasing = TRUE)[1]

ard_dir <- here::here("data", "ard", latest_ard_date)
rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", latest_ard_date)

biome_shortnames <- c("tcf", "mfws")

# Read in the CPI results based on the Matthew's Correlation Coefficient
cpi_results_full <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    return(data.table::fread(input = here::here(rf_cpi_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))))
  }) %>% 
  data.table::rbindlist()

# Summarize the CPI results across iterations and across folds to determine a single CPI value
# per variable per biome (then rank those)
cpi_results <-
  cpi_results_full %>% 
  dplyr::group_by(Variable, spatial_fold = id, biome) %>% 
  dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
                   sd = sd(CPI, na.rm = TRUE),
                   count = sum(!is.na(CPI)),
                   min = min(CPI, na.rm = TRUE),
                   max = max(CPI, na.rm = TRUE),
                   lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
                   upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
  dplyr::arrange(biome, Variable, spatial_fold) %>% 
  dplyr::group_by(Variable, biome) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::filter(cpi_median > 0) %>%
  dplyr::arrange(biome, desc(cpi_median)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(key_var = ifelse(cpi_median > 0, 1, 0))

data.table::fwrite(x = cpi_results, file = here::here(rf_cpi_dir, "cpi-important-variables.csv"))
