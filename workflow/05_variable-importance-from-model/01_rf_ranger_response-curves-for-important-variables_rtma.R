library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(ranger)

latest_ard_date <- sort(list.files(path = here::here("data", "ard")), 
                        decreasing = TRUE)[1]

ard_dir <- here::here("data", "ard", latest_ard_date)
rf_tuning_dir <- here::here("data", "out", "rf", "tuning", latest_ard_date)
rf_predict_dir <- here::here("data", "out", "rf", "predict", latest_ard_date)
rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", latest_ard_date)
  
rf_response_curves_dir <- here::here("figs", "rf", "response-curves", latest_ard_date)
dir.create(rf_response_curves_dir, showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

# Full names of the biomes for the plot titles
# biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

driver_description <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

col_pal_df <- 
  read.csv(here::here("data", "out", "driver-color-palette.csv"))

col_pal <- setNames(object = col_pal_df$hexcode, nm = col_pal_df$type)

# Read in the CPI results based on the Matthew's Correlation Coefficient
cpi_results_full <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    return(data.table::fread(input = here::here(rf_cpi_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))))
  }) %>% 
  data.table::rbindlist()

# cpi_grouped_results_full <-
#   lapply(biome_shortnames, FUN = function(biome_shortname) {
#     return(data.table::fread(input = paste0(file = "data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, ".csv")))
#   }) %>% 
#   data.table::rbindlist()


# Get the threshold for classification so we can add it to the final plots
# Tuned hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = here::here(rf_tuning_dir, paste0("rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, ".csv")))
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
  slice(1) %>% 
  dplyr::ungroup()

tuned_hyperparameters <-
  tuned_hyperparameters %>% 
  dplyr::mutate(biome_fullname = biome_lookup[match(biome, names(biome_lookup))])

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
  # dplyr::filter(cpi_mean > 0) %>%
  # dplyr::filter((n_pos / n) >= 1/3 & (n_neg / n) <= 1/3) %>%
  # dplyr::arrange(biome, desc(cpi_mean))
  dplyr::filter(cpi_median > 0) %>%
  dplyr::arrange(biome, desc(cpi_median))

cpi_results
cpi_results %>% 
  group_by(biome) %>% 
  tally()

cpi_results %>% 
  filter(biome == "tcf") %>% 
  print(n = 23)


cpi_results %>% 
  filter(biome == "mfws") %>% 
  print(n = 23)


cpi_results %>% 
  filter(biome == "dxs") %>% 
  print(n = 23)

# # Summarize CPI results for groups across iterations and variables
# cpi_grouped_results <-
#   cpi_grouped_results_full %>%
#   dplyr::group_by(Group, spatial_fold = id, biome) %>% 
#   dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
#                    sd = sd(CPI, na.rm = TRUE),
#                    count = sum(!is.na(CPI)),
#                    min = min(CPI, na.rm = TRUE),
#                    max = max(CPI, na.rm = TRUE),
#                    lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
#                    upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
#   dplyr::arrange(biome, Group, spatial_fold) %>% 
#   dplyr::group_by(Group, biome) %>% 
#   dplyr::summarize(n_pos = length(which(cpi > 0)),
#                    n_neg = length(which(cpi < 0)),
#                    n_0 = length(which(cpi == 0)),
#                    cpi_mean = mean(x = cpi),
#                    cpi_lwr = mean(x = lwr),
#                    cpi_upr = mean(x = upr),
#                    cpi_median = median(x = cpi),
#                    n = n()) %>% 
#   dplyr::filter(cpi_mean > 0) %>% 
#   dplyr::arrange(biome, desc(cpi_mean))
# 
# cpi_grouped_results
# 
# cpi_grouped_results %>% 
#   group_by(biome) %>% 
#   tally()

for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
  key_vars <-
    cpi_results %>%
    dplyr::filter(biome == biome_shortname) %>% 
    dplyr::pull(Variable)
  
  observed_data <-
    data.table::fread(input = here::here(ard_dir, paste0("daily-drivers-of-california-megafires_", biome_shortname, ".csv"))) %>% 
    tidyr::pivot_longer(cols = !c(did, event_day, daily_area_ha, cumu_area_tm01, ewe, biome_name_daily, biome_shortname, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310, spatial_fold)) %>% 
    dplyr::rename(target_var = name, x = value)
  
  newdata <-
    data.table::fread(input = here::here(rf_predict_dir, paste0("rf_ranger_predictions_rtma_", biome_shortname, ".csv")))
  
  newdata <- 
    newdata[target_var %in% key_vars, ] %>%
    as.data.frame()
  
  newdata_agg <-
    split(x = newdata, drop = TRUE, f = list(newdata$target_var, newdata$iter)) |>
    lapply(FUN = function(x) {
      return(data.frame(target_var = unique(x$target_var),
                        iter = unique(x$iter),
                        x = as.numeric(unique(x[[unique(x$target_var)]])),
                        ewe = mean(x$ewe),
                        lwr_ttest = t.test(x = x$ewe)$conf.int[1],
                        upr_ttest = t.test(x = x$ewe)$conf.int[2]))
    }) |>
    do.call("rbind", args = _) |>
    dplyr::mutate(target_var = factor(target_var, levels = key_vars))
  
  rownames(newdata_agg) <- 1:nrow(newdata_agg)
  
  newdata_plotting <-
    newdata[, c("target_var", "group", "iter", "ewe", key_vars)] %>%
    split(f = .$target_var) %>%
    lapply(FUN = function(x) {
      data.frame(target_var = x$target_var,
                 group = x$group,
                 iter = x$iter,
                 x = as.numeric(x[, unique(x$target_var)]),
                 ewe = x$ewe)
    }) %>%
    do.call(what = "rbind", args = .) |>
    dplyr::mutate(target_var = factor(target_var, levels = key_vars))
  
  rownames(newdata_plotting) <- 1:nrow(newdata_plotting)
  
  observed_data <- 
    observed_data  |>
    dplyr::filter(target_var %in% key_vars) |>
    dplyr::mutate(target_var = factor(target_var, levels = key_vars))
  
  # Get classification threshold and plot title from tuned_hyperparameters object
  classification_thresh <- tuned_hyperparameters$classification_thresh[tuned_hyperparameters$biome == biome_shortname]
  biome_fullname <- tuned_hyperparameters$biome_fullname[tuned_hyperparameters$biome == biome_shortname]
  
  response_curves_spaghetti_gg <-
    ggplot() +
    geom_line(data = newdata_plotting, aes(x = x, y = ewe, group = group), alpha = 0.1) +
    geom_line(data = newdata_agg, aes(x = x, y = ewe), color = "red", linewidth = 1.25) +
    geom_hline(yintercept = classification_thresh, color = "blue", linewidth = 1.25) +
    # geom_ribbon(alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw() +
    labs(title = paste0("Response curves for important variables in ", biome_fullname))
  
  response_curves_highlight_gg <-
    ggplot() +
    # geom_hline(yintercept = classification_thresh, color = "blue", lwd = 1.25) +
    geom_line(data = newdata_agg, aes(x = x, y = ewe), linewidth = 1.25) +
    geom_rug(data = observed_data, aes(x = x)) +
    # geom_ribbon(aes(ymin = lwr_ttest, ymax = upr_ttest), alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw() +
    labs(title = paste0("Expectation of response curves for important variables in ", biome_fullname))
  
  
  ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = here::here(rf_response_curves_dir, paste0(biome_shortname, "_response-curves-important-variables_rtma_spaghetti.png")), width = 10, height = 10)
  ggplot2::ggsave(plot = response_curves_highlight_gg, filename = here::here(rf_response_curves_dir, paste0(biome_shortname, "_response-curves-important-variables_rtma_highlight.png")), width = 10, height = 10)
}

