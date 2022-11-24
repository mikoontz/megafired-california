library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(ranger)

dir.create("data/out/rf/predict", showWarnings = FALSE)
dir.create("figs/rf/response-curves/", showWarnings = FALSE)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Full names of the biomes for the plot titles
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

# Read in the CPI results based on the Matthew's Correlation Coefficient
cpi_results_full <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    return(data.table::fread(input = paste0("data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv")))
  }) %>% 
  data.table::rbindlist()

cpi_grouped_results_full <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    return(data.table::fread(input = paste0(file = "data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_grouped_spatial-cv_", biome_shortname, ".csv")))
  }) %>% 
  data.table::rbindlist()

# Get the count of observations in each spatial fold so we can weight the mean CPI value to more heavily rely on CPI calculated for
# the folds with more fire
fold_n <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- 
      read.csv(paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv")) %>% 
      dplyr::group_by(biome_shortname, eco_name_daily, spatial_fold) %>% 
      tally()
  }) %>% 
  do.call("rbind", .)

# Get the threshold for classification so we can add it to the final plots
# Tuned hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/tuning/rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, ".csv"))
})

tuning_metrics <- data.table::rbindlist(tuning_metrics_l)

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
  dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>%
  dplyr::arrange(biome, Variable, spatial_fold) %>% 
  dplyr::group_by(Variable, biome) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_wgt_mean = weighted.mean(x = cpi, w = n),
                   cpi_wgt_lwr = weighted.mean(x = lwr, w = n),
                   cpi_wgt_upr = weighted.mean(x = upr, w = n),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::filter(cpi_median > 0) %>%
  # dplyr::filter(cpi_wgt_mean > 0 & n_neg < 2 & (n_pos / n) >= 0.5) %>%
  dplyr::arrange(biome, desc(cpi_median))
  # dplyr::arrange(biome, desc(cpi_wgt_mean))

cpi_results
cpi_results %>% 
  group_by(biome) %>% 
  tally()

# Summarize CPI results for groups across iterations and variables
cpi_grouped_results <-
  cpi_grouped_results_full %>%
  dplyr::group_by(Group, spatial_fold = id, biome) %>% 
  dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
                   sd = sd(CPI, na.rm = TRUE),
                   count = sum(!is.na(CPI)),
                   min = min(CPI, na.rm = TRUE),
                   max = max(CPI, na.rm = TRUE),
                   lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
                   upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
  dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>%
  dplyr::arrange(biome, Group, spatial_fold) %>% 
  dplyr::group_by(Group, biome) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_wgt_mean = weighted.mean(x = cpi, w = n),
                   cpi_wgt_lwr = weighted.mean(x = lwr, w = n),
                   cpi_wgt_upr = weighted.mean(x = upr, w = n),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::filter(cpi_median > 0) %>% 
  dplyr::arrange(biome, desc(cpi_median))

cpi_grouped_results

cpi_grouped_results %>% 
  group_by(biome) %>% 
  tally()

### visualization
# Get the models that were fit on the whole dataset for each biome
fitted_models_l <- lapply(X = biome_shortnames, FUN = function(biome_shortname) {
  return(readr::read_rds(file = paste0("data/out/rf/fitted/rf_ranger_fitted-model_rtma_", biome_shortname, ".rds")))
})

# For each biome, create the response plot
for (counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
  
  key_vars <-
    cpi_results %>%
    dplyr::filter(biome == biome_shortname) %>% 
    dplyr::pull(Variable)
  
  if(!file.exists(paste0("data/out/rf/predict/rf_ranger_predictions_rtma", biome_shortname, ".csv"))) {
    set.seed(1510)
    
    fitted_model <- fitted_models_l[[counter]]
    data <- fitted_model$forest$data
    
    indep_vars <- fitted_model$forest$independent.variable.names
    n_pred <- 100
    n_rep <- 2000
    idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
    
    newdata <- lapply(seq_along(key_vars), FUN = function(i) {
      other_vars <- setdiff(indep_vars, key_vars[i])
      if (is.factor(data[[key_vars[i]]])) {
        newdata_x <- levels(data[[key_vars[i]]])
      } else {
        newdata_x <- seq(min(data[[key_vars[i]]]), max(data[[key_vars[i]]]), length.out = n_pred)
      }
      
      this_var_newdata <-
        lapply(seq_along(idx), FUN = function(j) {
          this_idx_newdata <-
            data.frame(target_var = key_vars[i], group = j, iter = 1:length(newdata_x), x = as.numeric(newdata_x)) %>%
            cbind(data[idx[j], other_vars], row.names = "iter") %>%
            dplyr::mutate(iter = 1:length(newdata_x))
          
          names(this_idx_newdata)[names(this_idx_newdata) == "x"] <- key_vars[i]
          
          this_idx_newdata <- this_idx_newdata[, c("target_var", "group", "iter", indep_vars)]
          
        }) %>%
        do.call(what = "rbind", args = .)
    }) |>
      do.call("rbind", args = _)
    
    newdata$ewe <- predict(fitted_model, data = newdata)$predictions
    
    data.table::fwrite(x = newdata, file = paste0("data/out/rf/predict/rf_ranger_predictions_rtma_", biome_shortname, ".csv"))
  }
  
  newdata <- 
    data.table::fread(input = paste0("data/out/rf/predict/rf_ranger_predictions_rtma_", biome_shortname, ".csv")) %>% 
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
  
  # Get classification threshold and plot title from tuned_hyperparameters object
  classification_thresh <- tuned_hyperparameters$classification_thresh[tuned_hyperparameters$biome == biome_shortname]
  biome_fullname <- tuned_hyperparameters$biome_fullname[tuned_hyperparameters$biome == biome_shortname]
  
  response_curves_spaghetti_gg <-
    ggplot() +
    geom_line(data = newdata_plotting, aes(x = x, y = ewe, group = group), alpha = 0.1) +
    geom_line(data = newdata_agg, aes(x = x, y = ewe), color = "red", lwd = 1.25) +
    geom_hline(yintercept = classification_thresh, color = "blue", lwd = 1.25) +
    # geom_ribbon(alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw() +
    labs(title = paste0("Response curves for important variables in ", biome_fullname))
  
  response_curves_highlight_gg <-
    ggplot(data = newdata_agg, aes(x = x, y = ewe)) +
    # geom_hline(yintercept = classification_thresh, color = "blue", lwd = 1.25) +
    geom_line(lwd = 1.25) +
    # geom_ribbon(aes(ymin = lwr_ttest, ymax = upr_ttest), alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw() +
    labs(title = paste0("Expectation of response curves for important variables in ", biome_fullname))
  
  
  ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = paste0("figs/rf/response-curves/", biome_shortname, "_response-curves-important-variables_rtma_spaghetti.png"), width = 10)
  ggplot2::ggsave(plot = response_curves_highlight_gg, filename = paste0("figs/rf/response-curves/", biome_shortname, "_response-curves-important-variables_rtma_highlight.png"), width = 10)
}
