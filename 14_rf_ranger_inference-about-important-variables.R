library(dplyr)
library(data.table)
library(readr)
library(ggplot2)

dir.create("data/out/rf/predict", showWarnings = FALSE)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

cpi_results_full <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    return(data.table::fread(input = paste0("data/out/rf/rf_ranger_variable-importance_cpi_classif-fbeta_spatial-cv_", biome_shortname, ".csv")))
  }) %>% 
  data.table::rbindlist()

cpi_grouped_results_full <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    return(data.table::fread(input = paste0(file = "data/out/rf/rf_ranger_variable-importance_cpi_classif-fbeta_grouped_spatial-cv_", biome_shortname, ".csv")))
  }) %>% 
  data.table::rbindlist()


# Group across folds (summarizing across iterations) to see CPI of each variable for each fold
cpi_results_full %>%
  dplyr::group_by(biome, Variable, id) %>%
  dplyr::summarize(cpi = mean(CPI),
                   n = n())

# Group across folds (summarizing across iterations) to see CPI of each variable for each fold
cpi_grouped_results_full %>%
  dplyr::group_by(biome, Group, id) %>%
  dplyr::summarize(CPI = mean(CPI),
                   n = n())

# Group across iterations (summarizing across folds), then group across folds (summarizing CPI per variable for whole biome)
# in order to 
cpi_results <-
  cpi_results_full %>%
  dplyr::group_by(biome, Variable, iter) %>%
  dplyr::summarize(CPI = mean(CPI)) %>% 
  dplyr::group_by(biome, Variable) %>% 
  dplyr::summarize(cpi = mean(CPI),
                   se = sd(CPI) / sqrt(n()),
                   lwr = cpi - se) %>% 
  dplyr::filter(lwr > 0) %>% 
  dplyr::arrange(biome, desc(cpi)) %>% 
  dplyr::ungroup()

# Group across iterations (summarizing across folds), then group across folds (summarizing CPI per variable for whole biome)
# in order to 
cpi_grouped_results <-
  cpi_grouped_results_full %>%
  dplyr::group_by(biome, Group, iter) %>%
  dplyr::summarize(CPI = mean(CPI)) %>% 
  dplyr::group_by(biome, Group) %>% 
  dplyr::summarize(cpi = mean(CPI),
                   se = sd(CPI) / sqrt(n()),
                   lwr = cpi - se) %>% 
  dplyr::filter(lwr > 0) %>%
  dplyr::arrange(biome, desc(cpi)) %>% 
  dplyr::ungroup()

### visualization
# Get the models that were fit on the whole dataset for each biome
fitted_models_l <- lapply(X = biome_shortnames, FUN = function(biome_shortname) {
  return(readr::read_rds(file = paste0("data/out/rf/fitted/rf_ranger_fitted-model_", biome_shortname, ".rds")))
})

# For each biome, create the response plot
for (counter in 2:4) {
  biome_shortname <- biome_shortnames[counter]
  
  key_vars <-
    cpi_results %>%
    dplyr::filter(biome == biome_shortname) %>% 
    dplyr::filter(lwr >= 0) %>%
    dplyr::arrange(dplyr::desc(cpi)) %>%
    dplyr::pull(Variable)
  
  if(!file.exists(paste0("data/out/rf/predict/rf_ranger_predictions_", biome_shortname, ".csv"))) {
    fitted_model <- fitted_models_l[[counter]]
    data <- fitted_model$forest$data
    
    indep_vars <- fitted_model$forest$independent.variable.names
    n_pred <- 100
    n_rep <- 1000
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
    
    data.table::fwrite(x = newdata, file = paste0("data/out/rf/predict/rf_ranger_predictions_", biome_shortname, ".csv"))
  }
  
  newdata <- 
    data.table::fread(input = paste0("data/out/rf/predict/rf_ranger_predictions_", biome_shortname, ".csv")) %>% 
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
  
  response_curves_spaghetti_gg <-
    ggplot() +
    geom_line(data = newdata_plotting, aes(x = x, y = ewe, group = group), alpha = 0.1) +
    geom_line(data = newdata_agg, aes(x = x, y = ewe), color = "red") +
    # geom_ribbon(alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw()
  
  response_curves_highlight_gg <-
    ggplot(data = newdata_agg, aes(x = x, y = ewe)) +
    geom_line(lwd = 1.25) +
    # geom_ribbon(aes(ymin = lwr_ttest, ymax = upr_ttest), alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw()
  
  ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables_spaghetti.png"))
  ggplot2::ggsave(plot = response_curves_highlight_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables_highlight.png"))
}
