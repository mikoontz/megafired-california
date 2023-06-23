library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(ranger)

latest_ard_date <- sort(list.files(path = here::here("data", "ard"), pattern = "[0-9]"), 
                        decreasing = TRUE)[1]

ard_dir <- here::here("data", "ard", latest_ard_date)
rf_tuning_dir <- here::here("data", "out", "rf", "tuning", latest_ard_date)
rf_predict_dir <- here::here("data", "out", "rf", "predict", latest_ard_date)
rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", latest_ard_date)
rf_tables_dir <- here::here("tables", "rf", latest_ard_date)

rf_response_curves_dir <- here::here("figs", "rf", "response-curves", latest_ard_date)
dir.create(rf_response_curves_dir, showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
# biome_shortnames <- c("tcf", "mfws", "dxs")
biome_shortnames <- c("tcf", "mfws")

# Full names of the biomes for the plot titles
# biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
# biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands")
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub")
names(biome_lookup) <- biome_shortnames

driver_description <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

col_pal_df <- 
  read.csv(here::here("data", "out", "driver-color-palette.csv"))

col_pal <- setNames(object = col_pal_df$hexcode, nm = col_pal_df$type)

cpi_results <- data.table::fread(here::here(rf_cpi_dir, "cpi-important-variables.csv"))
model_skill_results <- data.table::fread(here::here(rf_tables_dir, "model-skill-results.csv"))

for(counter in seq_along(biome_shortnames)) {
  biome_shortname <- biome_shortnames[counter]
  
  key_vars <-
    cpi_results %>%
    dplyr::filter(key_var == 1) %>%
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
  
  # Get tuned classification threshold from model skills table
  classification_thresh <- model_skill_results$classification_thresh[model_skill_results$biome == biome_shortname]
  # Get full name for biome from the biome lookup vector
  biome_fullname <- biome_lookup[match(biome_shortname, names(biome_lookup))]  
  
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

