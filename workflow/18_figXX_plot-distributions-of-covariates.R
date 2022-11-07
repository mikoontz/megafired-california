# Plot the conditional predictive impact of variables

library(data.table)
library(dplyr)
library(ggplot2)
library(ggdist)
library(sf)
library(patchwork)

# Get the function to take the generic analysis ready data and prepare it for {ranger}
source("workflow/10_generic-analysis-ready-data-to-ranger-ready-data_function.R")

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
# Full names of the biomes for the plot titles
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_lookup_tables <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- 
      read.csv(paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv")) %>% 
      dplyr::arrange(spatial_fold)
  }) %>% 
  do.call("rbind", .)

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_n <- 
  fold_lookup_tables %>% 
  dplyr::group_by(biome_name_daily, biome_shortname, eco_name_daily, spatial_fold) %>% 
  dplyr::tally()

cpi <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <-
      data.table::fread(input = paste0("data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))
  }) %>% 
  data.table::rbindlist()

cpi_summary_across_iter <- 
  cpi %>% 
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
  dplyr::mutate(biome_fullname = biome_lookup[match(biome, names(biome_lookup))])

cpi_summary_across_folds <-
  cpi_summary_across_iter %>% 
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
  dplyr::arrange(biome, desc(cpi_median), desc(cpi_wgt_mean))

cpi_tcf <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "tcf", ]
cpi_mfws <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "mfws", ]
cpi_tgss <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "tgss", ]
cpi_dxs <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "dxs", ]

cpi_summary_across_folds_tcf <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "tcf", ]
cpi_summary_across_folds_mfws <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "mfws", ]
cpi_summary_across_folds_tgss <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "tgss", ]
cpi_summary_across_folds_dxs <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "dxs", ]

color_palette <- c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')

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

ard_fires <- lapply(biome_shortnames, FUN = prep_fires, fires_all = fires_all) %>% setNames(biome_shortnames)




ard_biome <- ard_fires[["tcf"]]

ard_biome$human_drivers

scale_drought <- c("pdsi_z", "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y")
scale_0_to_100 <- c("max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct", "max_wind_filled_gust_rtma_pct", "min_wind_filled_gust_rtma_pct", "max_temp_rtma_pct", "min_temp_rtma_pct", "max_rh_rtma_pct", "min_rh_rtma_pct", "max_vpd_rtma_pct", "min_vpd_rtma_pct", "bi_pct", "erc_pct", "fm100_pct", "fm1000_pct")
scale_not_normalized <- c("npl", "concurrent_fires", "wind_anisotropy_rtma", "wind_terrain_anisotropy_rtma", "wind_terrain_alignment_rtma", "sqrt_aoi_tm1")  
scale_0_to_1 <- c(ard_biome$fuel_drivers, ard_biome$topography_drivers, "friction_walking_only", "road_density_mpha")

plot_data_0_to_1 <- 
  ard_biome$data %>% 
  dplyr::select("did", ard_biome$predictor.variable.names) %>% 
  dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>% 
  dplyr::select(!tidyselect::all_of(c(scale_not_normalized, scale_drought)))

plot_data_0_to_1_long <-
  plot_data_0_to_1 %>% 
  tidyr::pivot_longer(cols = !did, names_to = "variable", values_to = "value")

ggplot(plot_data_0_to_1_long, aes(x = value)) + 
  geom_density() +
  facet_wrap(facets = "variable", scales = "free_y") +
  geom_vline(xintercept = 0.5, color = "red") +
  theme_bw()

ggplot(plot_data_0_to_1_long, aes(x = value, y = factor(variable, levels = rev(cpi_summary_across_folds_tcf$Variable)))) + 
  geom_point() +
  # facet_wrap(facets = "variable", scales = "free_y") +
  # geom_vline(xintercept = 0.5, color = "red") +
  theme_bw()

ggplot(plot_data_0_to_1_long, aes(x = value, y = factor(variable, levels = rev(cpi_summary_across_folds_tcf$Variable)))) + 
  geom_boxplot() +
  theme_bw() +
  geom_vline(xintercept = 0.5, color = "red")

summary(ard_fires[["tcf"]]$data[, ard_fires[["tcf"]]$predictor.variable.names])
