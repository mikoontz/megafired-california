# Plot the distribution of covariate values

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

scale_drought <- c("pdsi_z", "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y")
scale_0_to_100 <- c("max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct", "max_wind_filled_gust_rtma_pct", "min_wind_filled_gust_rtma_pct", "max_temp_rtma_pct", "min_temp_rtma_pct", "max_rh_rtma_pct", "min_rh_rtma_pct", "max_vpd_rtma_pct", "min_vpd_rtma_pct", "bi_pct", "erc_pct", "fm100_pct", "fm1000_pct")
scale_not_normalized <- c("npl", "concurrent_fires", "wind_anisotropy_rtma", "wind_terrain_anisotropy_rtma", "wind_terrain_alignment_rtma", "sqrt_aoi_tm1")  
scale_0_to_1 <- c(ard_fires[[1]]$fuel_drivers, ard_fires[[1]]$topography_drivers, "friction_walking_only", "road_density_mpha")

figs_0_to_1 <- 
  lapply(biome_shortnames, FUN = function(x) {
    ard_biome <- ard_fires[[x]]
    cpi_summary_across_folds_biome <- cpi_summary_across_folds[cpi_summary_across_folds$biome == x, ]
    
    # This represents the distributions of all 0 to 1 and 0 to 100 scaled data
    plot_data_0_to_1 <-
      ard_biome$data %>% 
      dplyr::select("did", "biome_name_daily", ard_biome$predictor.variable.names) %>% 
      dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>% 
      dplyr::select(!tidyselect::all_of(c(scale_not_normalized, scale_drought))) %>% 
      dplyr::rename(biome_fullname = biome_name_daily)
    
    # spot_check_01 <-
    #   ard_biome$data %>%
    #   dplyr::select("did", "biome_name_daily", ard_biome$predictor.variable.names) %>%
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>%
    #   dplyr::select(!tidyselect::all_of(c(scale_not_normalized, scale_drought))) %>%
    #   dplyr::rename(biome_fullname = biome_name_daily) %>%
    #   dplyr::pull(barren_grass_forb_herb_mix_tm01)
    # thresh <- ecdf(spot_check_01)(0.5)
    # 
    # spot_check_02 <-
    #   ard_biome$data %>% 
    #   dplyr::select("did", "biome_name_daily", "ewe", ard_biome$predictor.variable.names) %>% 
    #   dplyr::mutate(id = stringr::str_sub(did, start = 1, end = -12)) %>% 
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>% 
    #   dplyr::select(!tidyselect::all_of(c(scale_not_normalized, scale_drought))) %>% 
    #   dplyr::rename(biome_fullname = biome_name_daily) %>% 
    #   dplyr::mutate(barren_grass_forb_herb_mix_tm01_cover = ifelse(barren_grass_forb_herb_mix_tm01 >= 0.5, yes = "high", no = "low"))
    # 
    # spot_check_02 %>% 
    #   dplyr::group_by(barren_grass_forb_herb_mix_tm01_cover, ewe) %>% 
    #   dplyr::tally()
    # 
    # spot_check_02 %>% 
    #   dplyr::filter(barren_grass_forb_herb_mix_tm01_cover == "low") %>% 
    #   group_by(id, ewe) %>% 
    #   tally() %>% 
    #   arrange(desc(n))
    # 
    # common_names <- 
    #   read.csv("data/out/fired-frap-mtbs-join.csv") %>% 
    #   dplyr::filter(id_fired %in% c("135646", "122950", "122694", "135752")) %>% 
    #   dplyr::mutate(id_fired = factor(id_fired, levels = c("135646", "122950", "122694", "135752"))) %>% 
    #   dplyr::arrange(id_fired)
    # 
    # common_names
    
    plot_data_0_to_1_long <- 
      plot_data_0_to_1 %>% 
      tidyr::pivot_longer(cols = !c(did, biome_fullname), names_to = "variable", values_to = "value")
    
    plot_data_0_to_1_long_summarized <-
      plot_data_0_to_1_long %>% 
      dplyr::group_by(biome_fullname, variable) %>% 
      dplyr::summarize(mean = mean(value, na.rm = TRUE),
                       sd = sd(value, na.rm = TRUE),
                       count = sum(!is.na(value)),
                       min = min(value, na.rm = TRUE),
                       max = max(value, na.rm = TRUE),
                       lwr10pct = quantile(value, probs = 0.10),
                       upr90pct = quantile(value, probs = 0.90),
                       lwr25pct = quantile(value, probs = 0.25),
                       upr75pct = quantile(value, probs = 0.75),
                       lwr95CI = mean(value, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(value)) - 1) * sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
                       upr95CI = mean(value, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(value)) - 1) * sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))))
    
    out_gg <-
      ggplot(plot_data_0_to_1_long_summarized, mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$Variable)))) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
      theme_bw() +
      facet_wrap(facets = "biome_fullname") +
      labs(x = "Variable value as a percentile (0 to 1 scale)",
           y = "Covariate")
  })

figs_0_to_1[[1]]
figs_0_to_1[[2]]
figs_0_to_1[[3]]
figs_0_to_1[[4]]

figs_drought <- 
  lapply(biome_shortnames, FUN = function(x) {
    ard_biome <- ard_fires[[x]]
    cpi_summary_across_folds_biome <- cpi_summary_across_folds[cpi_summary_across_folds$biome == x, ]
    
    # This represents the distributions of all 0 to 1 and 0 to 100 scaled data
    plot_data_drought <-
      ard_biome$data %>% 
      dplyr::select("did", "biome_name_daily", ard_biome$predictor.variable.names) %>% 
      dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>% 
      dplyr::select(tidyselect::all_of(c("did", "biome_name_daily", scale_drought))) %>% 
      dplyr::rename(biome_fullname = biome_name_daily) %>% 
      tibble::as_tibble()
    
    plot_data_drought_long <- 
      plot_data_drought %>% 
      tidyr::pivot_longer(cols = !c(did, biome_fullname), names_to = "variable", values_to = "value")
    
    plot_data_drought_long_summarized <-
      plot_data_drought_long %>% 
      dplyr::group_by(biome_fullname, variable) %>% 
      dplyr::summarize(mean = mean(value, na.rm = TRUE),
                       sd = sd(value, na.rm = TRUE),
                       count = sum(!is.na(value)),
                       min = min(value, na.rm = TRUE),
                       max = max(value, na.rm = TRUE),
                       lwr10pct = quantile(value, probs = 0.10),
                       upr90pct = quantile(value, probs = 0.90),
                       lwr25pct = quantile(value, probs = 0.25),
                       upr75pct = quantile(value, probs = 0.75),
                       lwr95CI = mean(value, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(value)) - 1) * sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
                       upr95CI = mean(value, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(value)) - 1) * sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))))
    
    out_gg <-
      ggplot(plot_data_drought_long_summarized, mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$Variable)))) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
      theme_bw() +
      facet_wrap(facets = "biome_fullname") +
      labs(x = "Variable value (normalized to z-score)",
           y = "Covariate")
  })

figs_drought[[1]]
figs_drought[[2]]
figs_drought[[3]]
figs_drought[[4]]

# This represents the distributions of all 0 to 1 and 0 to 100 scaled data
plot_data_non_normalized <-
  ard_fires %>% 
  lapply(FUN = function(x) x$data) %>%
  data.table::rbindlist(fill = TRUE) %>% 
  dplyr::select(tidyselect::all_of(c("did", "biome_name_daily", scale_not_normalized))) %>% 
  dplyr::rename(biome_fullname = biome_name_daily) %>% 
  tibble::as_tibble()

plot_data_non_normalized_long <- 
  plot_data_non_normalized %>%
  dplyr::mutate(biome_fullname = factor(biome_fullname, levels = biome_lookup),
                npl = as.numeric(as.character(npl)),
                wind_terrain_anisotropy_rtma = wind_terrain_anisotropy_rtma*2) %>% # this bounds the variable by [0,1] like wind anisotropy and wind terrain alignment instead of [0,0.5]
  tidyr::pivot_longer(cols = !c(did, biome_fullname), names_to = "variable", values_to = "value")

plot_data_non_normalized_long_summarized <-
  plot_data_non_normalized_long %>% 
  dplyr::group_by(biome_fullname, variable) %>% 
  dplyr::summarize(mean = mean(value, na.rm = TRUE),
                   sd = sd(value, na.rm = TRUE),
                   count = sum(!is.na(value)),
                   min = min(value, na.rm = TRUE),
                   max = max(value, na.rm = TRUE),
                   lwr10pct = quantile(value, probs = 0.10),
                   upr90pct = quantile(value, probs = 0.90),
                   lwr25pct = quantile(value, probs = 0.25),
                   upr75pct = quantile(value, probs = 0.75),
                   lwr95CI = mean(value, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(value)) - 1) * sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
                   upr95CI = mean(value, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(value)) - 1) * sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))))

out_gg_1 <-
  ggplot(plot_data_non_normalized_long_summarized[plot_data_non_normalized_long_summarized$variable %in% c("sqrt_aoi_tm1", "concurrent_fires"), ], mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$Variable)))) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
  theme_bw() +
  facet_wrap(facets = "biome_fullname") +
  labs(x = "Variable value",
       y = "Covariate")

out_gg_2 <-
  ggplot(plot_data_non_normalized_long_summarized[plot_data_non_normalized_long_summarized$variable %in% c("npl"), ], mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$Variable)))) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
  theme_bw() +
  facet_wrap(facets = "biome_fullname") +
  labs(x = "Variable value",
       y = "Covariate") +
  scale_x_continuous(limits = c(1, 5))

# wind_aspect_alignment_rad = wind direction (radians) - slope aspect (radians); wind blowing uphill = 0, wind blowing downhill = pi, wind blowing across slope is pi/2 or 3pi/2
# wind terrain alignment = abs(cos(wind_aspect_alignment_rad)); wind blowing uphill and wind blowing downhill get max value of 1; wind blowing across slope gets value of 0
# Daily wind terrain alignment = mean(wind_terrain_alignment)
# daily wind anisotropy = sd(cos(wind direction (radians))); 
# daily wind terrain anisotropy = sd(wind terrain alignment)

# https://stats.stackexchange.com/questions/45588/variance-of-a-bounded-random-variable; If X [m, M], then var(X) <= (M - m)^2 / 4, then sd(X) <= (M - m) / 2
# Therefore, maximum standard deviation of variable bounded by [0, 1] is 0.5
# Therefore, maximum standard deviation of variable bounded by [-1, 1] is 1

# Wind terrain alignment is bound by [0, 1] because of the abs(cos(X)) function applied
# wind anisotropy is bound by [0, 1] because standard deviation has to be greater than 0, and the internal cos(X) function is bound by [-1, 1]
# wind terrain alignment anisotropy is bound by [0, 0.5] because standard deviation is greater than 0, and max standard deviation of variable bounded by [0, 1] is 0.5
# Above, we multiplied terrain alignment anisotropy by 2 to make it bound by [0,1] like these other variables
out_gg_3 <-
  ggplot(plot_data_non_normalized_long_summarized[plot_data_non_normalized_long_summarized$variable %in% c("wind_anisotropy_rtma", "wind_terrain_alignment_rtma", "wind_terrain_anisotropy_rtma"), ], mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$Variable)))) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
  theme_bw() +
  facet_wrap(facets = "biome_fullname") +
  labs(x = "Variable value (bounded by [0,1])",
       y = "Covariate") +
  scale_x_continuous(limits = c(0, 1))

out_gg_1
out_gg_2
out_gg_3