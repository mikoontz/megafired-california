# Plot the distribution of covariate values

library(data.table)
library(dplyr)
library(ggplot2)
library(ggdist)
library(sf)
library(patchwork)

# Get the function to take the generic analysis ready data and prepare it for {ranger}
latest_ard_date <- sort(list.files(path = here::here("data", "ard", "early")), 
                        decreasing = TRUE)[1]

ard_dir <- here::here("data", "ard", "early", latest_ard_date)
rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", "early", latest_ard_date)
biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
# biome_shortnames <- c("tcf", "mfws", "dxs")

# Full names of the biomes for the plot titles
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
# biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

driver_descriptions <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

col_pal_df <- 
  read.csv(here::here("data", "out", "driver-color-palette.csv"))

col_pal <- setNames(object = col_pal_df$hexcode, nm = col_pal_df$type)

ard_fnames <- here::here(ard_dir, paste0("daily-drivers-of-california-megafires_", biome_shortnames, "_early.csv"))
ard_fires <- lapply(ard_fnames, FUN = data.table::fread) |> data.table::rbindlist(fill = TRUE)

scale_drought <- c("pdsi_z", "spei14d", "spei30d", 
                   "spei90d", "spei180d", "spei270d", 
                   "spei1y", "spei2y", "spei5y")

scale_nonnormalized <- c("npl", "short_concurrent_fires", "wind_anisotropy_rtma", 
                          "wind_terrain_anisotropy_rtma", 
                          "sqrt_aoi_tm1") 

scale_0_to_1 <- c("max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct", 
                  "max_wind_filled_gust_rtma_pct", "min_wind_filled_gust_rtma_pct", 
                  "max_temp_rtma_pct", "min_temp_rtma_pct", "max_rh_rtma_pct", 
                  "min_rh_rtma_pct", "max_vpd_rtma_pct", "min_vpd_rtma_pct", 
                  "bi_pct", "erc_pct", "fm100_pct", "fm1000_pct", 
                  driver_descriptions$variable[driver_descriptions$type == "fuel"], 
                  driver_descriptions$variable[driver_descriptions$type == "topography"], 
                  "friction_walking_only", "road_density_mpha",
                  "min_wind_terrain_alignment_rtma_pct", "max_wind_terrain_alignment_rtma_pct")

full_predictor_variable_names <- driver_descriptions$variable

cpi <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- NA
    if(file.exists(here::here(rf_cpi_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv")))) {
      out <-
        data.table::fread(input = here::here(rf_cpi_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))) %>% 
        dplyr::rename(variable = Variable)}
  }) %>% 
  data.table::rbindlist(fill = TRUE)

cpi_summary_across_iter <- 
  cpi %>% 
  dplyr::group_by(variable, spatial_fold = id, biome) %>% 
  dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
                   sd = sd(CPI, na.rm = TRUE),
                   count = sum(!is.na(CPI)),
                   min = min(CPI, na.rm = TRUE),
                   max = max(CPI, na.rm = TRUE),
                   lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
                   upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
  dplyr::arrange(biome, variable, spatial_fold) %>% 
  dplyr::mutate(biome_fullname = biome_lookup[match(biome, names(biome_lookup))]) %>% 
  dplyr::left_join(driver_description)

cpi_summary_across_folds <-
  cpi_summary_across_iter %>% 
  dplyr::group_by(variable, biome) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::arrange(biome, desc(cpi_median)) %>% 
  dplyr::left_join(driver_description)

figs_0_to_1 <- 
  lapply(biome_shortnames, FUN = function(x) {
    ard_biome <- ard_fires[ard_fires$biome_shortname == x, ]
    cpi_summary_across_folds_biome <- cpi_summary_across_folds[cpi_summary_across_folds$biome == x, ]
    
    # This represents the distributions of all 0 to 1 scaled data
    plot_data_0_to_1 <-
      ard_biome %>% 
      tibble::as_tibble() %>% 
      dplyr::select("did", "biome_name_daily", tidyselect::all_of(full_predictor_variable_names)) %>% 
      dplyr::select(!tidyselect::all_of(c(scale_nonnormalized, scale_drought))) %>% 
      dplyr::rename(biome_fullname = biome_name_daily)
    
    drop_cols <- names(plot_data_0_to_1)[which(sapply(plot_data_0_to_1, FUN = function(x) {return(all(is.na(x)))}))]
    
    plot_data_0_to_1 <- 
      plot_data_0_to_1 %>% 
      dplyr::select(!tidyselect::all_of(drop_cols))
    
    # spot_check_01 <-
    #   ard_biome$data %>%
    #   dplyr::select("did", "biome_name_daily", ard_biome$predictor.variable.names) %>%
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>%
    #   dplyr::select(!tidyselect::all_of(c(scale_nonnormalized, scale_drought))) %>%
    #   dplyr::rename(biome_fullname = biome_name_daily) %>%
    #   dplyr::pull(barren_grass_forb_herb_mix_tm01)
    # thresh <- ecdf(spot_check_01)(0.5)
    # 
    # spot_check_02 <-
    #   ard_biome$data %>% 
    #   dplyr::select("did", "biome_name_daily", "ewe", ard_biome$predictor.variable.names) %>% 
    #   dplyr::mutate(id = stringr::str_sub(did, start = 1, end = -12)) %>% 
    #   dplyr::mutate(dplyr::across(tidyselect::all_of(scale_0_to_100), ~ .x / 100)) %>% 
    #   dplyr::select(!tidyselect::all_of(c(scale_nonnormalized, scale_drought))) %>% 
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
    
    if(nrow(cpi_summary_across_folds_biome) > 0) {
      plot_data_0_to_1_long_summarized <-
        plot_data_0_to_1_long_summarized %>% 
        dplyr::mutate(variable = factor(variable, levels = rev(cpi_summary_across_folds_biome$variable)))
    }
    
    out_gg <-
      ggplot(plot_data_0_to_1_long_summarized, mapping = aes(x = mean, y = variable)) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
      geom_vline(xintercept = 0.5) +
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
    ard_biome <- ard_fires[ard_fires$biome_shortname == x, ]
    cpi_summary_across_folds_biome <- cpi_summary_across_folds[cpi_summary_across_folds$biome == x, ]
    
    # This represents the distributions of all z-score data (the drought data)
    plot_data_drought <-
      ard_biome %>% 
      tibble::as_tibble() %>% 
      dplyr::select("did", "biome_name_daily", tidyselect::all_of(full_predictor_variable_names)) %>% 
      dplyr::select(tidyselect::all_of(c("did", "biome_name_daily", scale_drought))) %>% 
      dplyr::rename(biome_fullname = biome_name_daily)
    
    drop_cols <- names(plot_data_drought)[which(sapply(plot_data_drought, FUN = function(x) {return(all(is.na(x)))}))]
    
    plot_data_drought <- 
      plot_data_drought %>% 
      dplyr::select(!tidyselect::all_of(drop_cols))
    
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
    
    if(nrow(cpi_summary_across_folds_biome) > 0) {
      plot_data_drought_long_summarized <-
        plot_data_drought_long_summarized %>% 
        dplyr::mutate(variable = factor(variable, levels = rev(cpi_summary_across_folds_biome$variable)))
    }
    
    out_gg <-
      ggplot(plot_data_drought_long_summarized, mapping = aes(x = mean, y = variable)) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
      ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
      geom_vline(xintercept = 0) +
      theme_bw() +
      facet_wrap(facets = "biome_fullname") +
      labs(x = "Variable value (normalized to z-score)",
           y = "Covariate")
  })

figs_drought[[1]]
figs_drought[[2]]
figs_drought[[3]]
figs_drought[[4]]



# This represents the distributions of all data that aren't normalized at all
nonnormalized_plotting_data <- 
  lapply(biome_shortnames, FUN = function(x) {
    ard_biome <- ard_fires[ard_fires$biome_shortname == x, ]
    cpi_summary_across_folds_biome <- cpi_summary_across_folds[cpi_summary_across_folds$biome == x, ]
    
    # This represents the distributions of all z-score data (the nonnormalized data)
    plot_data_nonnormalized <-
      ard_biome %>% 
      tibble::as_tibble() %>% 
      dplyr::select("did", "biome_name_daily", tidyselect::all_of(full_predictor_variable_names)) %>% 
      dplyr::select(tidyselect::all_of(c("did", "biome_name_daily", scale_nonnormalized))) %>% 
      dplyr::rename(biome_fullname = biome_name_daily)
    
    drop_cols <- names(plot_data_nonnormalized)[which(sapply(plot_data_nonnormalized, FUN = function(x) {return(all(is.na(x)))}))]
    
    plot_data_nonnormalized <- 
      plot_data_nonnormalized %>% 
      dplyr::select(!tidyselect::all_of(drop_cols))
    
    plot_data_nonnormalized_long <- 
      plot_data_nonnormalized %>% 
      tidyr::pivot_longer(cols = !c(did, biome_fullname), names_to = "variable", values_to = "value")
    
    plot_data_nonnormalized_long_summarized <-
      plot_data_nonnormalized_long %>% 
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
    
    if(nrow(cpi_summary_across_folds_biome) > 0) {
      plot_data_nonnormalized_long_summarized <-
        plot_data_nonnormalized_long_summarized %>% 
        dplyr::mutate(variable = factor(variable, levels = rev(cpi_summary_across_folds_biome$variable)))
    }
    

    return(plot_data_nonnormalized_long_summarized)
    
    # out_gg <-
    #   ggplot(plot_data_nonnormalized_long_summarized, mapping = aes(x = mean, y = variable)) +
    #   ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
    #   ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
    #   ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
    #   theme_bw() +
    #   facet_wrap(facets = "biome_fullname") +
    #   labs(x = "Variable value (normalized to z-score)",
    #        y = "Covariate")
  }) %>% 
  data.table::rbindlist()

out_gg_3 <-
  ggplot(nonnormalized_plotting_data[nonnormalized_plotting_data$variable %in% c("wind_anisotropy_rtma", "wind_terrain_alignment_rtma", "wind_terrain_anisotropy_rtma"), ], mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$variable)))) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
  theme_bw() +
  facet_wrap(facets = "biome_fullname") +
  labs(x = "Variable value (bounded by [0,1])",
       y = "Covariate") +
  scale_x_continuous(limits = c(0, 1))


out_gg_1 <-
  ggplot(nonnormalized_plotting_data[nonnormalized_plotting_data$variable %in% c("sqrt_aoi_tm1", "concurrent_fires"), ], 
         mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$variable)))) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr10pct, xmax = upr90pct)) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr25pct, xmax = upr75pct), lwd = 2) +
  ggdist::geom_pointinterval(mapping = aes(xmin = lwr95CI, xmax = upr95CI), lwd = 2, color = "red") +
  theme_bw() +
  facet_wrap(facets = "biome_fullname") +
  labs(x = "Variable value",
       y = "Covariate")

out_gg_2 <-
  ggplot(nonnormalized_plotting_data[nonnormalized_plotting_data$variable %in% c("npl"), ], 
         mapping = aes(x = mean, y = factor(variable, levels = rev(cpi_summary_across_folds_biome$variable)))) +
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

figs_0_to_1[[1]]
figs_0_to_1[[2]]
figs_0_to_1[[3]]
figs_0_to_1[[4]]

figs_drought[[1]]
figs_drought[[2]]
figs_drought[[3]]
figs_drought[[4]]

out_gg_1
out_gg_2
out_gg_3
