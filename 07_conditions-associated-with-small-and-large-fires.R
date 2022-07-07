# Average value of predictors at different sizes of fires
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(patchwork)

# read in fire data
system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/  data/out/", stdout = TRUE)  

# read in driver descriptions
system2(command = "aws", args = "s3 cp s3://california-megafires/tables/driver-descriptions.csv  tables/driver-descriptions.csv", stdout = TRUE)  

fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

driver_descriptions <- read.csv(file = "tables/driver-descriptions.csv")

# Interquartile range for fire-independent scaling relationships
lwr_med_upr = c(0.25, 0.50, 0.75)

# Get different plots comparing actual measures of variables within fires to fire-independent measures
compare_fire_to_fire_independent <- function(fi_summary_fname, fired_drivers_fname, precision = 20) {
  
  fires <-
    data.table::fread(fired_drivers_fname) %>%
    dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                  -cumu_count, -cumu_area_ha,
                  -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                  -starts_with("adj"),
                  -ends_with("rtma"), -ends_with("rtma_pct"),
                  -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>%
    dplyr::left_join(fired_daily_response) %>% 
    dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha)) %>% 
    dplyr::select(-c(daily_area_tminus1_ha, cum_area_ha, cum_area_ha_tminus1, daily_perim_km, daily_perim_tminus1_km, active_fireline_km, samp_id, predicted_aoi_log_cumarea_tm1, predicted_aoi_cumarea_tm1, aoir_modeled_cumarea_tm1, aoir_cumarea_tm1, predicted_aoi_log_sqrtarea_tm1, predicted_aoi_sqrtarea_tm1, aoir_modeled_sqrtarea_tm1, aoir_sqrtarea_tm1, daily_area_ha))
  
  idx <- substr(names(fires), start = 1, stop = 4) == "raw_"
  names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]
  
  fires_long <-
    fires %>% 
    dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10)) %>%
    dplyr::mutate(area_log10_round = (round(area_log10_pct * precision) / precision)) %>% 
    dplyr::mutate(area_log10_round = ifelse(area_log10_round == max(area_log10_round), yes = area_log10_round[-which.max(area_log10_round)], no = area_log10_round)) %>% 
    tidyr::pivot_longer(cols = -c(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_round, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310), names_to = "driver", values_to = "value") %>% 
    dplyr::select(did, id, date, ig_year, area_ha, area_log10, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310, everything()) %>% 
    dplyr::left_join(driver_descriptions) %>% 
    dplyr::mutate(driver_desc_ggplot = ifelse(driver_type %in% c("fuel", "topography"), yes = driver_desc, no = driver)) 

  effect_size_summaries <- 
    fires_long %>% 
    dplyr::group_by(biome_name, driver, driver_desc, driver_type, area_log10_round, driver_desc_ggplot) %>% 
    summarize(mean_value = mean(value, na.rm = TRUE),
              lwr = t.test(value, conf.level = 0.95)$conf.int[1],
              upr = t.test(value, conf.level = 0.95)$conf.int[2], 
              n = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(area_log10 = area_log10_round,
                  value = mean_value)
  
  fi_summary <- data.table::fread(fi_summary_fname)
  
  fi_summary_wide <- 
    fi_summary %>% 
    dplyr::select(-driver_desc) %>%
    dplyr::mutate(area_log10_pct = ecdf(fires$area_log10)(area_log10_round)) %>% 
    dplyr::rename(area_log10 = area_log10_pct) %>% 
    dplyr::filter(quantile %in% lwr_med_upr) %>% 
    dplyr::mutate(quant_desc = dplyr::case_when(quantile == min(quantile) ~ "lwr",
                                                quantile == max(quantile) ~ "upr",
                                                TRUE ~ "med")) %>%
    dplyr::select(-quantile) %>% 
    tidyr::pivot_wider(names_from = "quant_desc", values_from = "value") %>% 
    dplyr::left_join(driver_descriptions) %>% 
    dplyr::mutate(driver_desc_ggplot = ifelse(driver_type %in% c("fuel", "topography"), yes = driver_desc, no = driver)) 

  # Weather
  fires_weather <-
    # fires_long %>% 
    effect_size_summaries %>% 
    dplyr::filter(driver %in% c("max_vpd_pct", "min_vpd_pct", "max_temp_pct", "min_temp_pct", "max_rh_pct", "min_rh_pct", "max_wind_speed_pct", "min_wind_speed_pct", "max_soil_water_pct", "min_soil_water_pct", "erc_pct", "bi_pct", "wind_anisotropy", "wind_terrain_alignment", "wind_terrain_anisotropy"))
  
  weather_gg <- 
    ggplot() +
    # geom_smooth(data = fires_weather, aes(x = area_log10, y = value), color = "red", fill = "red") +
    geom_line(data = fires_weather, mapping = aes(x = area_log10, y = value), color = "red") + 
    geom_ribbon(data = fires_weather, aes(x = area_log10, ymin = lwr, ymax = upr), color = NA, fill = "red", alpha = 0.25) +
    facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
    theme_bw() +
    labs(x = "Daily area of increase percentile")
  
  # climate
  fires_climate <-
    # fires_long %>% 
    effect_size_summaries %>% 
    dplyr::filter(driver %in% c("spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z"))
  
  climate_gg <- 
    ggplot() +
    # geom_smooth(data = fires_climate, aes(x = area_log10, y = value), color = "red", fill = "red") +
    geom_line(data = fires_climate, mapping = aes(x = area_log10, y = value), color = "red") + 
    geom_ribbon(data = fires_climate, aes(x = area_log10, ymin = lwr, ymax = upr), color = NA, fill = "red", alpha = 0.25) +
    facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
    geom_hline(yintercept = 0) +
    theme_bw() +
    labs(x = "Daily area of increase percentile")
  
  # Fuels
  fires_fuels <-
    # fires_long %>% 
    effect_size_summaries %>% 
    dplyr::filter(driver_type %in% c("fuel"))
  
  fi_fuels <-
    fi_summary_wide %>% 
    dplyr::filter(driver_type %in% c("fuel"))
  
  fuels_gg <-
    ggplot() +
    # geom_smooth(data = fires_fuels, aes(x = area_log10, y = value), color = "red", fill = "red") +
    geom_line(data = fires_fuels, mapping = aes(x = area_log10, y = value), color = "red") + 
    geom_ribbon(data = fires_fuels, aes(x = area_log10, ymin = lwr, ymax = upr), color = NA, fill = "red", alpha = 0.25) +
    geom_point(data = fi_fuels, mapping = aes(x = area_log10, y = med)) + 
    geom_line(data = fi_fuels, mapping = aes(x = area_log10, y = med)) + 
    geom_ribbon(data = fi_fuels, mapping = aes(x = area_log10, ymin = lwr, ymax = upr), alpha = 0.5, fill = "black") +
    facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
    theme_bw() +
    labs(x = "Daily area of increase percentile")
  
  # topography
  fires_topo <-
    # fires_long %>%
    effect_size_summaries %>%
    dplyr::filter(driver_type %in% c("topography"))
  
  fi_topo <-
    fi_summary_wide %>% 
    dplyr::filter(driver_type %in% c("topography"))
  
  topo_gg <-
    ggplot() +
    # geom_smooth(data = fires_topo, aes(x = area_log10, y = value), color = "red", fill = "red") +
    geom_line(data = fires_topo, mapping = aes(x = area_log10, y = value), color = "red") + 
    geom_ribbon(data = fires_topo, mapping = aes(x = area_log10, ymin = lwr, ymax = upr), color = NA, fill = "red", alpha = 0.25) +
    geom_point(data = fi_topo, mapping = aes(x = area_log10, y = med)) + 
    geom_line(data = fi_topo, mapping = aes(x = area_log10, y = med)) + 
    geom_ribbon(data = fi_topo, mapping = aes(x = area_log10, ymin = lwr, ymax = upr), alpha = 0.5, fill = "black") +
    facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
    theme_bw() +
    labs(x = "Daily area of increase percentile")
  
  # human
  fires_human <-
    # fires_long %>% 
    effect_size_summaries %>% 
    dplyr::filter(driver_type %in% c("human"))
  
  fi_human <-
    fi_summary_wide %>% 
    dplyr::filter(driver_type %in% c("human"))
  
  human_gg <-
    ggplot() +
    # geom_smooth(data = fires_human, aes(x = area_log10, y = value), color = "red", fill = "red") +
    geom_line(data = fires_human, mapping = aes(x = area_log10, y = value), color = "red") + 
    geom_ribbon(data = fires_human, mapping = aes(x = area_log10, ymin = lwr, ymax = upr), color = NA, fill = "red", alpha = 0.25) +
    geom_point(data = fi_human, mapping = aes(x = area_log10, y = med)) + 
    geom_line(data = fi_human, mapping = aes(x = area_log10, y = med)) + 
    geom_ribbon(data = fi_human, mapping = aes(x = area_log10, ymin = lwr, ymax = upr), alpha = 0.5, fill = "black") +
    facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
    theme_bw() +
    labs(x = "Daily area of increase percentile")
  
  out <- list(weather = weather_gg, climate = climate_gg, fuels = fuels_gg, topo = topo_gg, human = human_gg) 
  
  return(out)
}

tcf <- compare_fire_to_fire_independent(fi_summary_fname = "data/out/fire-independent-area-scaling-summary_tcf.csv",
                                        fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v3.csv",
                                        precision = 10)

mfws <- compare_fire_to_fire_independent(fi_summary_fname = "data/out/fire-independent-area-scaling-summary_mfws.csv",
                                         fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v3.csv",
                                         precision = 10)

tgss <- compare_fire_to_fire_independent(fi_summary_fname = "data/out/fire-independent-area-scaling-summary_tgss.csv",
                                         fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v3.csv",
                                         precision = 10)

dxs <- compare_fire_to_fire_independent(fi_summary_fname = "data/out/fire-independent-area-scaling-summary_dxs.csv",
                                        fired_drivers_fname = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v3.csv",
                                        precision = 10)


weather_all <- ((tcf$weather + ggplot2::ggtitle("Temperate conifer forests")) + (mfws$weather + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub"))) / ((tgss$weather + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")) + (dxs$weather + ggplot2::ggtitle("Desert Xeric Shrubland")))
weather_all

climate_all <- ((tcf$climate + ggplot2::ggtitle("Temperate conifer forests")) + (mfws$climate + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub"))) / ((tgss$climate + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")) + (dxs$climate + ggplot2::ggtitle("Desert Xeric Shrubland")))
climate_all

fuels_all <- ((tcf$fuels + ggplot2::ggtitle("Temperate conifer forests")) + (mfws$fuels + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub"))) / ((tgss$fuels + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")) + (dxs$fuels + ggplot2::ggtitle("Desert Xeric Shrubland")))
fuels_all

tcf$fuels + ggplot2::ggtitle("Temperate conifer forests")
mfws$fuels + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub")
tgss$fuels + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")
dxs$fuels + ggplot2::ggtitle("Desert Xeric Shrubland")

topo_all <- ((tcf$topo + ggplot2::ggtitle("Temperate conifer forests")) + (mfws$topo + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub"))) / ((tgss$topo + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")) + (dxs$topo + ggplot2::ggtitle("Desert Xeric Shrubland")))
topo_all

tcf$topo + ggplot2::ggtitle("Temperate conifer forests")
mfws$topo + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub")
tgss$topo + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")
dxs$topo + ggplot2::ggtitle("Desert Xeric Shrubland")


human_all <- ((tcf$human + ggplot2::ggtitle("Temperate conifer forests")) + (mfws$human + ggplot2::ggtitle("Mediterranean Forest, Woodland, & Scrub"))) / ((tgss$human + ggplot2::ggtitle("Temperate Grassland, Savanna, and Shrubland")) + (dxs$human + ggplot2::ggtitle("Desert Xeric Shrubland")))
human_all