# Combine all drivers info so they are analysis ready
library(dplyr)
library(sf)
library(tidyr)
library(data.table)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
drivers_version <- "v9"
adjusted_drivers_version <- paste0(drivers_version, "_adjusted")

analysis_ready_nonspatial_version <- "v2"
analysis_ready_nonspatial_fname <- paste0("data/out/analysis-ready/FIRED-daily-scale-drivers_california_", analysis_ready_nonspatial_version, ".csv")

other_fires_summary <- 
  read.csv(file = "data/out/other-fires-summary.csv") %>%
  dplyr::filter(did %in% fired_daily$did) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  as_tibble()

# For defining "ewe" or not, what is the percentage threshold? E.g., 0.95 means an "ewe" is in the top
# 5th percentile for daily area of increase
pct_threshold <- 0.95

fires_all_list <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    
    fired_daily_response <- 
      data.table::fread(input = "data/out/fired/05_daily-with-behavior-metrics/fired_daily_ca_behavior-metrics.csv")
    
    driver_descriptions <- read.csv(file = "tables/driver-descriptions.csv")
    
    fires <- 
      data.table::fread(paste0("data/out/FIRED-daily-scale-drivers_california_", biome_shortname, "_", adjusted_drivers_version, ".csv")) %>% 
      dplyr::select(-max_wind_speed_era5, -min_wind_speed_era5, -max_rh_era5, -min_rh_era5, -max_temp_era5, -min_temp_era5, -max_soil_water_era5, -min_soil_water_era5, -max_vpd_era5, -min_vpd_era5,
                    -max_wind_speed_rtma, -min_wind_speed_rtma, -max_wind_gust_rtma, -min_wind_gust_rtma, -max_wind_filled_gust_rtma, -min_wind_filled_gust_rtma,
                    -max_rh_rtma, -min_rh_rtma, -max_temp_rtma, -min_temp_rtma,
                    -cumu_count, -cumu_area_ha,
                    -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac,
                    -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"),
                    -starts_with("raw"),
      ) %>% 
      dplyr::left_join(fired_daily_response) %>% 
      # dplyr::mutate(n_afd = ifelse(is.na(n_afd), yes = 0, no = n_afd),
      #               frp90 = ifelse(is.na(frp90), yes = 0, no = frp90),
      #               afd_per_ha = ifelse(is.na(afd_per_ha), yes = 0, no = afd_per_ha)) %>% 
      dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))
    
    idx <- substr(names(fires), start = 1, stop = 4) == "adj_"
    names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]
    
    fires <-
      fires  %>% 
      dplyr::select(id, did, date, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310, 
                    area_log10, area_ha, 
                    starts_with("change_diversity"), starts_with("landcover_diversity"),
                    upper_slopes, lower_slopes, flat, barriers_to_spread, valleys,
                    starts_with("csp_ergo_landforms"), elevation, friction, 
                    friction_walking_only, landform_diversity, 
                    starts_with("lcms_change"), starts_with("lcms_landcover"), 
                    valleys, road_density_mpha, rumple_index, 
                    ndvi, veg_structure_rumple, npl, concurrent_fires, 
                    wind_anisotropy_era5, wind_terrain_anisotropy_era5, 
                    wind_terrain_alignment_era5, max_wind_terrain_alignment_era5, min_wind_terrain_alignment_era5, 
                    max_wind_speed_era5_pct, min_wind_speed_era5_pct, 
                    max_rh_era5_pct, min_rh_era5_pct, max_temp_era5_pct, min_temp_era5_pct, 
                    max_soil_water_era5_pct, min_soil_water_era5_pct, max_vpd_era5_pct, min_vpd_era5_pct, 
                    wind_anisotropy_rtma, wind_terrain_anisotropy_rtma,
                    wind_terrain_alignment_rtma, max_wind_terrain_alignment_rtma, min_wind_terrain_alignment_rtma,
                    max_wind_terrain_alignment_rtma_pct, min_wind_terrain_alignment_rtma_pct,
                    max_wind_speed_rtma_pct, min_wind_speed_rtma_pct,
                    max_wind_filled_gust_rtma_pct, min_wind_filled_gust_rtma_pct,
                    max_rh_rtma_pct, min_rh_rtma_pct, max_temp_rtma_pct, min_temp_rtma_pct,
                    max_vpd_rtma_pct, min_vpd_rtma_pct,
                    starts_with("spei"), pdsi_z, erc_pct, bi_pct, 
                    fm100_pct, fm1000_pct, 
                    sqrt_aoi_tm1, cum_area_ha_tminus1, event_day) %>% 
      as.data.frame()
    
    fires <- 
      fires %>% 
      dplyr::rename(peak_ridge_warm = csp_ergo_landforms_11,
                    peak_ridge = csp_ergo_landforms_12,
                    peak_ridge_cool = csp_ergo_landforms_13,
                    mountain_divide = csp_ergo_landforms_14,
                    cliff = csp_ergo_landforms_15,
                    upper_slope_warm = csp_ergo_landforms_21,
                    upper_slope = csp_ergo_landforms_22,
                    upper_slope_cool = csp_ergo_landforms_23,
                    upper_slope_flat = csp_ergo_landforms_24,
                    lower_slope_warm = csp_ergo_landforms_31,
                    lower_slope = csp_ergo_landforms_32,
                    lower_slope_cool = csp_ergo_landforms_33,
                    lower_slope_flat = csp_ergo_landforms_34,
                    valley = csp_ergo_landforms_41,
                    valley_narrow = csp_ergo_landforms_42) %>% 
      as.data.frame()
    
    
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_01", replacement = "trees")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_03", replacement = "shrubs_trees_mix")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_04", replacement = "grass_forbs_herb_trees_mix")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_05", replacement = "barren_trees_mix")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_07", replacement = "shrubs")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_08", replacement = "grass_forb_herb_shrub_mix")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_09", replacement = "barren_shrub_mix")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_10", replacement = "grass_forb_herb")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_11", replacement = "barren_grass_forb_herb_mix")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_landcover_12", replacement = "barren")
    
    names(fires) <- gsub(x = names(fires), pattern = "lcms_change_01", replacement = "fuel_stable")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_change_02", replacement = "fuel_slow_loss")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_change_03", replacement = "fuel_fast_loss")
    names(fires) <- gsub(x = names(fires), pattern = "lcms_change_04", replacement = "fuel_gain")
    
    return(fires)
  })

# Defining "ewe" as whether daily area of increase was >95th percentile for the biome
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

fires_all <- 
  data.table::rbindlist(fires_all_list) %>% 
  dplyr::group_by(biome_name_daily) %>% 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10),
                ewe = ifelse(area_log10_pct >= pct_threshold, yes = 1, no = 0)) %>% 
  dplyr::group_by(id) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cumu_ewe = cumsum(ewe)) %>%
  dplyr::mutate(cumu_ewe_m1 = ifelse(cumu_ewe > 0, yes = cumu_ewe - 1, no = cumu_ewe)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(biome_shortname = biome_shortnames[match(biome_name_daily, biome_lookup)])

# 95th percentile growth for temperate conifer forests is 1574 ha per day
# 95th percentile growth for Mediterranean Forest, Woodland & Scrub is 2886 ha per day
# 95th percentile growth for temperate grasslands, savannas & shrublands is 173 ha per day
# 95th percentile growth for desert & xeric shrublands is 1014 ha per day
fires_all %>%
  filter(ewe == 1) %>%
  group_by(biome_name_daily) %>%
  filter(area_ha == min(area_ha)) %>%
  select(biome_name_daily, area_ha)
  
# version 1 includes all fires and all available adjusted drivers
data.table::fwrite(x = fires_all, file = analysis_ready_nonspatial_fname)