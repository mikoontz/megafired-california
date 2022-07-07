library(dplyr)
library(data.table)
library(vegan)
library(ggplot2)

system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/fired_daily_random-locations data/out/fired_daily_random-locations")

static_drivers <- 
  list.files(path = "data/out/ee/fire-independent-drivers/randomly-located-fired-polys/", pattern = "static", full.names = TRUE) %>% 
  lapply(FUN = fread) %>% 
  data.table::rbindlist()

static_drivers <- static_drivers[static_drivers$`system:index` != "1_0", ]

static_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                      rumple_index = surf_area / proj_area,
                      road_density_mpha = (road_length_m) / (proj_area / 10000),
                      surf_area = NULL, road_length_m = NULL)]
fluc_drivers <- 
  list.files(path = "data/out/ee/fire-independent-drivers/randomly-located-fired-polys/", pattern = "fluc", full.names = TRUE) %>% 
  lapply(FUN = fread) %>% 
  data.table::rbindlist()

fluc_drivers <- fluc_drivers[fluc_drivers$`system:index` != "1_0", ]

# LCMS Landcovers 2 and 6 are only in Alaska, so we'll remove them here
fluc_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                    veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                    ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                    lcms_landcover_02 = NULL, lcms_landcover_06 = NULL)]


fi_drivers <- 
  merge(static_drivers, fluc_drivers, by = c("did", "date", "id", "samp_id")) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10*10)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / proj_area)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / proj_area)) %>% 
  dplyr::select(-c(lcms_landcover_13, lcms_landcover_14, lcms_landcover_15, lcms_change_05)) %>% 
  dplyr::mutate(landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11, csp_ergo_landforms_12, csp_ergo_landforms_13, csp_ergo_landforms_14, csp_ergo_landforms_15, csp_ergo_landforms_21, csp_ergo_landforms_22, csp_ergo_landforms_23, csp_ergo_landforms_24, csp_ergo_landforms_31, csp_ergo_landforms_32, csp_ergo_landforms_33, csp_ergo_landforms_34, csp_ergo_landforms_41, csp_ergo_landforms_42))) %>%
  # "barriers to spread" include all types of peaks/ridges, mountain/divides, and cliffs but not valleys/narrow valleys
  dplyr::mutate(barriers_to_spread = csp_ergo_landforms_11 + csp_ergo_landforms_12 + csp_ergo_landforms_13 + csp_ergo_landforms_14 + csp_ergo_landforms_15) %>% 
  dplyr::mutate(valleys = csp_ergo_landforms_41 + csp_ergo_landforms_42) %>% 
  dplyr::mutate(upper_slope = csp_ergo_landforms_21 + csp_ergo_landforms_22 + csp_ergo_landforms_23) %>% 
  dplyr::mutate(lower_slope = csp_ergo_landforms_31 + csp_ergo_landforms_32 + csp_ergo_landforms_33) %>% 
  dplyr::mutate(flat = csp_ergo_landforms_24 + csp_ergo_landforms_34) %>% 
  dplyr::mutate(landcover_diversity = vegan::diversity(cbind(lcms_landcover_01, lcms_landcover_03, lcms_landcover_04, lcms_landcover_05, lcms_landcover_07, lcms_landcover_08, lcms_landcover_09, lcms_landcover_10, lcms_landcover_11, lcms_landcover_12)),
                change_diversity = vegan::diversity(cbind(lcms_change_01, lcms_change_02, lcms_change_03, lcms_change_04))) %>% 
  dplyr::select(did, id, date, everything())

fi_drivers <-
  fi_drivers %>% 
  tidyr::pivot_longer(cols = !c(did, date, id, samp_id), names_to = "driver", values_to = "value") %>% 
  dplyr::select(-samp_id) %>% 
  tidyr::nest(data = !c(did, date, id, driver))

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v3.csv"

fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

driver_descriptions <- read.csv(file = "tables/driver-descriptions.csv")

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -starts_with("adj"),
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response) %>%
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha),
                ewe = ifelse(area_ha > 800, yes = 1, no = 0))

idx <- substr(names(fires), start = 1, stop = 4) == "raw_"
names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]

fires_long <-
  fires %>% 
  dplyr::select(-c(x_biggest_poly_3310, y_biggest_poly_3310, daily_area_tminus1_ha, cum_area_ha, cum_area_ha_tminus1, daily_perim_km, daily_perim_tminus1_km, active_fireline_km, samp_id, predicted_aoi_log_cumarea_tm1, predicted_aoi_cumarea_tm1, aoir_modeled_cumarea_tm1, aoir_cumarea_tm1, predicted_aoi_log_sqrtarea_tm1, predicted_aoi_sqrtarea_tm1, aoir_modeled_sqrtarea_tm1, aoir_sqrtarea_tm1, daily_area_ha)) %>% 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10)) %>%
  dplyr::mutate(area_log10_decile = floor(area_log10_pct * 10) + 0.5) %>% 
  dplyr::mutate(area_log10_decile = ifelse(area_log10_decile == 10.5, yes = 9.5, no = area_log10_decile)) %>%
  tidyr::pivot_longer(cols = -c(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily), names_to = "driver", values_to = "value") %>% 
  dplyr::select(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily, everything()) %>% 
  dplyr::left_join(driver_descriptions) %>% 
  dplyr::mutate(driver_desc_ggplot = ifelse(driver_type %in% c("fuel", "topography"), yes = driver_desc, no = driver)) %>% 
  dplyr::mutate(date = as.character(date))

fires_and_fi <-
  merge(x = as.data.table(fi_drivers), y = as.data.table(fires_long), on = c("did", "date", "id", "driver"))

# https://stackoverflow.com/questions/49939936/how-to-do-operations-on-list-columns-in-an-r-data-table-to-output-another-list-c
fires_and_fi[, driver_pct := mapply(data, value, FUN = function(fi_value, fire_value) {ecdf(fi_value$value)(fire_value)})]

# version 4 uses the actual FIRED polygons affine transformed to be on top of 1000 random points throughout the
# temperate conifer forest biome
fires_orig <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-starts_with("adj")) %>% 
  dplyr::mutate(date = as.character(date))

out <- 
  fires_and_fi %>% 
  dplyr::select(-c(data, value, area_log10_pct, area_log10_decile, driver_desc, driver_type, driver_desc_ggplot)) %>% 
  dplyr::mutate(driver = paste0("adj_", driver)) %>% 
  tidyr::pivot_wider(names_from = "driver", values_from = "driver_pct") %>% 
  dplyr::arrange(date, id) %>% 
  dplyr::left_join(fires_orig)

data.table::fwrite(x = out, file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v4.csv")

fires <- 
  data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v4.csv") %>% 
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"),
                -starts_with("raw")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))

idx <- substr(names(fires), start = 1, stop = 4) == "adj_"
names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]

fires_for_amy <-
  fires %>% 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10),
                ewe = ifelse(area_log10_pct >= 0.95, yes = 1, no = 0)) %>% 
  dplyr::select(did, ewe, barriers_to_spread, change_diversity, starts_with("csp_ergo_landforms"), elevation, flat, friction, friction_walking_only, landcover_diversity, landform_diversity, starts_with("lcms_change"), starts_with("lcms_landcover"), lower_slope, ndvi, road_density_mpha, rumple_index, upper_slope, valleys, veg_structure_rumple, npl, npl_at_ignition, concurrent_fires, wind_anisotropy, wind_terrain_anisotropy, wind_terrain_alignment, max_wind_speed_pct, min_wind_speed_pct, max_rh_pct, min_rh_pct, max_temp_pct, min_temp_pct, max_soil_water_pct, min_soil_water_pct, max_vpd_pct, min_vpd_pct, starts_with("spei"), pdsi_z, erc_pct, bi_pct, fm100_pct, fm1000_pct, sqrt_aoi_tm1)


fires_long <-
  fires %>% 
  dplyr::select(-c(x_biggest_poly_3310, y_biggest_poly_3310, daily_area_tminus1_ha, cum_area_ha, cum_area_ha_tminus1, daily_perim_km, daily_perim_tminus1_km, active_fireline_km, samp_id, predicted_aoi_log_cumarea_tm1, predicted_aoi_cumarea_tm1, aoir_modeled_cumarea_tm1, aoir_cumarea_tm1, predicted_aoi_log_sqrtarea_tm1, predicted_aoi_sqrtarea_tm1, aoir_modeled_sqrtarea_tm1, aoir_sqrtarea_tm1, daily_area_ha)) %>% 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10)) %>%
  dplyr::mutate(area_log10_decile = floor(area_log10_pct * 10) + 0.5) %>% 
  dplyr::mutate(area_log10_decile = ifelse(area_log10_decile == 10.5, yes = 9.5, no = area_log10_decile)) %>%
  tidyr::pivot_longer(cols = -c(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily), names_to = "driver", values_to = "value") %>% 
  dplyr::select(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily, everything()) %>% 
  dplyr::left_join(driver_descriptions) %>% 
  dplyr::mutate(driver_desc_ggplot = ifelse(driver_type %in% c("fuel", "topography"), yes = driver_desc, no = driver)) %>% 
  dplyr::mutate(date = as.character(date))


ggplot(dplyr::filter(fires_long, driver_type == "fuel"), aes(x = area_log10, y = value)) +
  geom_smooth(color = "red") +
  facet_wrap(facets = "driver_desc_ggplot") +
  geom_hline(yintercept = 0.5, color = "black") +
  theme_bw()
ggplot(dplyr::filter(fires_long, driver_type == "topography"), aes(x = area_log10, y = value)) +
  geom_smooth(color = "red") +
  facet_wrap(facets = "driver_desc_ggplot") +
  geom_hline(yintercept = 0.5, color = "black") +
  theme_bw()
ggplot(dplyr::filter(fires_long, driver_type == "human"), aes(x = area_log10, y = value)) +
  geom_smooth(color = "red") +
  facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
  geom_hline(yintercept = 0.5, color = "black") +
  theme_bw()

ggplot(dplyr::filter(fires_long, driver_type == "fuel"), aes(x = area_log10_pct, y = value)) +
  geom_smooth(color = "red") +
  facet_wrap(facets = "driver_desc_ggplot") +
  geom_hline(yintercept = 0.5, color = "black") +
  theme_bw()
ggplot(dplyr::filter(fires_long, driver_type == "topography"), aes(x = area_log10_pct, y = value)) +
  geom_smooth(color = "red") +
  facet_wrap(facets = "driver_desc_ggplot") +
  geom_hline(yintercept = 0.5, color = "black") +
  theme_bw()
ggplot(dplyr::filter(fires_long, driver_type == "human"), aes(x = area_log10_pct, y = value)) +
  geom_smooth(color = "red") +
  facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
  geom_hline(yintercept = 0.5, color = "black") +
  theme_bw()
