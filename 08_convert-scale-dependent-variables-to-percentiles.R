library(dplyr)
library(data.table)
library(vegan)
library(ggplot2)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

drivers_version <- "v9"
adjusted_drivers_version <- paste0(drivers_version, "_adjusted")

fired_drivers_fname <- paste0("data/out/FIRED-daily-scale-drivers_california_", drivers_version, ".csv")

for(i in seq_along(biome_shortnames)) {
  
  biome_shortname <- biome_shortnames[i]
  
  adjusted_drivers_fname <- paste0("data/out/FIRED-daily-scale-drivers_california_", biome_shortname, "_", adjusted_drivers_version, ".csv")
  
  static_drivers <- 
    list.files(path = "data/out/ee/fire-independent-drivers/randomly-located-fired-polys/", pattern = "static", full.names = TRUE)
  
  static_drivers <- static_drivers[grepl(pattern = biome_shortname, x = static_drivers)]
  static_drivers <- 
    static_drivers %>% 
    lapply(FUN = fread) %>% 
    data.table::rbindlist()
  
  static_drivers <- static_drivers[static_drivers$`system:index` != "1_0", ]
  
  static_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                        rumple_index = surf_area / proj_area,
                        road_density_mpha = (road_length_m) / (proj_area / 10000),
                        surf_area = NULL, road_length_m = NULL)]
  fluc_drivers <- 
    list.files(path = "data/out/ee/fire-independent-drivers/randomly-located-fired-polys/", pattern = "fluc", full.names = TRUE)
  
  fluc_drivers <- fluc_drivers[grepl(pattern = biome_shortname, x = fluc_drivers)]
  fluc_drivers <-
    fluc_drivers %>% 
    lapply(FUN = fread) %>% 
    data.table::rbindlist()
  
  fluc_drivers <- fluc_drivers[fluc_drivers$`system:index` != "1_0", ]
  
  # LCMS Landcovers 2 and 6 are only in Alaska, so we'll remove them here
  fluc_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                      veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                      ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                      lcms_landcover_02_tm00 = NULL, lcms_landcover_06_tm00 = NULL,
                      lcms_landcover_02_tm01 = NULL, lcms_landcover_06_tm01 = NULL)]
  
  
  fi_drivers <- 
    merge(static_drivers, fluc_drivers, by = c("did", "date", "id", "samp_id")) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10*10)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / proj_area)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / proj_area)) %>% 
    dplyr::select(-c(lcms_landcover_13_tm00, lcms_landcover_14_tm00, lcms_landcover_15_tm00, lcms_change_05_tm00),
                  -c(lcms_landcover_13_tm01, lcms_landcover_14_tm01, lcms_landcover_15_tm01, lcms_change_05_tm01)) %>% 
    dplyr::mutate(landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11, csp_ergo_landforms_12, csp_ergo_landforms_13, csp_ergo_landforms_14, csp_ergo_landforms_15, csp_ergo_landforms_21, csp_ergo_landforms_22, csp_ergo_landforms_23, csp_ergo_landforms_24, csp_ergo_landforms_31, csp_ergo_landforms_32, csp_ergo_landforms_33, csp_ergo_landforms_34, csp_ergo_landforms_41, csp_ergo_landforms_42))) %>%
    # "barriers to spread" include all types of peaks/ridges, mountain/divides, and cliffs but not valleys/narrow valleys
    dplyr::mutate(barriers_to_spread = csp_ergo_landforms_11 + csp_ergo_landforms_12 + csp_ergo_landforms_13 + csp_ergo_landforms_14 + csp_ergo_landforms_15) %>% 
    dplyr::mutate(valleys = csp_ergo_landforms_41 + csp_ergo_landforms_42) %>% 
    dplyr::mutate(upper_slopes = csp_ergo_landforms_21 + csp_ergo_landforms_22 + csp_ergo_landforms_23) %>% 
    dplyr::mutate(lower_slopes = csp_ergo_landforms_31 + csp_ergo_landforms_32 + csp_ergo_landforms_33) %>% 
    dplyr::mutate(flat = csp_ergo_landforms_24 + csp_ergo_landforms_34) %>% 
    dplyr::mutate(landcover_diversity_tm00 = vegan::diversity(cbind(lcms_landcover_01_tm00, lcms_landcover_03_tm00, lcms_landcover_04_tm00, lcms_landcover_05_tm00, lcms_landcover_07_tm00, lcms_landcover_08_tm00, lcms_landcover_09_tm00, lcms_landcover_10_tm00, lcms_landcover_11_tm00, lcms_landcover_12_tm00)),
                  change_diversity_tm00 = vegan::diversity(cbind(lcms_change_01_tm00, lcms_change_02_tm00, lcms_change_03_tm00, lcms_change_04_tm00)),
                  landcover_diversity_tm01 = vegan::diversity(cbind(lcms_landcover_01_tm01, lcms_landcover_03_tm01, lcms_landcover_04_tm01, lcms_landcover_05_tm01, lcms_landcover_07_tm01, lcms_landcover_08_tm01, lcms_landcover_09_tm01, lcms_landcover_10_tm01, lcms_landcover_11_tm01, lcms_landcover_12_tm01)),
                  change_diversity_tm01 = vegan::diversity(cbind(lcms_change_01_tm01, lcms_change_02_tm01, lcms_change_03_tm01, lcms_change_04_tm01)),
                  change_diversity_tm02 = vegan::diversity(cbind(lcms_change_01_tm02, lcms_change_02_tm02, lcms_change_03_tm02, lcms_change_04_tm02)),
                  change_diversity_tm03 = vegan::diversity(cbind(lcms_change_01_tm03, lcms_change_02_tm03, lcms_change_03_tm03, lcms_change_04_tm03)),
                  change_diversity_tm04 = vegan::diversity(cbind(lcms_change_01_tm04, lcms_change_02_tm04, lcms_change_03_tm04, lcms_change_04_tm04)),
                  change_diversity_tm05 = vegan::diversity(cbind(lcms_change_01_tm05, lcms_change_02_tm05, lcms_change_03_tm05, lcms_change_04_tm05))) %>% 
    dplyr::select(did, id, date, everything())
  
  fi_drivers_long <-
    fi_drivers %>% 
    tidyr::pivot_longer(cols = !c(did, date, id, samp_id), names_to = "driver", values_to = "value") %>% 
    dplyr::select(-samp_id) %>% 
    tidyr::nest(data = !c(did, date, id, driver)) %>% 
    as.data.table()
  
  fi_drivers_long[, date := as.Date(date)]
  
  # Get drivers as measured from within fire footprints
  fired_drivers <-
    data.table::fread(fired_drivers_fname) %>% 
    dplyr::select(-c(lcms_landcover_13_tm00, lcms_landcover_14_tm00, lcms_landcover_15_tm00, lcms_change_05_tm00),
                  -c(lcms_landcover_13_tm01, lcms_landcover_14_tm01, lcms_landcover_15_tm01, lcms_change_05_tm01)) %>% 
    dplyr::mutate(landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11, csp_ergo_landforms_12, csp_ergo_landforms_13, csp_ergo_landforms_14, csp_ergo_landforms_15, csp_ergo_landforms_21, csp_ergo_landforms_22, csp_ergo_landforms_23, csp_ergo_landforms_24, csp_ergo_landforms_31, csp_ergo_landforms_32, csp_ergo_landforms_33, csp_ergo_landforms_34, csp_ergo_landforms_41, csp_ergo_landforms_42))) %>%
    # "barriers to spread" include all types of peaks/ridges, mountain/divides, and cliffs but not valleys/narrow valleys
    dplyr::mutate(barriers_to_spread = csp_ergo_landforms_11 + csp_ergo_landforms_12 + csp_ergo_landforms_13 + csp_ergo_landforms_14 + csp_ergo_landforms_15) %>% 
    dplyr::mutate(valleys = csp_ergo_landforms_41 + csp_ergo_landforms_42) %>% 
    dplyr::mutate(upper_slopes = csp_ergo_landforms_21 + csp_ergo_landforms_22 + csp_ergo_landforms_23) %>% 
    dplyr::mutate(lower_slopes = csp_ergo_landforms_31 + csp_ergo_landforms_32 + csp_ergo_landforms_33) %>% 
    dplyr::mutate(flat = csp_ergo_landforms_24 + csp_ergo_landforms_34) %>% 
    dplyr::mutate(landcover_diversity_tm00 = vegan::diversity(cbind(lcms_landcover_01_tm00, lcms_landcover_03_tm00, lcms_landcover_04_tm00, lcms_landcover_05_tm00, lcms_landcover_07_tm00, lcms_landcover_08_tm00, lcms_landcover_09_tm00, lcms_landcover_10_tm00, lcms_landcover_11_tm00, lcms_landcover_12_tm00)),
                  change_diversity_tm00 = vegan::diversity(cbind(lcms_change_01_tm00, lcms_change_02_tm00, lcms_change_03_tm00, lcms_change_04_tm00)),
                  landcover_diversity_tm01 = vegan::diversity(cbind(lcms_landcover_01_tm01, lcms_landcover_03_tm01, lcms_landcover_04_tm01, lcms_landcover_05_tm01, lcms_landcover_07_tm01, lcms_landcover_08_tm01, lcms_landcover_09_tm01, lcms_landcover_10_tm01, lcms_landcover_11_tm01, lcms_landcover_12_tm01)),
                  change_diversity_tm01 = vegan::diversity(cbind(lcms_change_01_tm01, lcms_change_02_tm01, lcms_change_03_tm01, lcms_change_04_tm01)),
                  change_diversity_tm02 = vegan::diversity(cbind(lcms_change_01_tm02, lcms_change_02_tm02, lcms_change_03_tm02, lcms_change_04_tm02)),
                  change_diversity_tm03 = vegan::diversity(cbind(lcms_change_01_tm03, lcms_change_02_tm03, lcms_change_03_tm03, lcms_change_04_tm03)),
                  change_diversity_tm04 = vegan::diversity(cbind(lcms_change_01_tm04, lcms_change_02_tm04, lcms_change_03_tm04, lcms_change_04_tm04)),
                  change_diversity_tm05 = vegan::diversity(cbind(lcms_change_01_tm05, lcms_change_02_tm05, lcms_change_03_tm05, lcms_change_04_tm05))) %>% 
    dplyr::select(did, id, date, everything())
  
  fired_long <-
    fired_drivers %>% 
    tidyr::pivot_longer(cols = !c(did, date, id, samp_id), names_to = "driver", values_to = "raw") %>% 
    dplyr::select(-samp_id) %>% 
    as.data.table()
  
  # merge fire-independent drivers data with the fired drivers data
  fires_and_fi <-
    merge(x = fi_drivers_long, y = fired_long, on = c("did", "date", "id", "driver"))
  
  # convert raw proportional cover values to percentils based on comparing measured value within the
  # fire footprint to the measured value if you were to re-locate that fire footprint to 500 other 
  # locations within the biome
  # https://stackoverflow.com/questions/49939936/how-to-do-operations-on-list-columns-in-an-r-data-table-to-output-another-list-c
  fires_and_fi[, adj := mapply(data, raw, FUN = function(fi_value, fire_value) {ecdf(fi_value$value)(fire_value)})]
  
  fires_and_fi_wide <- 
    fires_and_fi %>% 
    dplyr::select(-data) %>%
    tidyr::pivot_wider(id_cols = c(did, id, date), names_from = driver, values_from = c(raw, adj)) %>% 
    dplyr::arrange(date, id)
  
  # Join together the other driver variables, some id columns (ig_year, biome_name, event_day, eco_name), and 
  other_vars <- setdiff(names(fired_drivers), unique(fires_and_fi$driver))
  other_vars <- other_vars[-which(other_vars == "samp_id")]
  
  fired_other_drivers <-
    fired_drivers[, ..other_vars]
  
  fired_daily_response <- 
    data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv") %>% 
    dplyr::rename(area_ha = daily_area_ha) %>% 
    dplyr::mutate(area_log10 = log10(area_ha)) %>% 
    dplyr::select(did, date, id, event_day, area_ha, area_log10, biome_name, eco_name, biome_name_daily, eco_name_daily)
  
  out <- merge(x = fired_other_drivers, y = fired_daily_response, on = c("did", "date", "id"))
  out <- 
    merge(x = fires_and_fi_wide, y = out, on = c("did", "date", "id")) %>% 
    dplyr::select(did, id, date, area_ha, area_log10, x_biggest_poly_3310, y_biggest_poly_3310, biome_name, eco_name, biome_name_daily, eco_name_daily, x_3310, y_3310, biggest_poly_area_ha, biggest_poly_frac, event_day, dplyr::everything()) %>% 
    dplyr::arrange(date, id)
  
  # version 4 uses the actual FIRED polygons affine transformed to be on top of 1000 random points throughout the
  # temperate conifer forest biome
  # version 5 uses the actual FIRED polygons affine transformed to be on top of 500 random points throughout each biome
  # version 6 cleans up the workflow and only uses data properly and cleanly created data
  # version 7 uses LCMS data from the year before the fire rather than the year of the fire
  # Versions 9 and later use the same version as the output version from previous effort, but with "adjusted" appended to filename
  
  data.table::fwrite(x = out, file = adjusted_drivers_fname)
}
