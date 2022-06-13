# build relationship between area and driver values, independent of fire

library(dplyr)
library(sf)
library(data.table)
library(ggplot2)
library(lubridate)
library(vegan)

### ---- Read in FIRED data and FIRED drivers data
fired_daily_orig <-
  sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::rename(ig_year = ignition_year) %>% 
  dplyr::select(did, id, date, ig_year) %>% 
  dplyr::as_tibble()

daily_resolve <- 
  read.csv("data/out/fired_daily_resolve.csv") %>% 
  dplyr::mutate(date = as.Date(date)) %>% 
  dplyr::as_tibble()

fired_daily_df <- 
  sf::st_read("data/out/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::mutate(date = as.Date(date)) %>% 
  dplyr::left_join(fired_daily_orig, by = c("did", "id", "date")) %>% 
  dplyr::left_join(daily_resolve, by = c("did", "id", "date", "samp_id")) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::as_tibble()

fired_daily_drivers <- 
  sf::st_read("data/out/FIRED-daily-scale-drivers_california_v7.gpkg") %>% 
  dplyr::mutate(area_log10 = log10(proj_area)) %>% 
  dplyr::left_join(fired_daily_df, by = c("did", "id", "date", "samp_id")) %>% 
  dplyr::select(-c(lcms_landcover_13, lcms_landcover_14, lcms_landcover_15, lcms_change_05)) %>% 
  dplyr::mutate(landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11, csp_ergo_landforms_12, csp_ergo_landforms_13, csp_ergo_landforms_14, csp_ergo_landforms_15, csp_ergo_landforms_21, csp_ergo_landforms_22, csp_ergo_landforms_23, csp_ergo_landforms_24, csp_ergo_landforms_31, csp_ergo_landforms_32, csp_ergo_landforms_33, csp_ergo_landforms_34, csp_ergo_landforms_41, csp_ergo_landforms_42))) %>%
  # "barriers to spread" include all types of peaks/ridges, mountain/divides, and cliffs but not valleys/narrow valleys
  dplyr::mutate(barriers_to_spread = csp_ergo_landforms_11 + csp_ergo_landforms_12 + csp_ergo_landforms_13 + csp_ergo_landforms_14 + csp_ergo_landforms_15) %>% 
  dplyr::mutate(valleys = csp_ergo_landforms_41 + csp_ergo_landforms_42) %>% 
  dplyr::mutate(upper_slope = csp_ergo_landforms_21 + csp_ergo_landforms_22 + csp_ergo_landforms_23) %>% 
  dplyr::mutate(lower_slope = csp_ergo_landforms_31 + csp_ergo_landforms_32 + csp_ergo_landforms_33) %>% 
  dplyr::mutate(flat = csp_ergo_landforms_24 + csp_ergo_landforms_34) %>% 
  dplyr::mutate(landcover_diversity = vegan::diversity(cbind(lcms_landcover_01, lcms_landcover_03, lcms_landcover_04, lcms_landcover_05, lcms_landcover_07, lcms_landcover_08, lcms_landcover_09, lcms_landcover_10, lcms_landcover_11, lcms_landcover_12)),
                change_diversity = vegan::diversity(cbind(lcms_change_01, lcms_change_02, lcms_change_03, lcms_change_04)))

### --- manipulate the information coming from fire-independent polygons
summarize_static_and_fluc_drivers <- function(static_drivers_fname, fluc_drivers_fname, fire_independent_polys_fname) {
  static_drivers <- data.table::fread(static_drivers_fname)
  static_drivers <- static_drivers[-1, ]
  static_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                        rumple_index = surf_area / proj_area,
                        road_density_mpha = (road_length_m) / (proj_area / 10000),
                        surf_area = NULL, road_length_m = NULL)]
  
  # Landcovers 2 and 6 are only for Alaska, 
  # Landcover 13 is snow/ice and Landcover 14 is water
  # Change 5 and Landcover 15 are 'non-processing area masks'
  fluc_drivers <- data.table::fread(fluc_drivers_fname)
  fluc_drivers <- fluc_drivers[-1, ]
  fluc_drivers[, `:=`(.geo = NULL, `system:index` = NULL, 
                      veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                      ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                      lcms_landcover_02 = NULL, lcms_landcover_06 = NULL, 
                      lcms_change_05 = NULL,
                      lcms_landcover_13 = NULL, lcms_landcover_14 = NULL, lcms_landcover_15 = NULL)]
  
  random_polys <- 
    sf::st_read(fire_independent_polys_fname) %>% 
    dplyr::mutate(area = as.numeric(sf::st_area(.))) %>% 
    sf::st_drop_geometry() %>% 
    as.data.table()
  
  random_polys[, date := NULL]
  
  static_drivers_with_polys <- random_polys[static_drivers, on = c("did", "id", "samp_id")]
  fluc_drivers_with_polys <- random_polys[fluc_drivers, on = c("did", "id", "samp_id")]
  
  csp_ergo_landforms_desc <-
    tribble(~code, ~color,	~driver_desc,
            "11", "#141414", "Peak/ridge (warm)",
            "12", "#383838", "Peak/ridge",
            "13", "#808080", "Peak/ridge (cool)",
            "14", "#EBEB8F", "Mountain/divide",
            "15", "#F7D311", "Cliff",
            "21", "#AA0000", "Upper slope (warm)",
            "22", "#D89382", "Upper slope",
            "23", "#DDC9C9", "Upper slope (cool)",
            "24", "#DCCDCE", "Upper slope (flat)",
            "31", "#1C6330", "Lower slope (warm)",
            "32", "#68AA63", "Lower slope",
            "33", "#B5C98E", "Lower slope (cool)",
            "34", "#E1F0E5", "Lower slope (flat)",
            "41", "#a975ba", "Valley",
            "42", "#6f198c", "Valley (narrow)") %>% 
    dplyr::mutate(driver = paste0("csp_ergo_landforms_", code))
  
  out_static <-
    static_drivers_with_polys %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10*10)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / area)) %>% 
    # add shannon diversity index of landcover and of landforms
    # -sum p_i log(b) p_i
    dplyr::mutate(landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11, csp_ergo_landforms_12, csp_ergo_landforms_13, csp_ergo_landforms_14, csp_ergo_landforms_15, csp_ergo_landforms_21, csp_ergo_landforms_22, csp_ergo_landforms_23, csp_ergo_landforms_24, csp_ergo_landforms_31, csp_ergo_landforms_32, csp_ergo_landforms_33, csp_ergo_landforms_34, csp_ergo_landforms_41, csp_ergo_landforms_42))) %>%
    # "barriers to spread" include all types of peaks/ridges, mountain/divides, and cliffs but not valleys/narrow valleys
    dplyr::mutate(barriers_to_spread = csp_ergo_landforms_11 + csp_ergo_landforms_12 + csp_ergo_landforms_13 + csp_ergo_landforms_14 + csp_ergo_landforms_15) %>% 
    dplyr::mutate(valleys = csp_ergo_landforms_41 + csp_ergo_landforms_42) %>% 
    dplyr::mutate(upper_slope = csp_ergo_landforms_21 + csp_ergo_landforms_22 + csp_ergo_landforms_23) %>% 
    dplyr::mutate(lower_slope = csp_ergo_landforms_31 + csp_ergo_landforms_32 + csp_ergo_landforms_33) %>% 
    dplyr::mutate(flat = csp_ergo_landforms_24 + csp_ergo_landforms_34) %>% 
    dplyr::mutate(area_log10 = log10(area)) %>%
    dplyr::mutate(area_log10_round = round(area_log10, 1)) %>% 
    dplyr::mutate(date = lubridate::seconds_to_period(date / 1000) + lubridate::ymd("1970-01-01") - lubridate::hours(8)) %>% 
    dplyr::select(did, id, date, everything()) %>% 
    tidyr::pivot_longer(cols = c(-did, -id, -date, -samp_id, -area, -area_log10, -area_log10_round), names_to = "driver", values_to = "value") %>% 
    dplyr::filter(driver != "proj_area") %>% 
    dplyr::left_join(csp_ergo_landforms_desc, by = "driver") %>% 
    dplyr::select(-code, -color) %>% 
    dplyr::mutate(driver_desc = ifelse(is.na(driver_desc), yes = driver, no = driver_desc)) %>% 
    dplyr::mutate(driver_desc = tolower(driver_desc))
  
  # # Landscape Change Monitoring System: Change
  lcms_change_desc <-
    tribble(~code, ~color, ~driver_desc,
            "01", "#3d4551", "Stable",
            "02", "#f39268", "Slow Loss",
            "03", "#d54309", "Fast Loss",
            "04", "#00a398", "Gain",
            "05", "#1B1716", "Non-Processing Area Mask") %>% 
    dplyr::mutate(driver = paste0("lcms_change_", code))
  
  lcms_landcover_desc <-
    tribble(~code, ~color, ~driver_desc,
            "01",	"#005e00", "Trees",
            "02",	"#008000", "Tall Shrubs & Trees Mix (SEAK Only)",
            "03",	"#00cc00", "Shrubs & Trees Mix",
            "04",	"#b3ff1a", "Grass/Forb/Herb & Trees Mix",
            "05",	"#99ff99", "Barren & Trees Mix",
            "06",	"#b30088", "Tall Shrubs (SEAK Only)",
            "07",	"#e68a00", "Shrubs",
            "08",	"#ffad33", "Grass/Forb/Herb & Shrubs Mix",
            "09",	"#ffe0b3", "Barren & Shrubs Mix",
            "10",	"#ffff00", "Grass/Forb/Herb",
            "11",	"#AA7700", "Barren & Grass/Forb/Herb Mix",
            "12",	"#d3bf9b", "Barren or Impervious",
            "13",	"#ffffff", "Snow or Ice",
            "14",	"#4780f3", "Water",
            "15",	"#1B1716", "Non-Processing Area Mask") %>% 
    dplyr::mutate(driver = paste0("lcms_landcover_", code))
  
  lcms_desc <- rbind(lcms_change_desc, lcms_landcover_desc)
  
  out_fluc <-
    fluc_drivers_with_polys %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / area)) %>%
    dplyr::mutate(landcover_diversity = vegan::diversity(cbind(lcms_landcover_01, lcms_landcover_03, lcms_landcover_04, lcms_landcover_05, lcms_landcover_07, lcms_landcover_08, lcms_landcover_09, lcms_landcover_10, lcms_landcover_11, lcms_landcover_12)),
                  change_diversity = vegan::diversity(cbind(lcms_change_01, lcms_change_02, lcms_change_03, lcms_change_04))) %>% 
    dplyr::mutate(area_log10 = log10(area)) %>%
    dplyr::mutate(area_log10_round = round(area_log10, 1)) %>% 
    dplyr::mutate(date = lubridate::seconds_to_period(date / 1000) + lubridate::ymd("1970-01-01") - lubridate::hours(8)) %>% 
    dplyr::mutate(ig_year = lubridate::year(date)) %>% 
    dplyr::select(did, id, date, ig_year, everything()) %>% 
    tidyr::pivot_longer(cols = -c(did, id, date, ig_year, samp_id, area, area_log10, area_log10_round), names_to = "driver", values_to = "value") %>% 
    dplyr::left_join(lcms_desc, by = "driver") %>% 
    dplyr::select(-code, -color) %>% 
    dplyr::mutate(driver_desc = ifelse(is.na(driver_desc), yes = driver, no = driver_desc)) %>% 
    dplyr::mutate(driver_desc = tolower(driver_desc))
  
  return(list(out_static = out_static, out_fluc = out_fluc))
}

### ---
get_quantiles <- function(x, probs) {
  quantiles <- quantile(x, probs = probs)
  data.frame(quantile = probs, value = quantiles)
}

### ---

summarize_fire_independent_scaling <- function(fire_independent_drivers_out) {
  
  area_quants_static <-
    fire_independent_drivers_out$out_static %>%
    group_by(area_log10_round, driver, driver_desc) %>% 
    summarize(summary = get_quantiles(value, probs = c(seq(0, 1, by = 0.025)))) %>% 
    tidyr::unpack(cols = "summary") %>% 
    dplyr::mutate(quantile = as.factor(quantile))
  
  area_quants_fluc <-
    fire_independent_drivers_out$out_fluc %>%
    group_by(area_log10_round, ig_year, driver, driver_desc) %>% 
    summarize(summary = get_quantiles(value, probs = c(seq(0, 1, by = 0.025)))) %>% 
    tidyr::unpack(cols = "summary") %>% 
    dplyr::mutate(quantile = as.factor(quantile))
  
  return(list(area_quants_static = area_quants_static, area_quants_fluc = area_quants_fluc))
}

### --- 

find_nonzero_medians <- function(fire_independent_drivers_out) {
  
  fi_summary <- summarize_fire_independent_scaling(fire_independent_drivers_out = fire_independent_drivers_out)
  
  # Looking for non-zero medians at the smallest fire-independent polygon size
  nonzero_medians_static <-
    fi_summary$area_quants_static %>% 
    group_by(driver) %>% 
    filter(area_log10_round == min(area_log10_round) & quantile == 0.5) %>% 
    arrange(driver) %>% 
    dplyr::ungroup()
  
  nonzero_medians_fluc <- 
    fi_summary$area_quants_fluc %>% 
    group_by(driver) %>% 
    filter(area_log10_round == min(area_log10_round) & quantile == 0.5) %>% 
    filter(value == min(value)) %>% 
    arrange(driver) %>% 
    slice(1)
  
  return(list(nonzero_medians_static = nonzero_medians_static, nonzero_medians_fluc = nonzero_medians_fluc))
}
#############################################################

adjust_scale_dependent_drivers <- function(fire_independent_drivers_out, fired_daily_drivers_biome) {
  # https://en.wikipedia.org/wiki/Median_absolute_deviation
  # MAD might be mostly useless here, as it will often be 0 for small polygons
  fire_independent_static_driver_medians <- 
    fire_independent_drivers_out$out_static %>%
    group_by(area_log10_round, driver, driver_desc) %>%
    summarize(median = median(value), mad = median(abs(value - median(value)))) %>% 
    dplyr::mutate(ig_year = list(2003:2020)) %>% 
    tidyr::unnest(cols = "ig_year") %>% 
    dplyr::select(ig_year, area_log10_round, dplyr::everything()) %>% 
    dplyr::ungroup()
  
  fire_independent_fluc_driver_medians <- 
    fire_independent_drivers_out$out_fluc %>%
    group_by(ig_year, area_log10_round, driver, driver_desc) %>% 
    summarize(median = median(value), mad = median(abs(value - median(value)))) %>% 
    dplyr::ungroup()
  
  fire_indep_meds <- rbind(fire_independent_static_driver_medians, fire_independent_fluc_driver_medians)
  
  fired_drivers_long <-
    fired_daily_drivers_biome %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(did, ig_year, area_log10, unique(fire_indep_meds$driver)) %>% 
    tidyr::pivot_longer(cols = -c(did, ig_year, area_log10), names_to = "driver", values_to = "value")
  
  (start_time <- Sys.time())
  fired_drivers_long <-
    fired_drivers_long %>% 
    dplyr::rename(fired_area_log10 = area_log10) %>% 
    dplyr::left_join(fire_indep_meds, by = c("ig_year", "driver")) %>% 
    dplyr::mutate(area_diff = area_log10_round - fired_area_log10,
                  area_diff_abs = abs(area_log10_round - fired_area_log10)) %>% 
    dplyr::group_by(did, ig_year, driver, driver_desc) %>% 
    dplyr::arrange(area_diff_abs) %>% 
    dplyr::slice(1:2) %>% 
    dplyr::mutate(wgt = 1 - (area_diff_abs / sum(area_diff_abs))) %>% 
    dplyr::summarize(fired_area_log10 = unique(fired_area_log10),
                     value = unique(value),
                     E_value = weighted.mean(x = median, w = wgt))
  (end_time <- Sys.time())
  (difftime(end_time, start_time, units = "mins"))
  
  fired_drivers_long <-
    fired_drivers_long %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(diff_median = value - E_value)
  
  fired_drivers_wide <-
    fired_drivers_long %>% 
    dplyr::select(-fired_area_log10, -value, -E_value, -driver_desc) %>% 
    tidyr::pivot_wider(id_cols = c(did, ig_year), names_from = "driver", values_from = diff_median)
  
  out <- 
    fired_daily_drivers_biome %>% 
    dplyr::select(-(fired_drivers_long$driver)) %>% 
    dplyr::left_join(fired_drivers_wide, by = c("did", "ig_year")) %>% 
    dplyr::rename(geometry = geom)
  
  return(out)
}

###################

### --- Temperate Conifer Forests
tcf_out <- summarize_static_and_fluc_drivers(static_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-tcf-static-drivers_california.csv",
                                             fluc_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-tcf-fluctuating-drivers_california.csv",
                                             fire_independent_polys_fname = "data/out/fire-independent-polygons_tcf_ca_v2.gpkg"
)

tcf_nonzero_medians <- find_nonzero_medians(fire_independent_drivers_out = tcf_out)

### What are the drivers within each of the California biomes that have non-zero median values
### at the smallest polygon size of the fire-independent polygons?

tcf_nonzero_medians_static <- tcf_nonzero_medians$nonzero_medians_static
tcf_final_static_drivers <- tcf_nonzero_medians_static$driver
# tcf_static_drivers_exclude <- c("lower_slope", "upper_slope", "friction_walking_only", tcf_nonzero_medians_static$driver[tcf_nonzero_medians_static$value == 0])
tcf_static_drivers_exclude <- c("csp_ergo_landforms_21", "csp_ergo_landforms_22", "csp_ergo_landforms_31", "csp_ergo_landforms_32", "friction_walking_only", tcf_nonzero_medians_static$driver[tcf_nonzero_medians_static$value == 0])
tcf_final_static_drivers <- tcf_final_static_drivers[!(tcf_final_static_drivers %in% tcf_static_drivers_exclude)]

# All fluctuating drivers perhaps have high enough expectation of proportional
# landcovers
tcf_nonzero_medians_fluc <- tcf_nonzero_medians$nonzero_medians_fluc
tcf_final_fluc_drivers <- tcf_nonzero_medians_fluc$driver
tcf_fluc_drivers_exclude <- c("lcms_change_01", "lcms_change_04", "lcms_landcover_01", "lcms_landcover_04", tcf_nonzero_medians_fluc$driver[tcf_nonzero_medians_fluc$value == 0])
tcf_final_fluc_drivers <- tcf_final_fluc_drivers[!(tcf_final_fluc_drivers %in% tcf_fluc_drivers_exclude)]

tcf_out$out_static <- 
  tcf_out$out_static %>% 
  dplyr::filter(driver %in% tcf_final_static_drivers)

tcf_out$out_fluc <-
  tcf_out$out_fluc %>% 
  dplyr::filter(driver %in% tcf_final_fluc_drivers)

tcf_drivers <-
  fired_daily_drivers %>% 
  dplyr::filter(biome_name == "Temperate Conifer Forests") %>% 
  dplyr::select(-{{tcf_static_drivers_exclude}}) %>% 
  dplyr::select(-{{tcf_fluc_drivers_exclude}})

tcf_drivers_adjusted <-
  adjust_scale_dependent_drivers(fire_independent_drivers_out = tcf_out, 
                                 fired_daily_drivers = tcf_drivers)

sf::st_write(obj = tcf_drivers_adjusted, dsn = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v1.gpkg", delete_dsn = TRUE)
data.table::fwrite(x = sf::st_drop_geometry(tcf_drivers_adjusted), file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v1.csv")

### --- Mediterranean Forests, Woodlands & Scrub

mfws_out <- summarize_static_and_fluc_drivers(static_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-mfws-static-drivers_california.csv",
                                              fluc_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-mfws-fluctuating-drivers_california.csv",
                                              fire_independent_polys_fname = "data/out/fire-independent-polygons_mfws_ca_v2.gpkg"
)

mfws_nonzero_medians <- find_nonzero_medians(fire_independent_drivers_out = mfws_out)

### What are the drivers within each of the California biomes that have non-zero median values
### at the smallest polygon size of the fire-independent polygons?

mfws_nonzero_medians_static <- mfws_nonzero_medians$nonzero_medians_static
mfws_final_static_drivers <- mfws_nonzero_medians_static$driver
# mfws_static_drivers_exclude <- c("lower_slope", "upper_slope", "friction_walking_only", mfws_nonzero_medians_static$driver[mfws_nonzero_medians_static$value == 0])
mfws_static_drivers_exclude <- c("csp_ergo_landforms_21", "csp_ergo_landforms_22", "csp_ergo_landforms_31", "csp_ergo_landforms_32", "friction_walking_only", mfws_nonzero_medians_static$driver[mfws_nonzero_medians_static$value == 0])
mfws_final_static_drivers <- mfws_final_static_drivers[!(mfws_final_static_drivers %in% mfws_static_drivers_exclude)]

# All fluctuating drivers perhaps have high enough expectation of proportional
# landcovers
mfws_nonzero_medians_fluc <- mfws_nonzero_medians$nonzero_medians_fluc
mfws_final_fluc_drivers <- mfws_nonzero_medians_fluc$driver
mfws_fluc_drivers_exclude <- c("lcms_change_01", "lcms_change_04", "lcms_landcover_01", "lcms_landcover_04", mfws_nonzero_medians_fluc$driver[mfws_nonzero_medians_fluc$value == 0])
mfws_final_fluc_drivers <- mfws_final_fluc_drivers[!(mfws_final_fluc_drivers %in% mfws_fluc_drivers_exclude)]

mfws_out$out_static <- 
  mfws_out$out_static %>% 
  dplyr::filter(driver %in% mfws_final_static_drivers)

mfws_out$out_fluc <-
  mfws_out$out_fluc %>% 
  dplyr::filter(driver %in% mfws_final_fluc_drivers)

mfws_drivers <-
  fired_daily_drivers %>% 
  dplyr::filter(biome_name == "Mediterranean Forests, Woodlands & Scrub") %>% 
  dplyr::select(-{{mfws_static_drivers_exclude}}) %>% 
  dplyr::select(-{{mfws_fluc_drivers_exclude}})

mfws_drivers_adjusted <-
  adjust_scale_dependent_drivers(fire_independent_drivers_out = mfws_out, 
                                 fired_daily_drivers = mfws_drivers)

sf::st_write(obj = mfws_drivers_adjusted, dsn = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v1.gpkg", delete_dsn = TRUE)
data.table::fwrite(x = sf::st_drop_geometry(mfws_drivers_adjusted), file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v1.csv")


### --- Temperate Grasslands, Savannas & Shrublands

tgss_out <- summarize_static_and_fluc_drivers(static_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-tgss-static-drivers_california.csv",
                                              fluc_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-tgss-fluctuating-drivers_california.csv",
                                              fire_independent_polys_fname = "data/out/fire-independent-polygons_tgss_ca_v2.gpkg"
)

tgss_nonzero_medians <- find_nonzero_medians(fire_independent_drivers_out = tgss_out)

### What are the drivers within each of the California biomes that have non-zero median values
### at the smallest polygon size of the fire-independent polygons?

tgss_nonzero_medians_static <- tgss_nonzero_medians$nonzero_medians_static
tgss_final_static_drivers <- tgss_nonzero_medians_static$driver
# tgss_static_drivers_exclude <- c("lower_slope", "upper_slope", "friction_walking_only", tgss_nonzero_medians_static$driver[tgss_nonzero_medians_static$value == 0])
tgss_static_drivers_exclude <- c("csp_ergo_landforms_21", "csp_ergo_landforms_22", "csp_ergo_landforms_31", "csp_ergo_landforms_32", "friction_walking_only", tgss_nonzero_medians_static$driver[tgss_nonzero_medians_static$value == 0])
tgss_final_static_drivers <- tgss_final_static_drivers[!(tgss_final_static_drivers %in% tgss_static_drivers_exclude)]

# All fluctuating drivers perhaps have high enough expectation of proportional
# landcovers
tgss_nonzero_medians_fluc <- tgss_nonzero_medians$nonzero_medians_fluc
tgss_final_fluc_drivers <- tgss_nonzero_medians_fluc$driver
tgss_fluc_drivers_exclude <- c("lcms_change_01", "lcms_change_04", "lcms_landcover_01", "lcms_landcover_04", tgss_nonzero_medians_fluc$driver[tgss_nonzero_medians_fluc$value == 0])
tgss_final_fluc_drivers <- tgss_final_fluc_drivers[!(tgss_final_fluc_drivers %in% tgss_fluc_drivers_exclude)]

tgss_out$out_static <- 
  tgss_out$out_static %>% 
  dplyr::filter(driver %in% tgss_final_static_drivers)

tgss_out$out_fluc <-
  tgss_out$out_fluc %>% 
  dplyr::filter(driver %in% tgss_final_fluc_drivers)

tgss_drivers <-
  fired_daily_drivers %>% 
  dplyr::filter(biome_name == "Temperate Grasslands, Savannas & Shrublands") %>% 
  dplyr::select(-{{tgss_static_drivers_exclude}}) %>% 
  dplyr::select(-{{tgss_fluc_drivers_exclude}})

tgss_drivers_adjusted <-
  adjust_scale_dependent_drivers(fire_independent_drivers_out = tgss_out, 
                                 fired_daily_drivers = tgss_drivers)

sf::st_write(obj = tgss_drivers_adjusted, dsn = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v1.gpkg", delete_dsn = TRUE)
data.table::fwrite(x = sf::st_drop_geometry(tgss_drivers_adjusted), file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v1.csv")


### --- Deserts & Xeric Shrublands

dxs_out <- summarize_static_and_fluc_drivers(static_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-dxs-static-drivers_california.csv",
                                             fluc_drivers_fname = "data/out/ee/fire-independent-drivers/fire-independent-dxs-fluctuating-drivers_california.csv",
                                             fire_independent_polys_fname = "data/out/fire-independent-polygons_dxs_ca_v2.gpkg"
)

dxs_nonzero_medians <- find_nonzero_medians(fire_independent_drivers_out = dxs_out)

### What are the drivers within each of the California biomes that have non-zero median values
### at the smallest polygon size of the fire-independent polygons?

dxs_nonzero_medians_static <- dxs_nonzero_medians$nonzero_medians_static
dxs_final_static_drivers <- dxs_nonzero_medians_static$driver
# dxs_static_drivers_exclude <- c("lower_slope", "upper_slope", "friction_walking_only", dxs_nonzero_medians_static$driver[dxs_nonzero_medians_static$value == 0])
dxs_static_drivers_exclude <- c("csp_ergo_landforms_21", "csp_ergo_landforms_22", "csp_ergo_landforms_31", "csp_ergo_landforms_32", "friction_walking_only", dxs_nonzero_medians_static$driver[dxs_nonzero_medians_static$value == 0])
dxs_final_static_drivers <- dxs_final_static_drivers[!(dxs_final_static_drivers %in% dxs_static_drivers_exclude)]

# All fluctuating drivers perhaps have high enough expectation of proportional
# landcovers
dxs_nonzero_medians_fluc <- dxs_nonzero_medians$nonzero_medians_fluc
dxs_final_fluc_drivers <- dxs_nonzero_medians_fluc$driver
dxs_fluc_drivers_exclude <- c("lcms_change_01", "lcms_change_04", "lcms_landcover_01", "lcms_landcover_04", dxs_nonzero_medians_fluc$driver[dxs_nonzero_medians_fluc$value == 0])
dxs_final_fluc_drivers <- dxs_final_fluc_drivers[!(dxs_final_fluc_drivers %in% dxs_fluc_drivers_exclude)]

dxs_out$out_static <- 
  dxs_out$out_static %>% 
  dplyr::filter(driver %in% dxs_final_static_drivers)

dxs_out$out_fluc <-
  dxs_out$out_fluc %>% 
  dplyr::filter(driver %in% dxs_final_fluc_drivers)

dxs_drivers <-
  fired_daily_drivers %>% 
  dplyr::filter(biome_name == "Deserts & Xeric Shrublands") %>% 
  dplyr::select(-{{dxs_static_drivers_exclude}}) %>% 
  dplyr::select(-{{dxs_fluc_drivers_exclude}})

dxs_drivers_adjusted <-
  adjust_scale_dependent_drivers(fire_independent_drivers_out = dxs_out, 
                                 fired_daily_drivers = dxs_drivers)

sf::st_write(obj = dxs_drivers_adjusted, dsn = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v1.gpkg", delete_dsn = TRUE)
data.table::fwrite(x = sf::st_drop_geometry(dxs_drivers_adjusted), file = "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v1.csv")

### ----

plot_fire_independent_scaling <- function(fire_independent_drivers_out, 
                                          subtitle,
                                          static_or_fluc = "static",
                                          lwr_med_upr = c(0.25, 0.50, 0.75),
                                          drivers_static = c("elevation", "friction", "landform_diversity", "rumple_index"),
                                          drivers_fluc = c("ndvi", "veg_structure_rumple", "landcover_diversity"),
                                          target_year = 2019) {
  
  fi_summary <- summarize_fire_independent_scaling(fire_independent_drivers_out = fire_independent_drivers_out)
  
  fi_data <- 
    fire_independent_drivers_out %>% 
    `[[`(grep(names(.), pattern = static_or_fluc))
  
  fi_summary_wide <- 
    fi_summary %>% 
    `[[`(grep(names(.), pattern = static_or_fluc)) %>% 
    dplyr::mutate(quantile = as.numeric(as.character(quantile)))
  
  if(static_or_fluc == "static") {
    fi_data <-
      fi_data %>% 
      dplyr::filter(driver %in% drivers_static)
    
    fi_summary_wide <-
      fi_summary_wide %>% 
      dplyr::filter(driver %in% drivers_static & quantile %in% lwr_med_upr) %>% 
      dplyr::mutate(quant_desc = dplyr::case_when(quantile == min(quantile) ~ "lwr",
                                                  quantile == max(quantile) ~ "upr",
                                                  TRUE ~ "med")) %>%
      dplyr::select(-quantile) %>% 
      tidyr::pivot_wider(names_from = "quant_desc", values_from = "value")
    
  } else if(static_or_fluc == "fluc") {
    fi_data <- 
      fi_data %>% 
      dplyr::filter(driver %in% drivers_fluc & ig_year == target_year)
    
    fi_summary_wide <- 
      fi_summary_wide %>% 
      dplyr::filter(driver %in% drivers_fluc & ig_year == target_year & quantile %in% lwr_med_upr) %>% 
      dplyr::mutate(quant_desc = dplyr::case_when(quantile == min(quantile) ~ "lwr",
                                                  quantile == max(quantile) ~ "upr",
                                                  TRUE ~ "med")) %>%
      dplyr::select(-quantile) %>% 
      tidyr::pivot_wider(names_from = "quant_desc", values_from = "value")
    
  }
  
  out_plot <-
    ggplot() +
    geom_point(data = fi_data, mapping = aes(x = area_log10, y = value)) +
    geom_line(data = fi_summary_wide, aes(x = area_log10_round, y = med), 
              col = "red", lwd = 1.25) +
    geom_ribbon(data = fi_summary_wide, aes(x = area_log10_round, ymin = lwr, ymax = upr), 
                alpha = 0.1, lwd = 1.25, col = "red", lty = 2) +
    facet_wrap(facets = c("driver_desc"), scales = "free_y") +
    scale_color_viridis_d() +
    theme_bw() +
    ggtitle(label = "Fire-independent scaling relationships", subtitle = subtitle)
  
  
  return(list(plot = out_plot, fi_summary_wide = fi_summary_wide))
}

# Temperate Conifer Forests

tcf_plot_static <- plot_fire_independent_scaling(fire_independent_drivers_out = tcf_out, static_or_fluc = "static", subtitle = "Temperate Conifer Forests")
tcf_plot_fluc <- plot_fire_independent_scaling(fire_independent_drivers_out = tcf_out, static_or_fluc = "fluc", subtitle = "Temperate Conifer Forests")

tcf_plot_static$plot
tcf_plot_fluc$plot

tcf_plot_static$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

tcf_plot_fluc$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

# Mediterranean Forests, Woodlands & Scrub

mfws_plot_static <- plot_fire_independent_scaling(fire_independent_drivers_out = mfws_out, static_or_fluc = "static", subtitle = "Mediterranean Forests, Woodlands & Scrub")
mfws_plot_fluc <- plot_fire_independent_scaling(fire_independent_drivers_out = mfws_out, static_or_fluc = "fluc", subtitle = "Mediterranean Forests, Woodlands & Scrub")

mfws_plot_static$plot
mfws_plot_fluc$plot

mfws_plot_static$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

mfws_plot_fluc$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

# Temperate Grasslands, Savannas & Shrublands

tgss_plot_static <- plot_fire_independent_scaling(fire_independent_drivers_out = tgss_out, static_or_fluc = "static", subtitle = "Temperate Grasslands, Savannas & Shrublands")
tgss_plot_fluc <- plot_fire_independent_scaling(fire_independent_drivers_out = tgss_out, static_or_fluc = "fluc", subtitle = "Temperate Grasslands, Savannas & Shrublands")

tgss_plot_static$plot
tgss_plot_fluc$plot

tgss_plot_static$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

tgss_plot_fluc$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

# Deserts & Xeric Shrublands

dxs_plot_static <- plot_fire_independent_scaling(fire_independent_drivers_out = dxs_out, static_or_fluc = "static", subtitle = "Deserts & Xeric Shrublands")
dxs_plot_fluc <- plot_fire_independent_scaling(fire_independent_drivers_out = dxs_out, static_or_fluc = "fluc", subtitle = "Deserts & Xeric Shrublands")

dxs_plot_static$plot
dxs_plot_fluc$plot

dxs_plot_static$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))

dxs_plot_fluc$fi_summary_wide %>% 
  mutate(range = (upr - lwr) / 2) %>% 
  group_by(driver, driver_desc) %>% 
  filter(range == max(range))


### --- Plot together

library(patchwork)

static_plots <- (tcf_plot_static$plot + mfws_plot_static$plot) / (tgss_plot_static$plot + dxs_plot_static$plot)
fluc_plots <- (tcf_plot_fluc$plot + mfws_plot_fluc$plot) / (tgss_plot_fluc$plot + dxs_plot_fluc$plot)

ggsave(filename = "figs/fire-independent-scaling-relationships_static_plot.png", plot = static_plots, width = 12, height = 12)
ggsave(filename = "figs/fire-independent-scaling-relationships_fluc_plot.png", plot = fluc_plots, width = 12, height = 12)
























# # Narrow valleys
# ggplot() +
#   geom_point(data = out_static[out_static$driver == "csp_ergo_landforms_42", ], mapping = aes(x = area_log10, y = value)) +
#   geom_point(data = area_quants_static[area_quants_static$driver == "csp_ergo_landforms_42", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_static[area_quants_static$driver == "csp_ergo_landforms_42", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_static[area_quants_static$driver == "csp_ergo_landforms_42" & area_quants_static$quantile == 0.5, ], mapping = aes(x = area_log10_round, y = value), color = "red") +
#   scale_color_viridis_d() +
#   ggtitle(unique(out_static$driver_desc[out_static$driver == "csp_ergo_landforms_42"]))
# 
# # Valleys
# ggplot() +
#   geom_point(data = out_static[out_static$driver == "csp_ergo_landforms_41", ], mapping = aes(x = area_log10, y = value)) +
#   geom_point(data = area_quants_static[area_quants_static$driver == "csp_ergo_landforms_41", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_static[area_quants_static$driver == "csp_ergo_landforms_41", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_static[area_quants_static$driver == "csp_ergo_landforms_41" & area_quants_static$quantile == 0.5, ], mapping = aes(x = area_log10_round, y = value), color = "red") +
#   scale_color_viridis_d() +
#   ggtitle(unique(out_static$driver_desc[out_static$driver == "csp_ergo_landforms_41"]))
# 
# # Vegetation structure rumple index
# ggplot() +
#   geom_point(data = out_fluc[out_fluc$driver == "veg_structure_rumple", ], mapping = aes(x = area_log10, y = value)) +
#   geom_point(data = area_quants_fluc[area_quants_fluc$driver == "veg_structure_rumple", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_fluc[area_quants_fluc$driver == "veg_structure_rumple", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_fluc[area_quants_fluc$driver == "veg_structure_rumple" & area_quants_fluc$quantile == 0.5, ], mapping = aes(x = area_log10_round, y = value), color = "red") +
#   facet_wrap(facets = "ig_year") +
#   scale_color_viridis_d() +
#   ggtitle(unique(out_fluc$driver_desc[out_fluc$driver == "veg_structure_rumple"]))
# 
# 
# # Veg structure rumple index for a single year
# ggplot() +
#   geom_point(data = out_fluc[out_fluc$ig_year == 2019 & out_fluc$driver == "veg_structure_rumple", ], mapping = aes(x = area_log10, y = value)) +
#   geom_point(data = area_quants_fluc[area_quants_fluc$ig_year == 2019 & area_quants_fluc$driver == "veg_structure_rumple", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_fluc[area_quants_fluc$ig_year == 2019 & area_quants_fluc$driver == "veg_structure_rumple", ], mapping = aes(x = area_log10_round, y = value, color = quantile)) +
#   geom_smooth(data = area_quants_fluc[area_quants_fluc$ig_year == 2019 & area_quants_fluc$driver == "veg_structure_rumple" & area_quants_fluc$quantile == 0.5, ], mapping = aes(x = area_log10_round, y = value), color = "red") +
#   scale_color_viridis_d() +
#   ggtitle(unique(out_fluc$driver_desc[out_fluc$driver == "veg_structure_rumple"]))
# 
# # All static drivers, but just the medians
# static_driver_median_plots <-
#   ggplot() +
#   geom_point(data = out_static, mapping = aes(x = area_log10, y = value)) +
#   geom_smooth(data = area_quants_static[area_quants_static$driver != "proj_area" & area_quants_static$quantile == 0.5, ], mapping = aes(x = area_log10_round, y = value), color = "red") +
#   facet_wrap(facets = c("driver_desc"), scales = "free_y") +
#   scale_color_viridis_d()
# 
# ggsave(filename = "figs/fire-independent_static-drivers_median.png", plot = static_driver_median_plots)
#
#
# All fluctuating drivers for an example year (2019), but just the medians
# fluc_driver_median_plots <-
#   ggplot() +
#   geom_point(data = out_fluc[out_fluc$ig_year == 2019, ], mapping = aes(x = area_log10, y = value)) +
#   geom_smooth(data = area_quants_fluc[area_quants_fluc$ig_year == 2019 & area_quants_fluc$quantile == 0.5, ], mapping = aes(x = area_log10_round, y = value), color = "red") +
#   facet_wrap(facets = c("driver_desc"), scales = "free_y") +
#   scale_color_viridis_d()
#
# ggsave(filename = "figs/fire-independent_fluc-drivers_medians.png", plot = fluc_driver_median_plots)



















# naming_conventions <-
#   fired_drivers_long %>%
#   dplyr::group_by(driver, driver_desc) %>%
#   dplyr::tally() %>%
#   dplyr::select(-n)
# 
# write.csv(x = naming_conventions, file = "data/out/driver-naming-conventions.csv", row.names = FALSE)

