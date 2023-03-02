library(dplyr)
library(data.table)
library(sf)
library(here)

static_version <- "v5"
fluc_version <- "v6"
roads_version <- "v1"

latest_ard_date <- sort(list.files(path = here::here("data", "ard")), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", latest_ard_date)

latest_rf_cpi_date <- sort(list.files(path = here::here("data", "out", "rf", "conditional-predictive-impact")), 
                           decreasing = TRUE)[1]

latest_rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", latest_rf_cpi_date)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

driver_description <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()
# What do we need?
# Raw proportion of each driver for each FIRED daily perimeter
## landfire, roads, fluc, static
# Assignment of EWE vs non-EWE for each FIRED daily perimeter
# Raw proportion of each driver for EWEs, raw proportion of each driver for non-EWEs

# Start with the background proportion of each driver within each RESOLVE biome

# The landfire data were in EPSG:5070, so rather than transform, I just took the mean
# of the 0's and 1's representing the different landfire disturbance histories using
# a raster layer that was already masked to remove water. This gives us the proportional
# cover directly.
lf_dist_by_resolve <- 
  data.table::fread(here::here("data", "out", "drivers", "landfire-disturbance", "landfire-disturbance-history-resolve.csv"))
lf_dist_by_resolve[, `:=`(.geo = NULL,
                          `system:index` = NULL)]

# Read in data related to area covered by non-water within polygons of interest
# non-water 30 x 30 m pixel counts by biome
static_paths <- paste0("data/out/ee/resolve-biomes-static-drivers_california_", static_version, ".csv")
roads_paths <- paste0("data/out/ee/resolve-biomes-daily-roads-drivers_california_", roads_version, ".csv")
fluc_paths <- paste0("data/out/ee/resolve-biomes-fluctuating-drivers_california_", fluc_version, ".csv")
nonwater_area_paths <- here::here("data", "out", "ee", "resolve-biome-non-water-area-30m-pixel-count.csv")

area_DT <- lapply(nonwater_area_paths, FUN = data.table::fread) |> data.table::rbindlist()
area_DT[, `:=`(.geo = NULL,
               `system:index` = NULL,
               nonwater_area_ha = sum * 30 * 30 / 1e4,
               sum = NULL)]

static_DT <- lapply(static_paths, FUN = data.table::fread) |> data.table::rbindlist()

static_DT[, `:=`(.geo = NULL, `system:index` = NULL,
                 rumple_index = surf_area / proj_area,
                 surf_area = NULL, proj_area = NULL,
                 samp_id = NULL, did = NULL, date = NULL)]

static_DT <- static_DT[id != "-999", ]
names(static_DT)[names(static_DT) == "id"] <- "biome_shortname"

roads_DT <- lapply(roads_paths, FUN = data.table::fread) |> data.table::rbindlist()
roads_DT[, `:=`(.geo = NULL,
                date = NULL,
                did = NULL,
                samp_id = NULL,
                `system:index` = NULL)]
roads_DT <- roads_DT[id != "-999", ]
names(roads_DT)[names(roads_DT) == "id"] <- "biome_shortname"

static_DT <- merge(x = static_DT, y = roads_DT, 
                   by = "biome_shortname",
                   all.x = TRUE)

# all NA values for road length means 0 road length was measured in the polygon
data.table::setnafill(x = static_DT, type = "const", fill = 0, cols = "road_length_m")

static_DT <- merge(x = static_DT, y = area_DT,
                   by = "biome_shortname",
                   all.x = TRUE)

static_DT[, `:=`(caltrans_road_density_mpha = (road_length_m) / (nonwater_area_ha),
                 road_length_m = NULL)]

fluc_DT <- lapply(fluc_paths, FUN = data.table::fread) |> data.table::rbindlist()

fluc_DT[, `:=`(.geo = NULL, `system:index` = NULL,
               samp_id = NULL, did = NULL,
               veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
               ndvi_proj_area = NULL, ndvi_surf_area = NULL,
               lcms_landcover_02_tm01 = NULL, lcms_landcover_06_tm01 = NULL, lcms_landcover_14_tm01 = NULL, lcms_landcover_15_tm01 = NULL)]

fluc_DT <- fluc_DT[id != "-999", ]
fluc_DT[, `:=`(year = lubridate::year(date),
               date = NULL)]
names(fluc_DT)[names(fluc_DT) == "id"] <- "biome_shortname"

daily_DT <- merge(static_DT, fluc_DT, by = "biome_shortname")

### Convert pixel counts to area
# using data.table::set() to modify columns in place
# https://stackoverflow.com/questions/16846380/apply-a-function-to-every-specified-column-in-a-data-table-and-update-by-referen
csp_ergo_landforms_cols <- which(grepl(x = names(daily_DT), pattern = "csp_ergo_landforms"))

# CSP Landforms based on USGS NED dataset at a 10m resolution, so each pixel covers 10*10 square meters 
for (j in csp_ergo_landforms_cols) {
  data.table::set(daily_DT, j = j, value = daily_DT[[j]] * 10 * 10 / 1e4 / daily_DT[["nonwater_area_ha"]])
}

lcms_cols <- which(grepl(x = names(daily_DT), pattern = "lcms"))

# LCMS product is based on Landsat at 30m resolution, so each pixel covers 30*30 square meters
for (j in lcms_cols) {
  data.table::set(daily_DT, j = j, value = daily_DT[[j]] * 30 * 30 / 1e4 / daily_DT[["nonwater_area_ha"]])
}

names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_01", replacement = "trees_original")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_03", replacement = "shrubs_trees_mix")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_04", replacement = "grass_forb_herb_trees_mix")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_05", replacement = "barren_trees_mix")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_07", replacement = "shrubs_original")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_08", replacement = "grass_forb_herb_shrub_mix")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_09", replacement = "barren_shrub_mix")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_10", replacement = "grass_forb_herb_original")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_11", replacement = "barren_grass_forb_herb_mix")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_12", replacement = "barren_original")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "lcms_landcover_13", replacement = "snow_or_ice")

names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_11", replacement = "peak_ridge_warm")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_12", replacement = "peak_ridge")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_13", replacement = "peak_ridge_cool")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_14", replacement = "mountain_divide")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_15", replacement = "cliff")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_21", replacement = "upper_slope_warm")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_22", replacement = "upper_slope")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_23", replacement = "upper_slope_cool")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_24", replacement = "upper_slope_flat")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_31", replacement = "lower_slope_warm")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_32", replacement = "lower_slope")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_33", replacement = "lower_slope_cool")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_34", replacement = "lower_slope_flat")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_41", replacement = "valley_original")
names(daily_DT) <- gsub(x = names(daily_DT), pattern = "csp_ergo_landforms_42", replacement = "valley_narrow")

daily_DT[, `:=`(peak_ridge_cliff = peak_ridge_warm + peak_ridge + peak_ridge_cool + mountain_divide + cliff,
                valleys = valley_original + valley_narrow,
                slope_warm = upper_slope_warm + lower_slope_warm,
                slope_cool = upper_slope_cool + lower_slope_cool,
                slope_neutral = upper_slope + lower_slope,
                flat = upper_slope_flat + lower_slope_flat,
                trees_tm01 =
                  trees_original_tm01,
                shrubs_tm01 = 
                  shrubs_trees_mix_tm01 + 
                  shrubs_original_tm01,
                grass_forb_herb_tm01 = 
                  grass_forb_herb_original_tm01 + 
                  grass_forb_herb_trees_mix_tm01 + 
                  grass_forb_herb_shrub_mix_tm01,
                barren_tm01 = 
                  barren_trees_mix_tm01 + 
                  barren_shrub_mix_tm01 + 
                  barren_grass_forb_herb_mix_tm01 + 
                  barren_original_tm01 + 
                  snow_or_ice_tm01)]

# Using lumped proportions
daily_DT[, `:=`(landform_diversity = vegan::diversity(cbind(peak_ridge_cliff,
                                                            valleys,
                                                            slope_warm,
                                                            slope_cool,
                                                            slope_neutral,
                                                            flat)),
                landcover_diversity_tm01 = vegan::diversity(cbind(trees_tm01,
                                                                  shrubs_tm01,
                                                                  grass_forb_herb_tm01,
                                                                  barren_tm01)))]

data.table::setcolorder(x = daily_DT, neworder = c("biome_shortname", "biome_fullname", "year"))

resolve_drivers <- merge(x = daily_DT, y = lf_dist_by_resolve, by = c("biome_shortname", "biome_fullname", "year"))
resolve_drivers <- data.table::melt(data = resolve_drivers, id.vars = c("biome_shortname", "biome_fullname", "year"), value.name = "expected_value")

resolve_drivers_summary <- 
  resolve_drivers %>% 
  dplyr::group_by(biome_shortname, biome_fullname, variable) %>% 
  dplyr::summarize(expected_value = mean(expected_value))
### End getting roads/fluc/static drivers into proportion verison

static_fluc_fired_drivers <-data.table::fread("data/out/drivers/fired-fluc-static-driver-proportions.csv")

# modify any columns as necessary
static_fluc_fired_drivers[, `:=`(date = as.Date(date))]

# drop unnecessary columns
keep_cols <- c("did", "id", "date",
               "elevation", "rumple_index",
               "caltrans_road_density_mpha",
               "ndvi", "veg_structure_rumple", 
               "peak_ridge_cliff", "valleys", "slope_warm", "slope_cool", "slope_neutral", "flat", 
               "trees_tm01", "shrubs_tm01", "grass_forb_herb_tm01", "barren_tm01",
               "landform_diversity", "landcover_diversity_tm01")

static_fluc_fired_drivers <- static_fluc_fired_drivers[, .SD, .SDcols = keep_cols]
static_fluc_fired_drivers[, year := lubridate::year(date)]

lf_fired_drivers <- data.table::fread("data/out/drivers/landfire-disturbance/fired_daily_disturbance-drivers_v1.csv")

# Some NAs seem to still be a part of these data; not sure why since the following line was run after extracting the 
# raw proportions. Run it again here
data.table::setnafill(x = lf_fired_drivers, type = "const", fill = 0, cols = names(lf_fired_drivers)[!(names(lf_fired_drivers) %in% c("did", "id", "date", "samp_id"))])

lf_fired_drivers[, `:=`(date = as.Date(date),
                        samp_id = NULL,
                        fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
                        fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10)]

# drop any unnecessary columns
keep_cols <- c("did", "id", "date",
               "fire_high_tm01_tm05", "fire_high_tm06_tm10",
               "fire_not_high_tm01_tm05", "fire_not_high_tm06_tm10",
               "insect_disease_tm01_tm10")

lf_fired_drivers <- lf_fired_drivers[, .SD, .SDcols = keep_cols]
lf_fired_drivers <- lf_fired_drivers[did %in% static_fluc_fired_drivers$did, ]

lf_fired_drivers[, year := lubridate::year(date)]

fired_drivers <- merge(x = static_fluc_fired_drivers, y = lf_fired_drivers,
                       by = c("did", "id", "date", "year"))


# Get the analysis ready data to tell us which did's are relevant
ard <- 
  lapply(X = list.files(here::here(latest_ard_dir), 
                        full.names = TRUE),
         FUN = data.table::fread) |> 
  data.table::rbindlist(fill = TRUE)

ard_long <- data.table::melt(ard, id.vars = c("did", "event_day", "daily_area_ha", "cumu_area_tm01", "ewe", "biome_name_daily", "biome_shortname", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", "spatial_fold"))
ard_summary <-
  ard_long %>% 
  dplyr::group_by(biome_shortname, variable, ewe) %>% 
  dplyr::summarize(value = mean(value))


fired_drivers_summary <-
  fired_drivers %>% 
  dplyr::group_by(biome_shortname, variable, ewe) %>% 
  dplyr::summarize(value = mean(value))


fired_drivers_summary %>% filter(biome_shortname == "tcf", variable == "flat")
resolve_drivers_summary %>% filter(biome_shortname == "tcf", variable == "flat")
ard_summary %>% filter(biome_shortname == "tcf", variable == "flat")


fired_drivers <- 
  fired_drivers[did %in% ard$did, ] %>% 
  merge(y = ard[, .SD, .SDcols = c("did", "ewe", "biome_shortname")], by = "did") %>% 
  data.table::melt(id.vars = c("did", "id", "date", "year", "biome_shortname", "ewe"))

# fired_drivers %>% 
#   tidyr::pivot_longer(cols = !tidyselect::all_of(c("did", "id", "date", "year", "biome_shortname", "ewe"))) %>% 
#   dplyr::group_by(biome_shortname, ewe, year, name) %>%
#   dplyr::summarize(val = mean(value)) %>%
#   dplyr::arrange(desc(biome_shortname), name, year, ewe) %>% 
#   dplyr::filter(biome_shortname == "tcf")

fired_v_expected <- merge(x = fired_drivers, 
                          y = resolve_drivers,
                          by = c("year", "biome_shortname", "variable"))

fired_v_expected[, diff := value - expected_value]

fired_v_expected <-
  fired_v_expected %>% 
  dplyr::group_by(biome_shortname, ewe, variable) %>%
  dplyr::summarize(mean_difference = mean(diff)) %>%
  dplyr::arrange(desc(biome_shortname), variable, ewe) %>% 
  dplyr::filter(biome_shortname %in% c("tcf", "mfws"))

# Look at the key variables
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

cpi_out <-
  lapply(c("tcf", "mfws", "dxs"), FUN = function(biome_shortname) {
    out <-
      data.table::fread(input = here::here(latest_rf_cpi_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))) %>% 
      dplyr::rename(variable = Variable, biome_shortname = biome)
  }) %>% 
  data.table::rbindlist()

cpi_summary_across_iter <- 
  cpi_out %>% 
  dplyr::group_by(variable, spatial_fold = id, biome_shortname) %>% 
  dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
                   sd = sd(CPI, na.rm = TRUE),
                   count = sum(!is.na(CPI)),
                   min = min(CPI, na.rm = TRUE),
                   max = max(CPI, na.rm = TRUE),
                   lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
                   upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
  dplyr::arrange(biome_shortname, variable, spatial_fold) %>% 
  dplyr::mutate(biome_fullname = biome_lookup[match(biome_shortname, names(biome_lookup))]) %>% 
  dplyr::left_join(driver_description)

cpi_summary_across_folds <-
  cpi_summary_across_iter %>% 
  dplyr::group_by(variable, biome_shortname) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::group_by(biome_shortname) %>% 
  dplyr::arrange(desc(cpi_median)) %>% 
  dplyr::filter(biome_shortname %in% c("tcf", "mfws")) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(key_var = ifelse(cpi_median > 0, 1, 0)) %>% 
  as.data.table()

cpi_summary_across_folds %>% 
  dplyr::filter(key_var == 1, biome_shortname == "tcf") %>% 
  arrange(biome_shortname, desc(cpi_median)) %>% 
  dplyr::select(variable, biome_shortname, cpi_median)

cpi_summary_across_folds %>% 
  dplyr::filter(key_var == 1, biome_shortname == "mfws") %>% 
  arrange(biome_shortname, desc(cpi_median)) %>% 
  dplyr::select(variable, biome_shortname, cpi_median)


out <- 
  merge(x = fired_v_expected, 
        y = cpi_summary_across_folds[, .SD, .SDcols = c("variable", "biome_shortname", "key_var")],
        by = c("variable", 'biome_shortname')) %>% 
  dplyr::filter(key_var == 1) %>% 
  tibble::as_tibble() %>% 
  dplyr::arrange(biome_shortname, variable)

cpi_tcf <-
  cpi_summary_across_folds %>% 
  dplyr::filter(biome_shortname == "tcf")

important_tcf <- cpi_tcf$variable[cpi_tcf$key_var == 1]
out_tcf <-
  out %>% 
  dplyr::filter(biome_shortname == "tcf") %>% 
  dplyr::mutate(variable = factor(variable, levels = cpi_tcf$variable[order(cpi_tcf$cpi_median, decreasing = TRUE)])) %>% 
  dplyr::arrange(variable, ewe)

cpi_mfws <-
  cpi_summary_across_folds %>% 
  dplyr::filter(biome_shortname == "mfws")

out_mfws <-
  out %>% 
  dplyr::filter(biome_shortname == "mfws") %>% 
  dplyr::mutate(variable = factor(variable, levels = cpi_mfws$variable[order(cpi_mfws$cpi_median, decreasing = TRUE)])) %>% 
  dplyr::arrange(variable, ewe)

out_tcf %>% print(n = 24)
out_mfws





