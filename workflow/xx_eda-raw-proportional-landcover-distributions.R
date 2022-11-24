# Exlore raw proportional landcover of static and fluctuating variables (i.e., not correcting for fire-independent scaling relationships)

library(data.table)
library(dplyr)
library(ggplot2)
library(ggdist)
library(sf)
library(patchwork)

# Get the function to take the generic analysis ready data and prepare it for {ranger}
source("workflow/12_generic-analysis-ready-data-to-ranger-ready-data_function.R")

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
# Full names of the biomes for the plot titles
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

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
ard <- 
  lapply(ard_fires, FUN = function(x) return(x$data)) %>% 
  data.table::rbindlist(fill = TRUE) %>% 
  dplyr::rename(biome_fullname = biome_name_daily)

static_version <- "v4"
fluc_version <- "v5"
driver_version <- "v9"

### FIRED DRIVERS

# lcms_landcover_desc <-
#   tribble(~value, ~color, ~description,
#           "01",	"#005e00", "Trees",
#           "02",	"#008000", "Tall Shrubs & Trees Mix (SEAK Only)",
#           "03",	"#00cc00", "Shrubs & Trees Mix",
#           "04",	"#b3ff1a", "Grass/Forb/Herb & Trees Mix",
#           "05",	"#99ff99", "Barren & Trees Mix",
#           "06",	"#b30088", "Tall Shrubs (SEAK Only)",
#           "07",	"#e68a00", "Shrubs",
#           "08",	"#ffad33", "Grass/Forb/Herb & Shrubs Mix",
#           "09",	"#ffe0b3", "Barren & Shrubs Mix",
#           "10",	"#ffff00", "Grass/Forb/Herb",
#           "11",	"#AA7700", "Barren & Grass/Forb/Herb Mix",
#           "12",	"#d3bf9b", "Barren or Impervious",
#           "13",	"#ffffff", "Snow or Ice",
#           "14",	"#4780f3", "Water",
#           "15",	"#1B1716", "Non-Processing Area Mask")

# Potential groupings:
# Trees: lcms_landcover_01
# Shrubs: lcms_landcover_07
# Grass/forb/herbs: lcms_landcover_10
# Mixed: lcms_landcover_03 + lcms_landcover_04 + lcms_landcover_08
# Barren: lcms_landcover_05 + lcms_landcover_09 + lcms_landcover_11 + lcms_landcover_12

# csp_landform_desc <-
#   tribble(~value, ~color, ~description,
#           "11",	"#141414",	"Peak/ridge (warm)",
#           "12",	"#383838",	"Peak/ridge",
#           "13",	"#808080",	"Peak/ridge (cool)",
#           "14",	"#EBEB8F",	"Mountain/divide",
#           "15",	"#F7D311",	"Cliff",
#           "21",	"#AA0000",	"Upper slope (warm)",
#           "22",	"#D89382",	"Upper slope",
#           "23",	"#DDC9C9",	"Upper slope (cool)",
#           "24",	"#DCCDCE",	"Upper slope (flat)",
#           "31",	"#1C6330",	"Lower slope (warm)",
#           "32",	"#68AA63",	"Lower slope",
#           "33",	"#B5C98E",	"Lower slope (cool)",
#           "34",	"#E1F0E5",	"Lower slope (flat)",
#           "41",	"#a975ba",	"Valley",
#           "42",	"#6f198c",	"Valley (narrow))")

# Potential groupings:
# 
# 
# 
# 

### landform and landcover variables
static_drivers <- data.table::fread(paste0("data/out/ee/FIRED-daily-static-drivers_california_", static_version, ".csv"))
static_drivers[, `:=`(.geo = NULL, samp_id = NULL, `system:index` = NULL,
                      rumple_index = surf_area / proj_area,
                      road_density_mpha = (road_length_m) / (proj_area / 10000),
                      surf_area = NULL, road_length_m = NULL)]
static_drivers <- static_drivers[did %in% ard$did, ]

fluc_drivers <- data.table::fread(paste0("data/out/ee/FIRED-daily-fluctuating-drivers_california_", fluc_version, ".csv"))
# LCMS Landcovers 2 and 6 are only in Alaska, so we'll remove them here
fluc_drivers[, `:=`(.geo = NULL, samp_id = NULL, `system:index` = NULL,
                    veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                    ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                    lcms_landcover_02_tm00 = NULL, lcms_landcover_06_tm00 = NULL,
                    lcms_landcover_02_tm01 = NULL, lcms_landcover_06_tm01 = NULL)]
fluc_drivers <- fluc_drivers[did %in% ard$did, ]

daily_drivers <- 
  merge(static_drivers, fluc_drivers, by = c("did", "date", "id")) %>% 
  merge(ard[, c("did", "ewe", "biome_fullname"), ], by = "did") %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10*10)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / proj_area)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / proj_area)) %>% 
  dplyr::select(-c(lcms_landcover_14_tm00, lcms_landcover_15_tm00, lcms_change_05_tm00),
                -c(lcms_landcover_14_tm01, lcms_landcover_15_tm01, lcms_change_05_tm01)) %>% 
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
  # dplyr::mutate(mixed_landcover_tm00 = lcms_landcover_03_tm00 + lcms_landcover_04_tm00 + lcms_landcover_08_tm00,
  #               barren_landcover_tm00 = lcms_landcover_05_tm00 + lcms_landcover_09_tm00 + lcms_landcover_11_tm00 + lcms_landcover_12_tm00) %>% 
  # dplyr::mutate(mixed_landcover_tm01 = lcms_landcover_03_tm01 + lcms_landcover_04_tm01 + lcms_landcover_08_tm01,
  #               barren_landcover_tm01 = lcms_landcover_05_tm01 + lcms_landcover_09_tm01 + lcms_landcover_11_tm01 + lcms_landcover_12_tm01) %>% 
  dplyr::mutate(shrubs_tm00 = lcms_landcover_03_tm00 + lcms_landcover_07_tm00,
                grass_forb_herb_tm00 = lcms_landcover_10_tm00 + lcms_landcover_04_tm00 + lcms_landcover_08_tm00,
                barren_landcover_tm00 = lcms_landcover_05_tm00 + lcms_landcover_09_tm00 + lcms_landcover_11_tm00 + lcms_landcover_12_tm00 + lcms_landcover_13_tm00) %>% 
  dplyr::mutate(shrubs_tm01 = lcms_landcover_03_tm01 + lcms_landcover_07_tm01,
                grass_forb_herb_tm01 = lcms_landcover_10_tm01 + lcms_landcover_04_tm01 + lcms_landcover_08_tm01,
                barren_landcover_tm01 = lcms_landcover_05_tm01 + lcms_landcover_09_tm01 + lcms_landcover_11_tm01 + lcms_landcover_12_tm01 + lcms_landcover_13_tm01) %>% 
  dplyr::select(did, id, date, everything()) %>% 
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

# Shrubs: lcms_landcover_07 + lcms_landcover_03
# Grass/forb/herbs: lcms_landcover_10 + lcms_landcover_04 + lcms_landcover_08
# Barren: lcms_landcover_05 + lcms_landcover_09 + lcms_landcover_11 + lcms_landcover_12 + lcms_landcover_13

names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_01", replacement = "trees")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_03", replacement = "shrubs_trees_mix")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_04", replacement = "grass_forbs_herb_trees_mix")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_05", replacement = "barren_trees_mix")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_07", replacement = "shrubs")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_08", replacement = "grass_forb_herb_shrub_mix")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_09", replacement = "barren_shrub_mix")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_10", replacement = "grass_forb_herb")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_11", replacement = "barren_grass_forb_herb_mix")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_landcover_12", replacement = "barren")

names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_change_01", replacement = "fuel_stable")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_change_02", replacement = "fuel_slow_loss")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_change_03", replacement = "fuel_fast_loss")
names(daily_drivers) <- gsub(x = names(daily_drivers), pattern = "lcms_change_04", replacement = "fuel_gain")

ggplot(daily_drivers, aes(x = barren_grass_forb_herb_mix_tm01)) +
  geom_density()

# FIRE INDEPENDENT DRIVERS
# biome_shortname <- "tcf"

fi_static_drivers <- 
  list.files(path = "data/out/ee/fire-independent-drivers/randomly-located-fired-polys/", pattern = "static", full.names = TRUE)

# fi_static_drivers <- fi_static_drivers[grepl(pattern = biome_shortname, x = fi_static_drivers)]

fi_static_drivers <- 
  fi_static_drivers %>% 
  lapply(FUN = fread) %>% 
  data.table::rbindlist()

fi_static_drivers <- fi_static_drivers[fi_static_drivers$`system:index` != "1_0", ]

fi_static_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                         rumple_index = surf_area / proj_area,
                         road_density_mpha = (road_length_m) / (proj_area / 10000),
                         surf_area = NULL, road_length_m = NULL)]
fi_fluc_drivers <- 
  list.files(path = "data/out/ee/fire-independent-drivers/randomly-located-fired-polys/", pattern = "fluc", full.names = TRUE)

# fi_fluc_drivers <- fi_fluc_drivers[grepl(pattern = biome_shortname, x = fi_fluc_drivers)]

fi_fluc_drivers <-
  fi_fluc_drivers %>% 
  lapply(FUN = fread) %>% 
  data.table::rbindlist()

fi_fluc_drivers <- fi_fluc_drivers[fi_fluc_drivers$`system:index` != "1_0", ]

# LCMS Landcovers 2 and 6 are only in Alaska, so we'll remove them here
fi_fluc_drivers[, `:=`(.geo = NULL, `system:index` = NULL,
                       veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                       ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                       lcms_landcover_02_tm00 = NULL, lcms_landcover_06_tm00 = NULL,
                       lcms_landcover_02_tm01 = NULL, lcms_landcover_06_tm01 = NULL)]


fi_drivers <- 
  merge(fi_static_drivers, fi_fluc_drivers, by = c("did", "date", "id", "samp_id")) %>% 
  merge(ard[, c("did", "ewe", "biome_fullname"), ], by = "did") %>% 
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
  dplyr::mutate(mixed_landcover_tm00 = lcms_landcover_03_tm00 + lcms_landcover_04_tm00 + lcms_landcover_08_tm00,
                barren_landcover_tm00 = lcms_landcover_05_tm00 + lcms_landcover_09_tm00 + lcms_landcover_11_tm00 + lcms_landcover_12_tm00) %>% 
  dplyr::mutate(mixed_landcover_tm01 = lcms_landcover_03_tm01 + lcms_landcover_04_tm01 + lcms_landcover_08_tm01,
                barren_landcover_tm01 = lcms_landcover_05_tm01 + lcms_landcover_09_tm01 + lcms_landcover_11_tm01 + lcms_landcover_12_tm01) %>% 
  dplyr::select(did, id, date, everything()) %>% 
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


names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_01", replacement = "trees")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_03", replacement = "shrubs_trees_mix")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_04", replacement = "grass_forbs_herb_trees_mix")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_05", replacement = "barren_trees_mix")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_07", replacement = "shrubs")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_08", replacement = "grass_forb_herb_shrub_mix")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_09", replacement = "barren_shrub_mix")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_10", replacement = "grass_forb_herb")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_11", replacement = "barren_grass_forb_herb_mix")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_landcover_12", replacement = "barren")

names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_change_01", replacement = "fuel_stable")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_change_02", replacement = "fuel_slow_loss")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_change_03", replacement = "fuel_fast_loss")
names(fi_drivers) <- gsub(x = names(fi_drivers), pattern = "lcms_change_04", replacement = "fuel_gain")

data.table::fwrite(x = fi_drivers, file = "incubator/fi-drivers-for-exploring-category-aggregation.csv")
data.table::fwrite(x = daily_drivers, file = "incubator/fired-drivers-for-exploring-category-aggregation.csv")

# aggregate(daily_drivers[, "barren_grass_forb_herb_mix_tm01"], list(biome = daily_drivers$biome_fullname), summary)
# aggregate(fi_drivers[, "barren_grass_forb_herb_mix_tm01"], list(biome = fi_drivers$biome_fullname), summary)

daily_drivers %>% 
  dplyr::select(biome_fullname, trees_tm01, shrubs_tm01, grass_forb_herb_tm01, barren_landcover_tm01) %>% 
  dplyr::group_by(biome_fullname) %>% 
  dplyr::summarise(dplyr::across(.fns = median))

summary(daily_drivers$barren_grass_forb_herb_mix_tm01)
summary(fi_drivers$barren_grass_forb_herb_mix_tm01)

str(state.x77)
aggregate(state.x77, list(Region = state.region), mean)

plot_data <-
  rbind(dplyr::mutate(dplyr::select(daily_drivers, "barren_grass_forb_herb_mix_tm01"), source = "fired"),
        dplyr::mutate(dplyr::select(fi_drivers, "barren_grass_forb_herb_mix_tm01"), source = "fi"))

ggplot(plot_data, aes(x = barren_grass_forb_herb_mix_tm01, color = source)) +
  geom_density() +
  theme_bw() +
  coord_cartesian(xlim = c(0, 0.005))
