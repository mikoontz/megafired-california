# Convert the flucuating and static driver outputs from Earth Engine into raw proportions
# and combine landcover types that we think should be lumped

library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(pbapply)
library(USAboundaries)

static_version <- "v5"
fluc_version <- "v6"
roads_version <- "v1"

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Read in data related to area covered by non-water within polygons of interest
# non-water 30 x 30 m pixel counts by biome

# We counted the number of 30x30 m pixels in EPSG:3310 for FIRED events and fire-independent FIRED events. 
# So we'll need to use the non-water area to get to proportion
prep_static_and_fluc_drivers <- function(static_paths, roads_paths, fluc_paths, nonwater_area_paths) {
  
  area_DT <- lapply(nonwater_area_paths, FUN = data.table::fread) |> data.table::rbindlist()
  area_DT[, `:=`(.geo = NULL,
                 `system:index` = NULL,
                 nonwater_area_ha = sum * 30 * 30 / 1e4,
                 sum = NULL)]
  area_DT[, date := as.character(date)]
  
  static_DT <- lapply(static_paths, FUN = data.table::fread) |> data.table::rbindlist()
  
  static_DT[, `:=`(.geo = NULL, `system:index` = NULL,
                   rumple_index = surf_area / proj_area,
                   surf_area = NULL, proj_area = NULL)]
  
  static_DT <- static_DT[did != "-999", ]
  
  roads_DT <- lapply(roads_paths, FUN = data.table::fread) |> data.table::rbindlist()
  roads_DT[, date := as.character(date)]
  
  static_DT <- merge(x = static_DT, y = roads_DT, 
                     by = c("did", "id", "date", "samp_id"),
                     all.x = TRUE)
  
  # all NA values for road length means 0 road length was measured in the polygon
  data.table::setnafill(x = static_DT, type = "const", fill = 0, cols = "road_length_m")
  
  static_DT <- merge(x = static_DT, y = area_DT,
                     by = c("did", "id", "date", "samp_id"),
                     all.x = TRUE)
  
  static_DT[, `:=`(caltrans_road_density_mpha = (road_length_m) / (nonwater_area_ha),
                   road_length_m = NULL)]
  
  fluc_DT <- lapply(fluc_paths, FUN = data.table::fread) |> data.table::rbindlist()
  
  fluc_DT[, `:=`(.geo = NULL, `system:index` = NULL,
                 veg_structure_rumple = ndvi_surf_area / ndvi_proj_area,
                 ndvi_proj_area = NULL, ndvi_surf_area = NULL,
                 lcms_landcover_02_tm01 = NULL, lcms_landcover_06_tm01 = NULL, lcms_landcover_14_tm01 = NULL, lcms_landcover_15_tm01 = NULL)]
  
  fluc_DT <- fluc_DT[did != "-999", ]
  
  daily_DT <- merge(static_DT, fluc_DT, by = c("did", "date", "id", "samp_id"))
  
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
  # Trees_tm00: lcms_landcover_01_tm00
  # Shrubs_tm00: lcms_landcover_07_tm00
  # Grass/forb/herbs_tm00: lcms_landcover_10_tm00
  # Mixed_tm00: lcms_landcover_03_tm00 + lcms_landcover_04_tm00 + lcms_landcover_08_tm00
  # Barren_tm00: lcms_landcover_05_tm00 + lcms_landcover_09_tm00 + lcms_landcover_11_tm00 + lcms_landcover_12_tm00 + lcms_landcover_13_tm00
  
  # Lumps things into the dominant veg cover type (plus barren)
  # shrubs_tm00 = 
  #   shrubs_trees_mix_tm00 + 
  #   shrubs_original_tm00,
  # grass_forb_herb_tm00 = 
  #   grass_forb_herb_original_tm00 + 
  #   grass_forb_herb_trees_mix_tm00 + 
  #   grass_forb_herb_shrub_mix_tm00,
  # barren_tm00 = 
  #   barren_trees_mix_tm00 + 
  #   barren_shrub_mix_tm00 + 
  #   barren_grass_forb_herb_mix_tm00 + 
  #   barren_original_tm00 + 
  #   snow_or_ice_tm00 
  
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
  
  # Potential groupings 
  # peak_ridge_cliff = peak_ridge_warm + peak_ridge + peak_ridge_cool + mountain_divide + cliff,
  # valleys = valley + valley_narrow,
  # slope_warm = upper_slope_warm + lower_slope_warm,
  # slope_cool = upper_slope_cool + lower_slope_cool,
  # slope_neutral = upper_slope + lower_slope,
  # flat = upper_slope_flat + lower_slope_flat)
  
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
  
  data.table::setcolorder(x = daily_DT, neworder = c("did", "id", "date", "samp_id"))
  
  return(daily_DT)
}

# We want to find out which FIRED and fire-independent FIRED perimeters have mismatches
# between how much non-water area is in each 
# That is, for a given `did`, there will be the FIRED location (samp_id == 0) and ~500 
# fire-independent locations of that perimeter shape elsewhere within the biome (samp_id %in% 1:500)
# For each, we have a measurment of how much nonwater area is within.
# Comparing the FIRED nonwater area to the fire-independent nonwater area for the same `did` we 
# can determine how many mismatches there are (e.g., the fire-independent perimeter was placed
# over Lake Tahoe)
# We'll call it a mismatch if the fire-independent nonwater area divided by the FIRED nonwater area
# is less than 0.9 or greater than 1.1

### FIRED non-water
fired_nonwater_area_DT <- 
  data.table::fread(here::here("data", "out", "ee", "fired-non-water-area-30m-pixel-count.csv"))

fired_nonwater_area_DT[, `:=`(.geo = NULL,
                              `system:index` = NULL,
                              fired_nonwater_area_ha = sum * 30 * 30 / 1e4,
                              sum = NULL,
                              samp_id = NULL)]

### Fire-independent FIRED non-water
fi_nonwater_area_DT <-
  lapply(list.files(path = here::here("data", "out", "ee"), 
                    pattern = "fi_fired_.*\\_non-water-area-30m-pixel-count.csv",
                    full.names = TRUE),
         FUN = data.table::fread) |>
  data.table::rbindlist()

fi_nonwater_area_DT[, `:=`(.geo = NULL,
                           `system:index` = NULL,
                           nonwater_area_ha = sum * 30 * 30 / 1e4,
                           sum = NULL)]

nonwater_area_DT <- merge(x = fi_nonwater_area_DT, 
                          y = fired_nonwater_area_DT,
                          by = c("date", "did", "id"))

nonwater_area_DT[, fi_to_fired_nonwater_area_frac := nonwater_area_ha / fired_nonwater_area_ha]

nonwater_area_DT <- 
  nonwater_area_DT[(fi_to_fired_nonwater_area_frac >= 0.9 & fi_to_fired_nonwater_area_frac <= 1.1), ]

valid_nonwater_area_DT <- 
  nonwater_area_DT[, .N, by = .(date, did, id)]

valid_did <- valid_nonwater_area_DT[N >= 400]

# Convert FIRED pixel counts to area
fired_daily_drivers <- prep_static_and_fluc_drivers(
  static_paths = paste0("data/out/ee/FIRED-daily-static-drivers_california_", static_version, ".csv"),
  roads_paths = paste0("data/out/drivers/roads/fired_daily_road-drivers_", roads_version, ".csv"),
  fluc_paths = paste0("data/out/ee/FIRED-daily-fluctuating-drivers_california_", fluc_version, ".csv"),
  nonwater_area_paths = here::here("data", "out", "ee", "fired-non-water-area-30m-pixel-count.csv")
)

fired_daily_drivers <- fired_daily_drivers[fired_daily_drivers$nonwater_area_ha != 0, ]
fired_daily_drivers <- fired_daily_drivers[did %in% valid_did$did, ]

data.table::fwrite(x = fired_daily_drivers, file = "data/out/drivers/fired-fluc-static-driver-proportions.csv")

fi_daily_drivers <- prep_static_and_fluc_drivers(
  static_paths = list.files(
    path = "data/out/ee/fire-independent-drivers", 
    pattern = "static", 
    full.names = TRUE
  ),
  roads_paths = list.files(
    path = "data/out/drivers/roads/fire-independent-locations/", 
    pattern = ".csv", 
    full.names = TRUE
  ),
  fluc_paths = list.files(
    path = "data/out/ee/fire-independent-drivers", 
    pattern = "fluc", 
    full.names = TRUE
  ),
  nonwater_area_paths = list.files(
    path = here::here("data", "out", "ee"), 
    pattern = "fi_fired_.*\\_non-water-area-30m-pixel-count.csv",
    full.names = TRUE
  )
)

fi_daily_drivers <- fi_daily_drivers[fi_daily_drivers$nonwater_area_ha != 0, ]
fi_daily_drivers <- fi_daily_drivers[did %in% valid_did$did, ]

data.table::fwrite(x = fi_daily_drivers, file = "data/out/drivers/fi-fluc-static-driver-proportions.csv")

### Fireshed analysis
fireshed_drivers <- prep_static_and_fluc_drivers(
  static_paths = paste0("data/out/ee/fireshed-static-drivers_california_", static_version, ".csv"),
  roads_paths = paste0("data/out/drivers/roads/fireshed_road-drivers_", roads_version, ".csv"),
  fluc_paths = paste0("data/out/ee/fireshed-fluctuating-drivers-2020_california_", fluc_version, ".csv"),
  nonwater_area_paths = here::here("data", "out", "ee", "fireshed-non-water-area-30m-pixel-count.csv")
)

fireshed_drivers <- fireshed_drivers[fireshed_drivers$nonwater_area_ha != 0, ]

data.table::fwrite(x = fireshed_drivers, file = "data/out/drivers/fireshed-fluc-static-driver-proportions.csv")


### Sierra Nevada PODS analysis
pods_drivers <- prep_static_and_fluc_drivers(
  static_paths = paste0("data/out/ee/pods-static-drivers_california_", static_version, ".csv"),
  roads_paths = paste0("data/out/drivers/roads/pods_road-drivers_", roads_version, ".csv"),
  fluc_paths = paste0("data/out/ee/pods-fluctuating-drivers-2020_california_", fluc_version, ".csv"),
  nonwater_area_paths = here::here("data", "out", "ee", "pods-non-water-area-30m-pixel-count.csv")
)

pods_drivers <- pods_drivers[pods_drivers$nonwater_area_ha != 0, ]

data.table::fwrite(x = pods_drivers, file = "data/out/drivers/pods-fluc-static-driver-proportions.csv")


# ### Convert the static, fluc, and roads pixel counts to proportions of the resolve ecoregion
# # Convert FIRED pixel counts to area
# resolve_daily_drivers <- prep_static_and_fluc_drivers(
#   static_paths = paste0("data/out/ee/resolve-biomes-static-drivers_california_", static_version, ".csv"),
#   roads_paths = paste0("data/out/ee/resolve-biomes-daily-roads-drivers_california_", roads_version, ".csv"),
#   fluc_paths = paste0("data/out/ee/resolve-biomes-fluctuating-drivers_california_", fluc_version, ".csv"),
#   nonwater_area_paths = here::here("data", "out", "ee", "resolve-biome-non-water-area-30m-pixel-count.csv")
# )
# 
# resolve_daily_drivers <- resolve_daily_drivers[resolve_daily_drivers$nonwater_area_ha != 0, ]
# resolve_daily_drivers <- resolve_daily_drivers[did %in% valid_did$did, ]
# 
# data.table::fwrite(x = resolve_daily_drivers, file = "data/out/drivers/resolve-fluc-static-driver-proportions.csv")
