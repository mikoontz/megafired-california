library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyr)
library(terra)
library(spatialRF)
library(vegan)
library(randomForestExplainer)
library(pdp)

# read in fire data
fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

# # CSP ERGO Landforms based on 10m NED DEM
# csp_ergo_landforms_desc <-
#   tribble(~value, ~color,	~description, 
#           "11", "#141414", "Peak/ridge (warm)",
#           "12", "#383838", "Peak/ridge",
#           "13", "#808080"," Peak/ridge (cool)",
#           "14", "#EBEB8F", "Mountain/divide",
#           "15", "#F7D311", "Cliff",
#           "21", "#AA0000", "Upper slope (warm)",
#           "22", "#D89382", "Upper slope",
#           "23", "#DDC9C9", "Upper slope (cool)",
#           "24", "#DCCDCE", "Upper slope (flat)",
#           "31", "#1C6330", "Lower slope (warm)",
#           "32", "#68AA63", "Lower slope",
#           "33", "#B5C98E", "Lower slope (cool)",
#           "34", "#E1F0E5", "Lower slope (flat)",
#           "41", "#a975ba", "Valley",
#           "42", "#6f198c", "Valley (narrow)") 
# 
# # Landscape Change Monitoring System: Change
# lcms_change_desc <-
#   tribble(value, 	~color, ~description,
#           "01", "#3d4551", "Stable",
#           "02", "#f39268", "Slow Loss",
#           "03", "#d54309", "Fast Loss",
#           "04", "#00a398", "Gain",
#           "05", "#1B1716", "Non-Processing Area Mask") 
# 
# # Landscape Change Monitoring System: Landcover
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
# 
#
# Keeping all categories of landcover, change, and landforms in the dataset:
# fires_working <- 
#   fires %>% 
#   dplyr::rename(peak_ridge_warm = csp_ergo_landforms_11,
#                 peak_ridge = csp_ergo_landforms_12,
#                 peak_ridge_cool = csp_ergo_landforms_13,
#                 mountain_divide = csp_ergo_landforms_14,
#                 cliff = csp_ergo_landforms_15,
#                 upper_slope_warm = csp_ergo_landforms_21,
#                 upper_slope = csp_ergo_landforms_22,
#                 upper_slope_cool = csp_ergo_landforms_23,
#                 upper_slope_flat = csp_ergo_landforms_24,
#                 lower_slope_warm = csp_ergo_landforms_31,
#                 lower_slope = csp_ergo_landforms_32,
#                 lower_slope_cool = csp_ergo_landforms_33,
#                 lower_slope_flat = csp_ergo_landforms_34,
#                 valley = csp_ergo_landforms_41,
#                 valley_narrow = csp_ergo_landforms_42) %>% 
#   dplyr::rename(trees = lcms_landcover_01,
#                 shrubs_trees_mix = lcms_landcover_03,
#                 grass_forbs_herb_trees_mix = lcms_landcover_04,
#                 barren_trees_mix = lcms_landcover_05,
#                 shrubs = lcms_landcover_07,
#                 grass_forb_herb_shrub_mix = lcms_landcover_08,
#                 barren_shrub_mix = lcms_landcover_09,
#                 grass_forb_herb = lcms_landcover_10,
#                 barren_grass_forb_herb_mix = lcms_landcover_11,
#                 barren = lcms_landcover_12) %>% 
#   dplyr::rename(fuel_stable = lcms_change_01,
#                 fuel_slow_loss = lcms_change_02,
#                 fuel_fast_loss = lcms_change_03,
#                 fuel_gain = lcms_change_04) %>% 
#   as.data.frame()

# fires_working <- 
#   fires %>% 
#   dplyr::rename(upper_slope_warm = csp_ergo_landforms_21,
#                 upper_slope = csp_ergo_landforms_22,
#                 lower_slope_warm = csp_ergo_landforms_31,
#                 lower_slope = csp_ergo_landforms_32) %>% 
#   dplyr::rename(trees = lcms_landcover_01,
#                 grass_forbs_herb_trees_mix = lcms_landcover_04) %>% 
#   dplyr::rename(fuel_stable = lcms_change_01,
#                 fuel_gain = lcms_change_04) %>% 
#   as.data.frame()

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

# ### Set 5
# human_drivers <- c("npl", "concurrent_fires", "friction")
# weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
#                      "max_temp_pct", "min_temp_pct", 
#                      "max_rh_pct", "min_rh_pct", 
#                      "max_vpd_pct", "min_vpd_pct", 
#                      "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
#                      "fm100_pct", "fm1000_pct")
# topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
# fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")


### Set 4
# human_drivers <- c("npl", "concurrent_fires", "friction")
# weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
#                      "max_temp_pct", "min_temp_pct", 
#                      "max_rh_pct", "min_rh_pct", 
#                      "max_vpd_pct", "min_vpd_pct", 
#                      "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
#                      "fm100_pct", "fm1000_pct")
# topography_drivers <- c("elevation", "rumple_index", "upper_slope",  "lower_slope", "landform_diversity")
# fuel_drivers <- c("ndvi", "change_diversity", "veg_structure_rumple", "landcover_diversity")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

### Set 3
# human_drivers <- c("npl", "npl_at_ignition", "concurrent_fires", "friction")
# weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
#                      "max_temp_pct", "min_temp_pct", 
#                      "max_rh_pct", "min_rh_pct", 
#                      "max_vpd_pct", "min_vpd_pct", 
#                      "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
#                      "fm100_pct", "fm1000_pct")
# topography_drivers <- c("elevation", "rumple_index", "upper_slope_warm", "upper_slope",  "lower_slope_warm", "lower_slope", "landform_diversity")
# fuel_drivers <- c("ndvi", "fuel_stable", "fuel_gain", "change_diversity", "veg_structure_rumple", "trees", "grass_forbs_herb_trees_mix", "landcover_diversity")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

### Set 2
# human_drivers <- c("npl", "concurrent_fires", "cumu_count", "cumu_area_ha", "friction", "friction_walking_only", "road_density_mpha")
# topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm", "peak_ridge", "peak_ridge_cool", "mountain_divide", "cliff", "upper_slope_warm", "upper_slope", "upper_slope_cool", "upper_slope_flat", "lower_slope_warm", "lower_slope", "lower_slope_cool", "lower_slope_flat", "valley", "valley_narrow", "landform_diversity")
# fuel_drivers <- c("ndvi", "fuel_stable", "fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "forest_structure_rumple", "trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren", "landcover_diversity", "change_diversity")
# fuel_drivers <- c("ndvi", "fuel_stable", "fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "veg_structure_rumple", "trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

### Set 1
# human_drivers <- c("npl", "concurrent_fires", "cumu_count", "cumu_area_ha", "friction", "friction_walking_only", "road_density_mpha")
# topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm", "peak_ridge", "peak_ridge_cool", "mountain_divide", "cliff", "upper_slope_warm", "upper_slope", "upper_slope_cool", "upper_slope_flat", "lower_slope_warm", "lower_slope", "lower_slope_cool", "lower_slope_flat", "valley", "valley_narrow")
# weather_drivers <- c("wind_anisotropy", "max_wind_speed_pct", "min_wind_speed_pct", 
#                      "max_temp_pct", "min_temp_pct", 
#                      "max_rh_pct", "min_rh_pct", "max_vpd_pct", "min_vpd_pct", "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", "fm100_pct", "fm1000_pct")
# fuel_drivers <- c("ndvi", "fuel_stable", "fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "forest_structure_rumple", "trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren", "landcover_diversity", "change_diversity")
# fuel_drivers <- c("ndvi", "fuel_stable", "fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "veg_structure_rumple", "trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# other_names <- c("did", "id", "megafire", "ig_year", "ig_month", "biome_name", "eco_name", "event_day", "aoi_ha", "c_area_ha", "aoir", "aoir_mod", "pred_aoi_l", "x_3310", "y_3310")

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

cor_mat <- cor(fires_working[, predictor.variable.names])
sort(cor_mat[, colnames(cor_mat) == "friction"])
sort(cor_mat[, colnames(cor_mat) == "fm1000_pct"])
sort(cor_mat[, colnames(cor_mat) == "npl"])
# # sort(cor_mat[, colnames(cor_mat) == "change_diversity"])
# sort(cor_mat[, colnames(cor_mat) == "grass_forb_herb_prop"])
# sort(cor_mat[, colnames(cor_mat) == "fuel_fast_loss_prop"])
# sort(cor_mat[, colnames(cor_mat) == "fm1000_pct"])
# 
# predictor.variable.names_reduced

##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

# desert_xeric_shrubland <- data[data$biome_name == "Deserts & Xeric Shrublands", ]
# xy_desert_xeric_shrubland <- desert_xeric_shrubland[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))
# 
# mediterranean_forest_woodland_scrub <- data[data$biome_name == "Mediterranean Forests, Woodlands & Scrub", ]
# xy_mediterranean_forest_woodland_scrub <- mediterranean_forest_woodland_scrub[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))
# 
# temperate_conifer_forests <- data[data$biome_name == "Temperate Conifer Forests", ]
# xy_temperate_conifer_forests <- temperate_conifer_forests[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))
# 
# temperate_grass_savanna_shrub <- data[data$biome_name == "Temperate Grasslands, Savannas & Shrublands", ]
# xy_temperate_grass_savanna_shrub <- temperate_grass_savanna_shrub[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))

#### ------ 
distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

tcf_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

tcf_dist_mat <- 
  sf::st_distance(x = tcf_sf, 
                  y = tcf_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_log10",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = tcf_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tcf_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tcf_nonspatial_response_curves_accentuate_gg

tcf_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_nonspatial,
    quantiles = c(0.5),
    ncol = 5,
    show.data = TRUE,
    line.color = "red"
  )

tcf_nonspatial_response_curves_accentuate_gg

(start_time <- Sys.time())
tcf_nonspatial_repeat <- spatialRF::rf_repeat(
  model = tcf_nonspatial, 
  repetitions = 30,
  seed = random_seed,
  verbose = FALSE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

spatialRF::plot_response_curves(
  tcf_nonspatial_repeat, 
  quantiles = 0.5,
  ncol = 3
)


### ---- Mediterranean Forest, Woodland & Scrub

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

mfws_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

mfws_dist_mat <- 
  sf::st_distance(x = mfws_sf, 
                  y = mfws_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
mfws_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_log10",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = mfws_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

mfws_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    mfws_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

mfws_nonspatial_response_curves_accentuate_gg


### ---- Temperate Grasslands, Savanna, & Shrub

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

tgss_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

tgss_dist_mat <- 
  sf::st_distance(x = tgss_sf, 
                  y = tgss_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
tgss_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_log10",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = tgss_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tgss_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tgss_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tgss_nonspatial_response_curves_accentuate_gg

### ---- Desert and Xeric Shrublands

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

dxs_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

dxs_dist_mat <- 
  sf::st_distance(x = dxs_sf, 
                  y = dxs_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
dxs_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_log10",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = dxs_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

dxs_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    dxs_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

ggsave(plot = (tcf_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title = "Temperate Conifer Forests")), 
       filename = "figs/tcf-rf-nonspatial-response-curves.png")
ggsave(plot = (mfws_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title  = "Mediterranean Forests, Woodlands & Scrub")), 
       filename = "figs/mfws-rf-nonspatial-response-curves.png")

ggsave(plot = (tgss_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title  = "Temperate Grasslands, Savannas & Shrublands")),
       filename = "figs/tgss-rf-nonspatial-response-curves.png")
ggsave(plot = (dxs_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title  = "Deserts & Xeric Shrublands")),
       filename = "figs/dxs-rf-nonspatial-response-curves.png")


###############################################
### Using residual of model with sqrt(previous day area) as the response

### Temperate conifer forest

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

#### ------ 
distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

tcf_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

tcf_dist_mat <- 
  sf::st_distance(x = tcf_sf, 
                  y = tcf_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "aoir_modeled_sqrtarea_tm1",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = tcf_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tcf_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tcf_nonspatial_response_curves_accentuate_gg

### ---- Mediterranean Forest, Woodland & Scrub

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

mfws_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

mfws_dist_mat <- 
  sf::st_distance(x = mfws_sf, 
                  y = mfws_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
mfws_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "aoir_modeled_sqrtarea_tm1",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = mfws_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

mfws_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    mfws_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

mfws_nonspatial_response_curves_accentuate_gg


### ---- Temperate Grasslands, Savanna, & Shrub

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

tgss_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

tgss_dist_mat <- 
  sf::st_distance(x = tgss_sf, 
                  y = tgss_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
tgss_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "aoir_modeled_sqrtarea_tm1",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = tgss_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tgss_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tgss_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tgss_nonspatial_response_curves_accentuate_gg

### ---- Desert and Xeric Shrublands

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  as.data.frame()

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", 
                     "max_vpd_pct", "min_vpd_pct", 
                     "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

dxs_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

dxs_dist_mat <- 
  sf::st_distance(x = dxs_sf, 
                  y = dxs_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
dxs_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "aoir_modeled_sqrtarea_tm1",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = dxs_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

dxs_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    dxs_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

ggsave(plot = (tcf_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title = "Temperate Conifer Forests")), 
       filename = "figs/tcf-rf-nonspatial-response-curves_aoir-sqrtarea-tm1.png")
ggsave(plot = (mfws_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title  = "Mediterranean Forests, Woodlands & Scrub")), 
       filename = "figs/mfws-rf-nonspatial-response-curves_aoir-sqrtarea-tm1.png")

ggsave(plot = (tgss_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title  = "Temperate Grasslands, Savannas & Shrublands")),
       filename = "figs/tgss-rf-nonspatial-response-curves_aoir-sqrtarea-tm1.png")
ggsave(plot = (dxs_nonspatial_response_curves_accentuate_gg + patchwork::plot_annotation(title  = "Deserts & Xeric Shrublands")),
       filename = "figs/dxs-rf-nonspatial-response-curves_aoir-sqrtarea-tm1.png")





######################### RTMA subset (fires 2011 and later)

### Temperate conifer forest

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v1.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -max_wind_speed_pct, -min_wind_speed_pct, -max_rh_pct, -min_rh_pct, -max_temp_pct, -min_temp_pct, -max_soil_water_pct, -min_soil_water_pct, -max_vpd_pct, -min_vpd_pct,
                -wind_anisotropy, -wind_terrain_anisotropy, -wind_terrain_alignment,
                -max_wind_speed_rtma, -min_wind_speed_rtma, -max_rh_rtma, -min_rh_rtma, -max_vpd_rtma, -min_vpd_rtma,
                -max_wind_gust_rtma, -min_wind_gust_rtma, -max_temp_rtma, -min_temp_rtma,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response)

fires_working <- 
  fires %>% 
  dplyr::filter(date >= as.Date("2011-01-01")) %>% 
  as.data.frame()

names(fires_working)

### Set 6
human_drivers <- c("npl", "concurrent_fires", "friction")
weather_drivers <- c("max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct", "max_wind_gust_rtma_pct", "min_wind_gust_rtma_pct",
                     "wind_anisotropy_rtma", 
                     "max_temp_rtma_pct", "min_temp_rtma_pct", 
                     "max_rh_rtma_pct", "min_rh_rtma_pct", 
                     "max_vpd_rtma_pct", "min_vpd_rtma_pct", 
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", 
                     "fm100_pct", "fm1000_pct")
topography_drivers <- c("elevation", "rumple_index", "landform_diversity")
fuel_drivers <- c("ndvi", "veg_structure_rumple", "landcover_diversity")
interacting_drivers <- c("wind_terrain_anisotropy_rtma", "wind_terrain_alignment_rtma", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# No NAs
apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires_working[, predictor.variable.names])[which(apply(fires_working[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 100)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires_working[!complete.cases(fires_working[, predictor.variable.names]), "id"])

fires_working <- fires_working[!(fires_working$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires_working[, predictor.variable.names])[round(apply(fires_working[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires_working <- 
  fires_working %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires_working[, predictor.variable.names]), 2, is.infinite))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", 
  "rumple_index", "elevation",
  "max_wind_speed_rtma_pct", "min_wind_speed_rtma_pct",
  "max_vpd_rtma_pct", "min_vpd_rtma_pct",
  "fm100_pct", "erc_pct", "spei1y",
  "ndvi", "veg_structure_rumple", "change_diversity",
  "wind_terrain_alignment_rtma"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires_working[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires_working
xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

#### ------ 
distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

tcf_sf <-
  data %>% 
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)

tcf_dist_mat <- 
  sf::st_distance(x = tcf_sf, 
                  y = tcf_sf) %>% 
  units::drop_units()

random_seed <- 1848

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "aoir_sqrtarea_tm1",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = tcf_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tcf_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tcf_nonspatial_response_curves_accentuate_gg














#############################################
##### ---- FEATURE ENGINEERING
dir.create("data/out/rf", showWarnings = FALSE)

# Feature engineering for the whole dataset
(start_time <- Sys.time())
ftr_eng_01 <- spatialRF::the_feature_engineer(data = data, 
                                              dependent.variable.name = "aoir_mod", 
                                              predictor.variable.names = predictor.variable.names_reduced, 
                                              xy = xy)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Fitting and evaluating a model without interactions.
# Testing 66 candidate interactions.
# No promising interactions found. 

readr::write_rds(x = predictor.variable.names_reduced, file = "data/out/rf/reduce-collinearity_preds-set-01_all-data.rds")
readr::write_rds(x = ftr_eng_01, file = "data/out/rf/feature-engineering_aoir-mod_preds-set-01_all-data.rds")

##### ---- Desert Xeric Shrublands
(start_time <- Sys.time())
ftr_eng_desert_xeric_shrubland <- spatialRF::the_feature_engineer(data = desert_xeric_shrubland, 
                                                                  dependent.variable.name = "aoir_mod", 
                                                                  predictor.variable.names = predictor.variable.names_reduced, 
                                                                  xy = xy_desert_xeric_shrubland)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

readr::write_rds(x = ftr_eng_desert_xeric_shrubland, file = "data/out/rf/feature-engineering_aoir-mod_preds-set-01_desert-xeric-shrubland.rds")

# Fitting and evaluating a model without interactions.
# Testing 66 candidate interactions.
# Interactions identified: 6
# ┌─────────────────────────────────────────┬───────────────────────┬───────────────────────┬─────────────────────────┐
# │ Interaction                             │ Importance (% of max) │ R-squared improvement │ Max cor with predictors │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ peak_ridge_prop..pca..forest_structure_ │                  83.1 │                 0.020 │                   0.75  │
# │ rumple                                  │                       │                       │                         │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ forest_structure_rumple..pca..rumple_in │                  67.8 │                 0.013 │                   0.72  │
# │ dex                                     │                       │                       │                         │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ forest_structure_rumple..pca..valley_pr │                  70.7 │                 0.024 │                   0.58  │
# │ op                                      │                       │                       │                         │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ valley_narrow_prop..pca..rumple_index   │                  61.9 │                 0.012 │                   0.72  │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ forest_structure_rumple..pca..trees_pro │                  69.7 │                 0.014 │                   0.35  │
# │ p                                       │                       │                       │                         │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ forest_structure_rumple..pca..valley_na │                  66.6 │                 0.012 │                   0.345 │
# │ rrow_prop                               │                       │                       │                         │
# └─────────────────────────────────────────┴───────────────────────┴───────────────────────┴─────────────────────────┘


##### ------ Mediterranean Forest Woodland Scrub

(start_time <- Sys.time())
ftr_eng_mediterranean_forest_woodland_scrub <- spatialRF::the_feature_engineer(data = mediterranean_forest_woodland_scrub, 
                                                                               dependent.variable.name = "aoir_mod", 
                                                                               predictor.variable.names = predictor.variable.names_reduced, 
                                                                               xy = xy_mediterranean_forest_woodland_scrub)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 38.72907 mins
# Fitting and evaluating a model without interactions.
# Testing 66 candidate interactions.
# No promising interactions found. 

readr::write_rds(x = ftr_eng_mediterranean_forest_woodland_scrub, file = "data/out/rf/feature-engineering_aoir-mod_preds-set-01_mediterranean-forest-woodland-scrub.rds")


##### ------- Temperate Conifer Forests
(start_time <- Sys.time())
ftr_eng_temperate_conifer_forests <- spatialRF::the_feature_engineer(data = temperate_conifer_forests, 
                                                                     dependent.variable.name = "aoir_mod", 
                                                                     predictor.variable.names = predictor.variable.names_reduced, 
                                                                     xy = xy_temperate_conifer_forests)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 77.58369 mins
# Fitting and evaluating a model without interactions.
# Testing 78 candidate interactions.
# No promising interactions found. 

readr::write_rds(x = ftr_eng_temperate_conifer_forests, file = "data/out/rf/feature-engineering_aoir-mod_preds-set-01_temperate-conifer-forests.rds")


##### ------- Temperate Grass Savanna Shrub
(start_time <- Sys.time())
ftr_eng_temperate_grass_savanna_shrub <- spatialRF::the_feature_engineer(data = temperate_grass_savanna_shrub, 
                                                                         dependent.variable.name = "aoir_mod", 
                                                                         predictor.variable.names = predictor.variable.names_reduced, 
                                                                         xy = xy_temperate_grass_savanna_shrub)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 47.32638 mins
# Fitting and evaluating a model without interactions.
# Testing 66 candidate interactions.
# No promising interactions found.
readr::write_rds(x = ftr_eng_temperate_grass_savanna_shrub, file = "data/out/rf/feature-engineering_aoir-mod_preds-set-01_temperate-grass-savanna-shrub.rds")

##### ------ PLOT TRAINING DATA

##### ---- Desert Xeric Shrublands

##### ------ Mediterranean Forest Woodland Scrub
mediterranean_forest_woodland_scrub_training_gg <- 
  spatialRF::plot_training_df(
    data = mediterranean_forest_woodland_scrub,
    dependent.variable.name = "aoir_mod",
    predictor.variable.names = predictor.variable.names_reduced,
    ncol = 5,
    point.color = viridis::viridis(100, option = "F"),
    line.color = "gray30"
  )

ggsave(filename = "data/out/rf/training-data-plot_aoir-mod_preds-set-01_mediterranean_forest_woodland_scrub.png", 
       plot = mediterranean_forest_woodland_scrub_training_gg)

##### ------- Temperate Conifer Forests
temperate_conifer_forests_training_gg <- 
  spatialRF::plot_training_df(
    data = temperate_conifer_forests,
    dependent.variable.name = "aoir_mod",
    predictor.variable.names = predictor.variable.names_reduced,
    ncol = 5,
    point.color = viridis::viridis(100, option = "F"),
    line.color = "gray30"
  )

ggsave(filename = "data/out/rf/training-data-plot_aoir-mod_preds-set-01_temperate-conifer-forests.png", 
       plot = temperate_conifer_forests_training_gg)

##### ------- Temperate Grass Savanna Shrub
temperate_grass_savanna_shrub_training_gg <- 
  spatialRF::plot_training_df(
    data = temperate_grass_savanna_shrub,
    dependent.variable.name = "aoir_mod",
    predictor.variable.names = predictor.variable.names_reduced,
    ncol = 5,
    point.color = viridis::viridis(100, option = "F"),
    line.color = "gray30"
  )

ggsave(filename = "data/out/rf/training-data-plot_aoir-mod_preds-set-01_temperate-grass-savanna-shrub.png", 
       plot = temperate_grass_savanna_shrub_training_gg)

##### ----- Get distance matrices and thresholds
distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

desert_xeric_shrubland_sf <-
  desert_xeric_shrubland %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

desert_xeric_shrubland_dist_mat <- sf::st_distance(x = desert_xeric_shrubland_sf, 
                                                   y = desert_xeric_shrubland_sf)

mediterranean_forest_woodland_scrub_sf <-
  mediterranean_forest_woodland_scrub %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

mediterranean_forest_woodland_scrub_dist_mat <- 
  sf::st_distance(x = mediterranean_forest_woodland_scrub_sf, 
                  y = mediterranean_forest_woodland_scrub_sf) %>% 
  units::drop_units()

temperate_conifer_forests_sf <-
  temperate_conifer_forests %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

temperate_conifer_forests_dist_mat <- 
  sf::st_distance(x = temperate_conifer_forests_sf, 
                  y = temperate_conifer_forests_sf) %>% 
  units::drop_units()

temperate_grass_savanna_shrub_sf <-
  temperate_grass_savanna_shrub %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

temperate_grass_savanna_shrub_dist_mat <- 
  sf::st_distance(x = temperate_grass_savanna_shrub_sf, 
                  y = temperate_grass_savanna_shrub_sf) %>% 
  units::drop_units()

##### ------ PLOT TRAINING DATA MORAN
##### ---- Desert Xeric Shrublands

# need to sort out PCA interactions before proceeding for this biome
# desert_xeric_shrubland_training_moran_gg <-
#   spatialRF::plot_training_df_moran(
#     data = desert_xeric_shrubland,
#     dependent.variable.name = "aoir_mod",
#     predictor.variable.names = ,
#     distance.matrix = desert_xeric_shrubland_dist_mat,
#     distance.thresholds = distance_thresholds,
#     fill.color = viridis::viridis(
#       100,
#       option = "F",
#       direction = -1
#     ),
#     point.color = "gray40"
#   )

##### ------ Mediterranean Forest Woodland Scrub

mediterranean_forest_woodland_scrub_training_moran_gg <-
  spatialRF::plot_training_df_moran(
    data = mediterranean_forest_woodland_scrub,
    dependent.variable.name = "aoir_mod",
    predictor.variable.names = predictor.variable.names_reduced,
    distance.matrix = mediterranean_forest_woodland_scrub_dist_mat,
    distance.thresholds = distance_thresholds,
    fill.color = viridis::viridis(
      100,
      option = "F",
      direction = -1
    ),
    point.color = "gray40"
  )

ggsave(filename = "data/out/rf/training-data-moran-plot_aoir-mod_preds-set-01_mediterranean-forest-woodland-scrub.png", 
       plot = mediterranean_forest_woodland_scrub_training_moran_gg)

##### ------- Temperate Conifer Forests

temperate_conifer_forests_training_moran_gg <-
  spatialRF::plot_training_df_moran(
    data = temperate_conifer_forests,
    dependent.variable.name = "aoir_mod",
    predictor.variable.names = predictor.variable.names_reduced,
    distance.matrix = temperate_conifer_forests_dist_mat,
    distance.thresholds = distance_thresholds,
    fill.color = viridis::viridis(
      100,
      option = "F",
      direction = -1
    ),
    point.color = "gray40"
  )

ggsave(filename = "data/out/rf/training-data-moran-plot_aoir-mod_preds-set-01_temperate-conifer-forests.png", 
       plot = temperate_conifer_forests_training_moran_gg)

##### ------- Temperate Grass Savanna Shrub
# didn't work? not sure why.
# Error in if (moran.i.observed > moran.i.expected & p.value <= 0.05) { : 
#     missing value where TRUE/FALSE needed

temperate_grass_savanna_shrub_training_moran_gg <-
  spatialRF::plot_training_df_moran(
    data = temperate_grass_savanna_shrub,
    dependent.variable.name = "aoir_mod",
    predictor.variable.names = predictor.variable.names_reduced,
    distance.matrix = temperate_grass_savanna_shrub_dist_mat,
    distance.thresholds = distance_thresholds,
    fill.color = viridis::viridis(
      100,
      option = "F",
      direction = -1
    ),
    point.color = "gray40"
  )

ggsave(filename = "data/out/rf/training-data-moran-plot_aoir-mod_preds-set-01_temperate-grass-savanna-shrub.png", 
       plot = temperate_grass_savanna_shrub_training_moran_gg)

##### ------ RANDOM FOREST MODELS
# Note: these run fast (the non-spatial versions); less than a minute each
random_seed <- 1418

desert_xeric_shrubland_dist_mat
mediterranean_forest_woodland_scrub_dist_mat
temperate_conifer_forests_dist_mat
temperate_grass_savanna_shrub_dist_mat

##### ---- Desert Xeric Shrublands
# need to sort out PCA interactions before proceeding
desert_xeric_shrubland
xy_desert_xeric_shrubland

dxs_nonspatial <- spatialRF::rf(
  data = desert_xeric_shrubland,
  dependent.variable.name = "aoir_mod",
  predictor.variable.names = ,
  distance.matrix = desert_xeric_shrubland_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy_desert_xeric_shrubland, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = FALSE
)


##### ------ Mediterranean Forest Woodland Scrub
# mediterranean_forest_woodland_scrub
# xy_mediterranean_forest_woodland_scrub

mfws_nonspatial <- spatialRF::rf(
  data = mediterranean_forest_woodland_scrub,
  dependent.variable.name = "aoir_mod",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = mediterranean_forest_woodland_scrub_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy_mediterranean_forest_woodland_scrub, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)


##### ------- Temperate Conifer Forests
# temperate_conifer_forests
# xy_temperate_conifer_forests

tcf_nonspatial <- spatialRF::rf(
  data = temperate_conifer_forests,
  dependent.variable.name = "aoir_mod",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = temperate_conifer_forests_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy_temperate_conifer_forests, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)


##### ------- Temperate Grass Savanna Shrub
# temperate_grass_savanna_shrub
# xy_temperate_grass_savanna_shrub

tgss_nonspatial <- spatialRF::rf(
  data = temperate_grass_savanna_shrub,
  dependent.variable.name = "aoir_mod",
  predictor.variable.names = predictor.variable.names_reduced,
  distance.matrix = temperate_grass_savanna_shrub_dist_mat,
  distance.thresholds = distance_thresholds,
  xy = xy_temperate_grass_savanna_shrub, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)

##### ------- RESIDUAL DIAGNOSTICS
# dxs_nonspatial_residual_gg <-
# spatialRF::plot_residuals_diagnostics(
#   dxs_nonspatial,
#   verbose = FALSE
# )

mfws_nonspatial_residual_gg <- 
  spatialRF::plot_residuals_diagnostics(
    mfws_nonspatial,
    verbose = FALSE
  )


tcf_nonspatial_residual_gg <-
  spatialRF::plot_residuals_diagnostics(
    tcf_nonspatial,
    verbose = FALSE
  )

tgss_nonspatial_residual_gg <-
  spatialRF::plot_residuals_diagnostics(
    tgss_nonspatial,
    verbose = FALSE
  )


# ggsave(filename = "data/out/rf/nonspatial-residual-plot_aoir-mod_pred-set-01_desert-xeric-shrubland.png", 
#        plot = dxs_nonspatial_residual_gg)

ggsave(filename = "data/out/rf/nonspatial-residual-plot_aoir-mod_pred-set-01_mediterranean-woodland-forest-scrub.png", 
       plot = mfws_nonspatial_residual_gg)

ggsave(filename = "data/out/rf/nonspatial-residual-plot_aoir-mod_pred-set-01_temperate-conifer-forest.png", 
       plot = tcf_nonspatial_residual_gg)

ggsave(filename = "data/out/rf/nonspatial-residual-plot_aoir-mod_pred-set-01_temperate-grass-savanna-shrub.png", 
       plot = tgss_nonspatial_residual_gg)


##### ------ GLOBAL VARIABLE IMPORTANCE

# dxs_nonspatial_global_variable_import_gg <-
#   spatialRF::plot_importance(
#     dxs_nonspatial,
#     verbose = FALSE
#   )

mfws_nonspatial_global_variable_import_gg <-
  spatialRF::plot_importance(
    mfws_nonspatial,
    verbose = FALSE
  )

tcf_nonspatial_global_variable_import_gg <-
  spatialRF::plot_importance(
    tcf_nonspatial,
    verbose = FALSE
  )

tgss_nonspatial_global_variable_import_gg <-
  spatialRF::plot_importance(
    tgss_nonspatial,
    verbose = FALSE
  )

##### ------ MEASURE IMPORTANCE
# About 6 minutes of run time per model

# (start_time <- Sys.time())
# dxs_importance_df <- randomForestExplainer::measure_importance(
#   forest = dxs_nonspatial,
#   measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
# )
# (end_time <- Sys.time())
# (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 
# write.csv(x = mfws_importance_df, 
#           file = "data/out/rf/nonspatial-measure-importance_aoir-mod_pred-set-01_desert-xeric-shrubland.csv", 
#           row.names = FALSE)


(start_time <- Sys.time())
mfws_importance_df <- randomForestExplainer::measure_importance(
  forest = mfws_nonspatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

write.csv(x = mfws_importance_df, 
          file = "data/out/rf/nonspatial-measure-importance_aoir-mod_pred-set-01_mediterranean-forest-woodland-scrub.csv", 
          row.names = FALSE)


(start_time <- Sys.time())
tcf_importance_df <- randomForestExplainer::measure_importance(
  forest = tcf_nonspatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

write.csv(x = tcf_importance_df, 
          file = "data/out/rf/nonspatial-measure-importance_aoir-mod_pred-set-01_temperate-conifer-forest.csv", 
          row.names = FALSE)


(start_time <- Sys.time())
tgss_importance_df <- randomForestExplainer::measure_importance(
  forest = tgss_nonspatial,
  measures = c("mean_min_depth", "no_of_nodes", "times_a_root", "p_value")
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

write.csv(x = tgss_importance_df, 
          file = "data/out/rf/nonspatial-measure-importance_aoir-mod_pred-set-01_temperate-grass-savanna-shrub.csv", 
          row.names = FALSE)

##### ----- RF IMPORTANCE
# note this code reassigns some slots of the original model

# (start_time <- Sys.time())
# dxs_nonspatial <- spatialRF::rf_importance(model = dxs_nonspatial)
# (end_time <- Sys.time())
# (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# readr::write_rds(x = dxs_nonspatial, file = "data/out/rf/rf-nonspatial-with-importance_aoir-mod_pred-set-01_desert-xeric-shrubland.rds")

(start_time <- Sys.time())
mfws_nonspatial <- spatialRF::rf_importance(model = mfws_nonspatial)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 24 minutes

readr::write_rds(x = mfws_nonspatial, file = "data/out/rf/rf-nonspatial-with-importance_aoir-mod_pred-set-01_mediterranean-forest-woodland-scrub.rds")

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf_importance(model = tcf_nonspatial)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 33 minutes

readr::write_rds(x = tcf_nonspatial, file = "data/out/rf/rf-nonspatial-with-importance_aoir-mod_pred-set-01_temperate-conifer-forest.rds")

(start_time <- Sys.time())
tgss_nonspatial <- spatialRF::rf_importance(model = tgss_nonspatial)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 26 minutes

readr::write_rds(x = tgss_nonspatial, file = "data/out/rf/rf-nonspatial-with-importance_aoir-mod_pred-set-01_temperate-grass-savanna-shrub.rds")


tcf_nonspatial$importance$per.variable %>% 
  ggplot2::ggplot() +
  ggplot2::aes(
    x = importance.oob,
    y = importance.cv
  ) + 
  ggplot2::geom_point(size = 3) + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Importance (out-of-bag)") + 
  ggplot2::ylab("Contribution to transferability") + 
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "red4")





##### ------ IMPORTANCE VALUES OF VARIABLES IN SPACE

tcf_local_importance <- spatialRF::get_importance_local(tcf_nonspatial)
tcf_local_importance <- 
  cbind(xy_temperate_conifer_forests, tcf_local_importance) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

#colors
color.low <- viridis::viridis(3, option = "F")[2]
color.high <- viridis::viridis(3, option = "F")[1]

ca <- USAboundaries::us_states(resolution = "low", states = "California")

ggplot(tcf_local_importance) +
  geom_sf(data = ca, fill = NA) +
  geom_sf(data = tcf_local_importance, aes(color = forest_structure_rumple)) +
  scale_color_gradient2(low = color.low, high = color.high) +
  theme_bw() +
  labs(color = "Forest structure\nrumple importance")

ggplot(tcf_local_importance) +
  geom_sf(data = ca, fill = NA) +
  geom_sf(data = tcf_local_importance, aes(color = valley_prop)) +
  scale_color_gradient2(low = color.low, high = color.high) +
  theme_bw() +
  labs(color = "Valley proportion\nimportance")


#### -----  PLOT RESPONSE CURVES
tcf_response_curves_gg <-
  spatialRF::plot_response_curves(model = tcf_nonspatial,
                                  quantiles = c(0.1, 0.5, 0.9),
                                  line.color = viridis::viridis(
                                    3, #same number of colors as quantiles
                                    option = "C", 
                                    end = 0.9
                                  ),
                                  ncol = 5,
                                  show.data = TRUE)

tcf_response_curves_gg

tcf_response_curves_accentuate_gg <- 
  spatialRF::plot_response_curves(model = tcf_nonspatial,
                                  quantiles = 0.5,
                                  ncol = 3)

tcf_response_curves_accentuate_gg

pdp::partial(object = tcf_nonspatial, 
             train = temperate_conifer_forests, 
             pred.var = "forest_structure_rumple", 
             plot = TRUE, 
             grid.resolution = 200
)


##### ----- PLOT RESPONSE SURFACES (INTERACTIONS)

# from the old thread: "I think wind speed and NDVI and wind speed and rumple_index might be good ones, given the Evers et al 2021 preprint (“Extreme Winds Flip Inuence of Fuels and Topography on Megare Burn Severity in Mesic Conifer Forests Under Record Fuel Aridity”) and perhaps wind speed * multi-year drought and wind speed * shorter term fuel moisture (fm100_pct maybe? VPD_pct?) based on Yufang’s paper (“Contrasting controls on wildland fires in Southern California during periods with and without Santa Ana winds”)"

spatialRF::plot_response_surface(model = tcf_nonspatial,
                                 a = "ndvi",
                                 b = "min_wind_speed_pct")

spatialRF::plot_response_surface(model = tcf_nonspatial,
                                 a = "rumple_index",
                                 b = "max_wind_speed_pct")

spatialRF::plot_response_surface(model = tcf_nonspatial,
                                 a = "spei1y",
                                 b = "max_wind_speed_pct")

sort(predictor.variable.names_reduced$selected.variables)


spatialRF::print_performance(tcf_nonspatial)




##### ----- SPATIAL CROSS VALIDATION
# Less than a minute per model

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf_evaluate(
  model = tcf_nonspatial,
  xy = xy_temperate_conifer_forests,      #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  seed = random_seed,
  verbose = FALSE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

readr::write_rds(x = tcf_nonspatial, file = "data/out/rf/rf-nonspatial-with-importance-and-spatial-cross-validation_aoir-mod_pred-set-01_temperate-conifer-forest.rds")

spatialRF::plot_evaluation(tcf_nonspatial)

spatialRF::print_evaluation(tcf_nonspatial)
# Spatial evaluation 
# - Training fraction:             0.75
# - Spatial folds:                 29
# 
# Metric Median   MAD Minimum Maximum
# r.squared  0.555 0.024   0.515    0.61


##### ----- ASSESSING SPATIAL AUTOCORRELATION
tcf_nonspatial <- readr::read_rds(file = "data/out/rf/rf-nonspatial-with-importance-and-spatial-cross-validation_aoir-mod_pred-set-01_temperate-conifer-forest.rds")

# spatialRF::plot_moran(model = tcf_nonspatial, verbose = FALSE)

(start_time <- Sys.time())
tcf_spatial <- spatialRF::rf_spatial(
  model = tcf_nonspatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random_seed
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 1974 minutes (~33 hours)

readr::write_rds(x = tcf_spatial, file = "data/out/rf/rf-spatial_aoir-mod_pred-set-01_temperate-conifer-forest.rds")
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))



temperate_conifer_forests



# More consolidated versions of proportional landcovers (landcover, change, and landform layers)
# peak_ridge_prop = csp_ergo_landforms_11_prop + csp_ergo_landforms_12_prop + csp_ergo_landforms_13_prop + csp_ergo_landforms_14_prop + csp_ergo_landforms_15_prop
# upper_slope_prop = csp_ergo_landforms_21_prop + csp_ergo_landforms_22_prop + csp_ergo_landforms_23_prop + csp_ergo_landforms_24_prop
# lower_slope_prop = csp_ergo_landforms_31_prop + csp_ergo_landforms_32_prop + csp_ergo_landforms_33_prop + csp_ergo_landforms_34_prop
# valley_prop = csp_ergo_landforms_41_prop + csp_ergo_landforms_42_prop
#
# fuel_stable_prop = lcms_change_01_prop,
# fuel_slow_loss_prop = lcms_change_02_prop,
# fuel_fast_loss_prop = lcms_change_03_prop,
# fuel_gain_prop = lcms_change_04_prop
# 
# trees_prop = lcms_landcover_01_prop + lcms_landcover_03_prop + lcms_landcover_04_prop + lcms_landcover_05_prop
# shrubs_prop = lcms_landcover_07_prop + lcms_landcover_08_prop + lcms_landcover_09_prop
# grass_prop = lcms_landcover_10_prop + lcms_landcover_11_prop
# barren_prop = lcms_landcover_12_prop

data <- 
  fires_working %>% 
  dplyr::mutate(peak_ridge_prop = csp_ergo_landforms_11_prop + csp_ergo_landforms_12_prop + csp_ergo_landforms_13_prop + csp_ergo_landforms_14_prop + csp_ergo_landforms_15_prop,
                upper_slope_prop = csp_ergo_landforms_21_prop + csp_ergo_landforms_22_prop + csp_ergo_landforms_23_prop + csp_ergo_landforms_24_prop,
                lower_slope_prop = csp_ergo_landforms_31_prop + csp_ergo_landforms_32_prop + csp_ergo_landforms_33_prop + csp_ergo_landforms_34_prop,
                valley_prop = csp_ergo_landforms_41_prop + csp_ergo_landforms_42_prop) %>% 
  dplyr::mutate(trees_prop = lcms_landcover_01_prop + lcms_landcover_03_prop + lcms_landcover_04_prop + lcms_landcover_05_prop,
                shrubs_prop = lcms_landcover_07_prop + lcms_landcover_08_prop + lcms_landcover_09_prop,
                grass_prop = lcms_landcover_10_prop + lcms_landcover_11_prop,
                barren_prop = lcms_landcover_12_prop) %>% 
  dplyr::rename(fuel_stable_prop = lcms_change_01_prop,
                fuel_slow_loss_prop = lcms_change_02_prop,
                fuel_fast_loss_prop = lcms_change_03_prop,
                fuel_gain_prop = lcms_change_04_prop) %>% 
  dplyr::select(-starts_with("csp_ergo_landforms"), -starts_with("lcms_change"), -starts_with("lcms_landcover")) %>% 
  dplyr::mutate(megafire = ifelse(megafire == "megafire", 1, 0)) %>% 
  as.data.frame()

data

human_drivers <- c("npl", "concurrent_fires", "cumu_count", "cumu_area_ha", "friction", "friction_walking_only", "road_density_mpha")
topography_drivers <- c("elevation", "rumple_index", "peak_ridge_prop", "upper_slope_prop", "lower_slope_prop", "valley_prop", "landform_diversity")
weather_drivers <- c("wind_anisotropy", "max_wind_speed_pct", "min_wind_speed_pct", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", "max_vpd_pct", "min_vpd_pct", "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei180d", "spei1y", "spei270d", "spei30d", "spei90d", "fm100_pct", "fm1000_pct")
fuel_drivers <- c("ndvi", "fuel_stable_prop", "fuel_slow_loss_prop", "fuel_fast_loss_prop", "fuel_gain_prop", "forest_structure_rumple", "trees_prop", "shrubs_prop", "grass_prop", "barren_prop", "landcover_diversity", "change_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

# The data required to fit random forest models with spatialRF must fulfill several conditions:
sum(apply(data, 2, is.na))
apply(data, 2, function(x) return(sum(is.na(x))))

data <-
  data %>% 
  dplyr::select(-spei2y, -spei5y, -pdsi_z)

data <- na.omit(data)

sum(apply(scale(data[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(data[, predictor.variable.names]), 2, is.infinite))

xy <- data[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))

# Reduce collinearity in the predictors
preference.order <- c(
  "npl", "road_density_mpha",
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "spei1y", "fm100_pct",
  "ndvi", "forest_structure_rumple", "trees_prop", "grass_prop", "shrubs_prop", "barren_prop", "fuel_fast_loss_prop", "fuel_slow_loss_prop", "fuel_gain_prop",
  "wind_terrain_alignment"
)

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = data[, predictor.variable.names],
  cor.threshold = 0.65,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 2.5,
    preference.order = preference.order
  )

ftr_eng_01 <- spatialRF::the_feature_engineer(data = data, 
                                              dependent.variable.name = "aoir_mod", 
                                              predictor.variable.names = predictor.variable.names_reduced, 
                                              xy = xy)
# Fitting and evaluating a model without interactions.
# Testing 21 candidate interactions.
# Interactions identified: 2
# ┌─────────────────────────────────────────┬───────────────────────┬───────────────────────┬─────────────────────────┐
# │ Interaction                             │ Importance (% of max) │ R-squared improvement │ Max cor with predictors │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ forest_structure_rumple..pca..fuel_slow │                 100.0 │                 0.021 │                   0.22  │
# │ _loss_prop                              │                       │                       │                         │
# ├─────────────────────────────────────────┼───────────────────────┼───────────────────────┼─────────────────────────┤
# │ forest_structure_rumple..pca..road_dens │                  94.5 │                 0.025 │                   0.182 │
# │ ity_mpha                                │                       │                       │                         │
# └─────────────────────────────────────────┴───────────────────────┴───────────────────────┴─────────────────────────┘
# Comparing models with and without interactions via spatial cross-validation.
# notch went outside hinges. Try setting notch=FALSE.

#adding interaction column to the training data
data_for_model <- ftr_eng_01$data

#adding interaction name to predictor.variable.names
predictor.variable.names_model <- ftr_eng_01$predictor.variable.names

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

desert_xeric_shrubland <- data_for_model[data_for_model$biome_name == "Deserts & Xeric Shrublands", ]

desert_xeric_shrubland_sf <-
  desert_xeric_shrubland %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

desert_xeric_shrubland_dist_mat <- sf::st_distance(x = desert_xeric_shrubland_sf, y = desert_xeric_shrubland_sf)

desert_xeric_shrubland_rf <- spatialRF::rf(data = desert_xeric_shrubland, 
                                           dependent.variable.name = "aoir_mod", 
                                           predictor.variable.names = predictor.variable.names_model, 
                                           distance.matrix = units::drop_units(desert_xeric_shrubland_dist_mat),
                                           distance.thresholds = distance.thresholds)


ftr_eng_02 <- spatialRF::the_feature_engineer(data = data, 
                                              dependent.variable.name = "aoi_log", 
                                              predictor.variable.names = predictor.variable.names_reduced, 
                                              xy = xy)

ftr_eng_03 <- spatialRF::the_feature_engineer(data = data, 
                                              dependent.variable.name = "aoir_mod", 
                                              predictor.variable.names = predictor.variable.names_reduced, 
                                              xy = xy)

#adding interaction column to the training data
data_for_model <- desert_xeric_shrubland <- data[data$biome_name == "Deserts & Xeric Shrublands", ]


#adding interaction name to predictor.variable.names
predictor.variable.names_model <- ftr_eng_03$predictor.variable.names

desert_xeric_shrubland_rf_03 <- spatialRF::rf(data = desert_xeric_shrubland, 
                                              dependent.variable.name = "aoir_mod", 
                                              predictor.variable.names = predictor.variable.names_reduced, 
                                              distance.matrix = units::drop_units(desert_xeric_shrubland_dist_mat),
                                              distance.thresholds = distance.thresholds)


ggplot(data, aes(x = forest_structure_rumple, y = fuel_slow_loss_prop, color = aoi_log)) + 
  geom_point()

ggplot(data, aes(x = forest_structure_rumple, y = aoi_log)) + 
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = fuel_slow_loss_prop, y = aoi_log, color = forest_structure_rumple)) + 
  geom_point() +
  geom_smooth() +
  scale_color_viridis_c()





mediterranean_forest_woodland_scrub <- data_for_model[data_for_model$biome_name == "Mediterranean Forests, Woodlands & Scrub", ]
temperate_conifer_forests <- data_for_model[data_for_model$biome_name == "Temperate Conifer Forests", ]
temperate_grass_savanna_shrub <- data_for_model[data_for_model$biome_name == "Temperate Grasslands, Savannas & Shrublands", ]


rf1 <- spatialRF::rf(data = data, dependent.variable.name = "aoir", predictor.variable.names = predictor.variable.names, distance.matrix = NULL)



summary(rf1)


mediterranean_forest_woodland_scrub_rf <- spatialRF::rf(data = mediterranean_forest_woodland_scrub, 
                                                        dependent.variable.name = "aoir", 
                                                        predictor.variable.names = predictor.variable.names, 
                                                        distance.matrix = NULL)

temperate_conifer_forests_rf <- spatialRF::rf(data = temperate_conifer_forests, 
                                              dependent.variable.name = "aoir", 
                                              predictor.variable.names = predictor.variable.names, 
                                              distance.matrix = NULL)

temperate_grass_savanna_shrub_rf <- spatialRF::rf(data = temperate_grass_savanna_shrub, 
                                                  dependent.variable.name = "aoir", 
                                                  predictor.variable.names = predictor.variable.names, 
                                                  distance.matrix = NULL)


data(plant_richness_df)
data(distance_matrix)


desert_xeric_shrubland_rf <- spatialRF::rf(data = desert_xeric_shrubland, 
                                           dependent.variable.name = "aoir", 
                                           predictor.variable.names = predictor.variable.names, 
                                           distance.matrix = desert_xeric_shrubland_dist_mat)


?st_distance
mode(desert_xeric_shrubland_dist_mat)
class(distance_matrix)
