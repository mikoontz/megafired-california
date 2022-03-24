library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyr)
library(terra)
library(fasterize)
library(spatialRF)

# read in fire data
fires <-
  data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california_v4.csv") %>%
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

events <-
  sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>%
  sf::st_transform(3310) %>%
  sf::st_centroid() %>%
  dplyr::rename(geometry = geom) %>%
  dplyr::mutate(x_3310 = sf::st_coordinates(.)[, "X"],
                y_3310 = sf::st_coordinates(.)[, "Y"]) %>%
  dplyr::filter(tot_hect >= 120) %>%
  dplyr::filter(ig_date >= lubridate::ymd("2003-01-01") & ig_date <= lubridate::ymd("2020-12-31"))


target_fires <-
  fires %>%
  dplyr::filter(tot_hect >= 120) %>% 
  dplyr::filter(ig_date >= lubridate::ymd("2003-01-01") & ig_date <= lubridate::ymd("2020-12-31"))

fires_working <-
  target_fires %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-lcms_landuse_01, -lcms_landuse_02, -lcms_landuse_04, -lcms_landuse_07,
                -lcms_landcover_02, -lcms_landcover_06, -lcms_landcover_02, -lcms_landcover_13, -lcms_landcover_14, -lcms_landcover_15, 
                -lcms_change_05,
                -name_frap, -olap_frap, -ig_date, -last_date, -date, -ends_with("rank"), -frp_90, -pred_aoi, -c_area_tm1, -event_dur, -aoir_mod, -event_modis_lc, -daily_modis_lc,
                -max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -ndvi_proj_area, -ndvi_surf_area, -proj_area, -surf_area, -road_length_m,
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"), -surf_area_ha, -proj_area_ha, -starts_with("lcms_landuse"),
                -npl_at_ignition,
                -tot_hect, -mecdf)

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
  # dplyr::left_join(y = events_resolve) %>%
  dplyr::mutate(megafire = ifelse(megafire == "megafire", 1, 0)) %>% 
  as.data.frame()

data

human_drivers <- c("npl", "concurrent_fires", "cumu_count", "cumu_area_ha", "friction", "friction_walking_only", "road_density_mpha")
topography_drivers <- c("elevation", "rumple_index", "peak_ridge_prop", "upper_slope_prop", "lower_slope_prop", "valley_prop")
weather_drivers <- c("wind_anisotropy", "max_wind_speed_pct", "min_wind_speed_pct", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", "max_vpd_pct", "min_vpd_pct", "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei180d", "spei1y", "spei270d", "spei30d", "spei90d", "fm100_pct", "fm1000_pct")
fuel_drivers <- c("ndvi", "fuel_stable_prop", "fuel_slow_loss_prop", "fuel_fast_loss_prop", "fuel_gain_prop", "forest_structure_rumple", "trees_prop", "shrubs_prop", "grass_prop", "barren_prop")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)
length(predictor.variable.names)

length(c("did", "id", "megafire", "ig_year", "ig_month", "biome_name", "eco_name", "event_day", "aoi_ha", "c_area_ha", "aoir", "pred_aoi_l", "x_3310", "y_3310"))

# The data required to fit random forest models with spatialRF must fulfill several conditions:
#   
sum(apply(data, 2, is.na))
apply(data, 2, function(x) return(sum(is.na(x))))

data <-
  data %>% 
  dplyr::select(-spei2y, -spei5y, -pdsi_z)

data <- na.omit(data)

sum(apply(scale(data[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(data[, predictor.variable.names]), 2, is.infinite))

# Columns must not yield NaN or Inf when scaled. You can check each condition with sum(apply(scale(df), 2, is.nan)) and sum(apply(scale(df), 2, is.infinite)). If higher than 0, you can find what columns are giving issues with sapply(as.data.frame(scale(df)), function(x)any(is.nan(x))) and sapply(as.data.frame(scale(df)), function(x)any(is.infinite(x))). Any column yielding TRUE will generate issues while trying to fit models with spatialRF.

rf1 <- spatialRF::rf(data = data, dependent.variable.name = "aoir", predictor.variable.names = predictor.variable.names, distance.matrix = NULL)
rf2 <- spatialRF::rf(data = data, dependent.variable.name = "aoi_ha", predictor.variable.names = predictor.variable.names, distance.matrix = NULL)

summary(rf1)

desert_xeric_shrubland <- data[data$biome_name == "Deserts & Xeric Shrublands", ]
mediterranean_forest_woodland_scrub <- data[data$biome_name == "Mediterranean Forests, Woodlands & Scrub", ]
temperate_conifer_forests <- data[data$biome_name == "Temperate Conifer Forests", ]
temperate_grass_savanna_shrub <- data[data$biome_name == "Temperate Grasslands, Savannas & Shrublands", ]

desert_xeric_shrubland_rf <- spatialRF::rf(data = desert_xeric_shrubland, 
                                           dependent.variable.name = "aoir", 
                                           predictor.variable.names = predictor.variable.names, 
                                           distance.matrix = NULL)

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
