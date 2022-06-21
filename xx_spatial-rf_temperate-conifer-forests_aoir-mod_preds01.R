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
fires <-
  data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california_v5.csv") %>%
  dplyr::filter(tot_hect >= 120) %>% 
  dplyr::filter(ig_date >= lubridate::ymd("2003-01-01") & ig_date <= lubridate::ymd("2020-12-31")) %>% 
  dplyr::select(-(starts_with("lcms_landuse")), # remove all landuse raw area coverage and proportion values
                -(starts_with("lcms_landcover") & !ends_with("prop")), # remove all landcover raw area coverage values in favor of proportions
                -(starts_with("lcms_change") & !ends_with("prop")), # remove all change raw area coverage values in favor of proportions
                -(starts_with("csp") & !ends_with("prop")), # remove all CSP landform raw area coverage values in favor of proportions
                -lcms_landcover_02_prop, -lcms_landcover_06_prop, -lcms_landcover_02_prop, -lcms_landcover_13_prop, -lcms_landcover_14_prop, -lcms_landcover_15_prop, 
                -lcms_change_05_prop,
                -name_frap, -olap_frap, -ig_date, -last_date, -date, -ends_with("rank"), -frp_90, -pred_aoi, -c_area_tm1, -event_dur, -event_modis_lc, -daily_modis_lc,
                -max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -ndvi_proj_area, -ndvi_surf_area, -proj_area, -surf_area, -road_length_m,
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"), -surf_area_ha, -proj_area_ha,
                -npl_at_ignition,
                -tot_hect, -mecdf) %>% 
  # add shannon diversity index of landcover and of landforms
  # -sum p_i log(b) p_i
  dplyr::mutate(landcover_diversity = vegan::diversity(cbind(lcms_landcover_01_prop, lcms_landcover_03_prop, lcms_landcover_04_prop, lcms_landcover_05_prop, lcms_landcover_07_prop, lcms_landcover_08_prop, lcms_landcover_09_prop, lcms_landcover_10_prop, lcms_landcover_11_prop, lcms_landcover_12_prop)),
                landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11_prop, csp_ergo_landforms_12_prop, csp_ergo_landforms_13_prop, csp_ergo_landforms_14_prop, csp_ergo_landforms_15_prop, csp_ergo_landforms_21_prop, csp_ergo_landforms_22_prop, csp_ergo_landforms_23_prop, csp_ergo_landforms_24_prop, csp_ergo_landforms_31_prop, csp_ergo_landforms_32_prop, csp_ergo_landforms_33_prop, csp_ergo_landforms_34_prop, csp_ergo_landforms_41_prop, csp_ergo_landforms_42_prop)),
                change_diversity = vegan::diversity(cbind(lcms_change_01_prop, lcms_change_02_prop, lcms_change_03_prop, lcms_change_04_prop)))


# Keeping all categories of landcover, change, and landforms in the dataset:
fires_working <- 
  fires %>% 
  dplyr::rename(peak_ridge_warm_prop = csp_ergo_landforms_11_prop,
                peak_ridge_prop = csp_ergo_landforms_12_prop,
                peak_ridge_cool_prop = csp_ergo_landforms_13_prop,
                mountain_divide_prop = csp_ergo_landforms_14_prop,
                cliff_prop = csp_ergo_landforms_15_prop,
                upper_slope_warm_prop = csp_ergo_landforms_21_prop,
                upper_slope_prop = csp_ergo_landforms_22_prop,
                upper_slope_cool_prop = csp_ergo_landforms_23_prop,
                upper_slope_flat_prop = csp_ergo_landforms_24_prop,
                lower_slope_warm_prop = csp_ergo_landforms_31_prop,
                lower_slope_prop = csp_ergo_landforms_32_prop,
                lower_slope_cool_prop = csp_ergo_landforms_33_prop,
                lower_slope_flat_prop = csp_ergo_landforms_34_prop,
                valley_prop = csp_ergo_landforms_41_prop,
                valley_narrow_prop = csp_ergo_landforms_42_prop) %>% 
  dplyr::rename(trees_prop = lcms_landcover_01_prop,
                shrubs_trees_mix_prop = lcms_landcover_03_prop,
                grass_forbs_herb_trees_mix_prop = lcms_landcover_04_prop,
                barren_trees_mix_prop = lcms_landcover_05_prop,
                shrubs_prop = lcms_landcover_07_prop,
                grass_forb_herb_shrub_mix_prop = lcms_landcover_08_prop,
                barren_shrub_mix_prop = lcms_landcover_09_prop,
                grass_forb_herb_prop = lcms_landcover_10_prop,
                barren_grass_forb_herb_mix_prop = lcms_landcover_11_prop,
                barren_prop = lcms_landcover_12_prop) %>% 
  dplyr::rename(fuel_stable_prop = lcms_change_01_prop,
                fuel_slow_loss_prop = lcms_change_02_prop,
                fuel_fast_loss_prop = lcms_change_03_prop,
                fuel_gain_prop = lcms_change_04_prop) %>% 
  dplyr::mutate(megafire = ifelse(megafire == "megafire", 1, 0)) %>% 
  as.data.frame()

human_drivers <- c("npl", "concurrent_fires", "cumu_count", "cumu_area_ha", "friction", "friction_walking_only", "road_density_mpha")
topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm_prop", "peak_ridge_prop", "peak_ridge_cool_prop", "mountain_divide_prop", "cliff_prop", "upper_slope_warm_prop", "upper_slope_prop", "upper_slope_cool_prop", "upper_slope_flat_prop", "lower_slope_warm_prop", "lower_slope_prop", "lower_slope_cool_prop", "lower_slope_flat_prop", "valley_prop", "valley_narrow_prop", "landform_diversity")
weather_drivers <- c("wind_anisotropy", "max_wind_speed_pct", "min_wind_speed_pct", 
                     "max_temp_pct", "min_temp_pct", 
                     "max_rh_pct", "min_rh_pct", "max_vpd_pct", "min_vpd_pct", "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z", "fm100_pct", "fm1000_pct")
fuel_drivers <- c("ndvi", "fuel_stable_prop", "fuel_slow_loss_prop", "fuel_fast_loss_prop", "fuel_gain_prop", "forest_structure_rumple", "trees_prop", "shrubs_trees_mix_prop", "grass_forbs_herb_trees_mix_prop", "barren_trees_mix_prop", "shrubs_prop", "grass_forb_herb_shrub_mix_prop", "barren_shrub_mix_prop", "grass_forb_herb_prop", "barren_grass_forb_herb_mix_prop", "barren_prop", "landcover_diversity", "change_diversity")
interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers)

other_names <- c("did", "id", "megafire", "ig_year", "ig_month", "biome_name", "eco_name", "event_day", "aoi_ha", "c_area_ha", "aoir", "aoir_mod", "pred_aoi_l", "x_3310", "y_3310")

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
  "npl", "road_density_mpha",
  "rumple_index", "elevation",
  "max_wind_speed_pct", "min_wind_speed_pct",
  "max_vpd_pct", "min_vpd_pct",
  "spei1y", "fm100_pct",
  "ndvi", "forest_structure_rumple", "change_diversity", "fuel_slow_loss_prop", "fuel_fast_loss_prop",
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


data <- fires_working
xy <- data[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))


temperate_conifer_forests <- data[data$biome_name == "Temperate Conifer Forests", ]
xy_temperate_conifer_forests <- temperate_conifer_forests[, c("x_3310", "y_3310")] %>% setNames(c("x", "y"))

distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)

temperate_conifer_forests_sf <-
  temperate_conifer_forests %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

temperate_conifer_forests_dist_mat <- 
  sf::st_distance(x = temperate_conifer_forests_sf, 
                  y = temperate_conifer_forests_sf) %>% 
  units::drop_units()

random_seed <- 1418

(start_time <- Sys.time())
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
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf_importance(model = tcf_nonspatial)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 33 minutes

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


(start_time <- Sys.time())
tcf_spatial <- spatialRF::rf_spatial(
  model = tcf_nonspatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random_seed
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 2027.889 mins
# Time difference of 33.79947 hours
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

readr::write_rds(x = tcf_spatial, file = "data/out/rf/rf-spatial_aoir-mod_pred-set-01_temperate-conifer-forest.rds")
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

tcf_spatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_spatial,
    quantiles = 0.5,
    ncol = 5
  )

ggsave(filename = "data/out/rf/rf-spatial-response-curves-accentuate_aoir-mod_pred-set-01_temperate-conifer-forest.png", plot = tcf_spatial_response_curves_accentuate_gg)

spatialRF::plot_moran(
  tcf_spatial, 
  verbose = FALSE
)

p1 <- spatialRF::plot_importance(
  tcf_nonspatial, 
  verbose = FALSE) + 
  ggplot2::ggtitle("Non-spatial model") 

p2 <- spatialRF::plot_importance(
  tcf_spatial,
  verbose = FALSE) + 
  ggplot2::ggtitle("Spatial model")

p1 | p2 

(start_time <- Sys.time())
tcf_spatial <- spatialRF::rf_evaluate(
  model = tcf_spatial,
  xy = xy_temperate_conifer_forests, #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  seed = random_seed,
  verbose = FALSE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 2.884896 mins

spatialRF::plot_evaluation(tcf_spatial)

(start_time <- Sys.time())
comparison <- spatialRF::rf_compare(
  models = list(
    `Non-spatial` = tcf_nonspatial,
    `Spatial` = tcf_spatial
  ),
  xy = xy_temperate_conifer_forests,
  repetitions = 30,
  training.fraction = 0.8,
  metrics = "r.squared",
  seed = random_seed
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

x <- comparison$comparison.df %>% 
  dplyr::group_by(model, metric) %>% 
  dplyr::summarise(value = round(median(value), 3)) %>% 
  dplyr::arrange(metric) %>% 
  as.data.frame()

colnames(x) <- c("Model", "Metric", "Median")

kableExtra::kbl(
  x,
  format = "html"
) %>%
  kableExtra::kable_paper("hover", full_width = F)

tcf_local_importance <- spatialRF::get_importance_local(tcf_spatial)
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
  geom_sf(data = tcf_local_importance, aes(color = valley_narrow_prop)) +
  scale_color_gradient2(low = color.low, high = color.high) +
  theme_bw() +
  labs(color = "Valley (narrow) proportion\nimportance")
