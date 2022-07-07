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
system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/  data/out/", stdout = TRUE)  

# read in driver descriptions
system2(command = "aws", args = "s3 cp s3://california-megafires/tables/driver-descriptions.csv  tables/driver-descriptions.csv", stdout = TRUE)  

fired_daily_response <- 
  data.table::fread(input = "data/out/fired_daily_ca_response-vars.csv")

driver_descriptions <- read.csv(file = "tables/driver-descriptions.csv")

#### ---- Temperate Conifer Forests ---- ####
fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v3.csv"
fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v4.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -starts_with("raw"),
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha),
                area_log10_pct = ecdf(area_log10)(area_log10),
                ewe = ifelse(area_log10_pct >= 0.95, yes = 1, no = 0)) %>% 
  as.data.frame()

idx <- substr(names(fires), start = 1, stop = 4) == "adj_"
names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]

# ### Set 9
# human_drivers <- c("npl", "concurrent_fires")
# weather_drivers <- c("max_wind_speed_pct", "min_wind_speed_pct", "wind_anisotropy",
#                      "max_temp_pct", "min_temp_pct",
#                      "max_rh_pct", "min_rh_pct",
#                      "max_vpd_pct", "min_vpd_pct",
#                      "max_soil_water_pct", "min_soil_water_pct",
#                      "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z",
#                      "fm100_pct", "fm1000_pct")
# topography_drivers <- c("raw_rumple_index")
# fuel_drivers <- c("raw_ndvi", "raw_veg_structure_rumple")
# interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment", "bi_pct", "erc_pct")
# 
# predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

### Set 8
human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
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

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

### Set 7
# predictor.variable.names <- driver_descritions$driver

### Set 6
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
# 
# predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

# No NAs
apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])

fires <- fires[!(fires$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires[, predictor.variable.names]), 2, is.infinite))

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

cor.mat <- cor(fires[, predictor.variable.names])
sort(cor.mat[, colnames(cor.mat) == "sqrt_aoi_tm1"])

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

##### ---- SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires
# xy <- data[, c("x_biggest_poly_3310", "y_biggest_poly_3310")] %>% setNames(c("x", "y"))

# Feature engineering
# (start_time <- Sys.time())
# ftr_eng_01 <- spatialRF::the_feature_engineer(data = data, 
#                                               dependent.variable.name = "area_log10", 
#                                               predictor.variable.names = predictor.variable.names_reduced, 
#                                               xy = xy)
# (end_time <- Sys.time())
# (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Fitting and evaluating a model without interactions.
# Testing 15 candidate interactions.
# No promising interactions found. 
# 
# > (end_time <- Sys.time())
# [1] "2022-06-13 16:19:33 MDT"
# > (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 11.01216 mins
# 
# distance_thresholds <- c(0, 1000, 5000, 10000, 25000, 50000)
# 
# tcf_sf <-
#   data %>% 
#   sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), crs = 3310, remove = FALSE)
# 
# tcf_dist_mat <- 
#   sf::st_distance(x = tcf_sf, 
#                   y = tcf_sf) %>% 
#   units::drop_units()
# 
random_seed <- 1848
# 

data <-
  data %>% 
  dplyr::mutate(area_log10_pct = dplyr::percent_rank(area_log10) * 100)
  
(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_ha",
  predictor.variable.names = predictor.variable.names_reduced,
  # distance.matrix = tcf_dist_mat,
  distance.thresholds = distance_thresholds,
  # xy = xy, #not needed by rf, but other functions read it from the model
  seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "area_log10",
  predictor.variable.names = predictor.variable.names_reduced,
  # distance.matrix = tcf_dist_mat,
  # distance.thresholds = distance_thresholds,
  # xy = xy, #not needed by rf, but other functions read it from the model
  # seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "ewe",
  predictor.variable.names = predictor.variable.names_reduced,
  # distance.matrix = tcf_dist_mat,
  # distance.thresholds = distance_thresholds,
  # xy = xy, #not needed by rf, but other functions read it from the model
  # seed = random_seed,
  ranger.arguments = list(classification = TRUE),
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

(start_time <- Sys.time())
tcf_nonspatial <- spatialRF::rf(
  data = data,
  dependent.variable.name = "ewe",
  predictor.variable.names = predictor.variable.names_reduced,
  # distance.matrix = tcf_dist_mat,
  distance.thresholds = distance_thresholds,
  # xy = xy, #not needed by rf, but other functions read it from the model
  # seed = random_seed,
  verbose = TRUE
)
(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

unique(tcf_nonspatial$ranger.arguments$case.weights)

tcf_nonspatial_response_curves_accentuate_gg <-
  spatialRF::plot_response_curves(
    tcf_nonspatial,
    quantiles = c(0.5),
    ncol = 5
  )

tcf_nonspatial_response_curves_accentuate_gg

preds <- predict(object = tcf_nonspatial, data = data)
sum(preds$predictions)
data$preds = preds$predictions

which(data$preds - data$ewe != 0)
data[which(data$preds == 1), ]
# 
# readr::write_rds(x = tcf_nonspatial, file = "data/out/rf/tcf-nonspatial.rds")
# 
# tcf_nonspatial <- readr::read_rds(file = "data/out/rf/tcf-nonspatial.rds")
# 
# fire_size_classes <- tibble(class = LETTERS[1:7], 
#                             size_lwr_ac = c(0, 0.25, 10, 100, 300, 1000, 5000),
#                             size_upr_ac = c(0.25, 10, 100, 300, 1000, 5000, NA),
#                             size_lwr_ha = size_lwr_ac / 2.47105,
#                             size_upr_ha = size_upr_ac / 2.47105)

data_long <-
  data %>% 
  dplyr::select(-c(daily_area_tminus1_ha, cum_area_ha, cum_area_ha_tminus1, daily_perim_km, daily_perim_tminus1_km, active_fireline_km, samp_id, predicted_aoi_log_cumarea_tm1, predicted_aoi_cumarea_tm1, aoir_modeled_cumarea_tm1, aoir_cumarea_tm1, predicted_aoi_log_sqrtarea_tm1, predicted_aoi_sqrtarea_tm1, aoir_modeled_sqrtarea_tm1, aoir_sqrtarea_tm1, daily_area_ha)) %>% 
  dplyr::mutate(area_log10_pct = dplyr::percent_rank(area_log10)) %>%
 dplyr::mutate(area_log10_decile = floor(area_log10_pct * 10) + 0.5) %>% 
  dplyr::mutate(area_log10_decile = ifelse(area_log10_decile == 10.5, yes = 9.5, no = area_log10_decile)) %>%
  tidyr::pivot_longer(cols = -c(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310), names_to = "driver", values_to = "value") %>% 
  dplyr::select(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310, everything()) %>% 
  dplyr::left_join(driver_descriptions) %>% 
  dplyr::mutate(driver_desc_ggplot = ifelse(driver_type %in% c("fuel", "topography"), yes = driver_desc, no = driver))

effect_size_summaries <- 
  data_long %>% 
  dplyr::group_by(biome_name, driver, driver_desc, area_log10_decile, driver_desc_ggplot) %>% 
  summarize(mean_value = mean(value),
            lwr = t.test(value, conf.level = 0.95)$conf.int[1],
            upr = t.test(value, conf.level = 0.95)$conf.int[2], 
            n = n()) %>% 
  dplyr::ungroup()

ggplot(effect_size_summaries, aes(x = area_log10_decile, y = mean_value, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_ribbon(alpha = 0.25) +
  facet_wrap(facets = "driver_desc_ggplot", scales = "free_y")

# ggplot(data_long, aes(x = area_log10, y = value)) +
#   geom_point() +
#   facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
#   geom_smooth(color = "red")

ggplot(data_long, aes(x = area_log10, y = value)) +
  facet_wrap(facets = "driver_desc_ggplot", scales = "free_y") +
  geom_smooth(color = "red")






fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tcf_v3.csv"

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
                ewe = ifelse(area_ha > 800, yes = 1, no = 0)) %>% 
  as.data.frame()

idx <- substr(names(fires), start = 1, stop = 4) == "raw_"
names(fires)[idx] <- substr(names(fires), start = 5, stop = nchar(names(fires)))[idx]

### Set 8
human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
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

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

# No NAs
apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])

fires <- fires[!(fires$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires[, predictor.variable.names]), 2, is.infinite))

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

cor.mat <- cor(fires[, predictor.variable.names])
sort(cor.mat[, colnames(cor.mat) == "sqrt_aoi_tm1"])

predictor.variable.names_reduced <- spatialRF::auto_cor(
  x = fires[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


data <- fires

data_long <-
  data %>% 
  dplyr::select(-c(daily_area_tminus1_ha, cum_area_ha, cum_area_ha_tminus1, daily_perim_km, daily_perim_tminus1_km, active_fireline_km, samp_id, predicted_aoi_log_cumarea_tm1, predicted_aoi_cumarea_tm1, aoir_modeled_cumarea_tm1, aoir_cumarea_tm1, predicted_aoi_log_sqrtarea_tm1, predicted_aoi_sqrtarea_tm1, aoir_modeled_sqrtarea_tm1, aoir_sqrtarea_tm1, daily_area_ha)) %>% 
  dplyr::mutate(area_log10_pct = dplyr::percent_rank(area_log10)) %>%
  # dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10)) %>%
  dplyr::mutate(area_log10_decile = floor(area_log10_pct * 10) + 0.5) %>% 
  dplyr::mutate(area_log10_decile = ifelse(area_log10_decile == 10.5, yes = 9.5, no = area_log10_decile)) %>%
  tidyr::pivot_longer(cols = -c(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310), names_to = "driver", values_to = "value") %>% 
  dplyr::select(did, id, date, ig_year, area_ha, area_log10, area_log10_pct, area_log10_decile, biome_name, eco_name, biome_name_daily, eco_name_daily, x_biggest_poly_3310, y_biggest_poly_3310, everything()) %>% 
  dplyr::left_join(driver_descriptions) %>% 
  dplyr::mutate(driver_desc_ggplot = ifelse(driver_type %in% c("fuel", "topography"), yes = driver_desc, no = driver))

effect_size_summaries <- 
  data_long %>% 
  dplyr::group_by(biome_name, driver, driver_desc, area_log10_decile, driver_desc_ggplot) %>% 
  summarize(mean_value = mean(value),
            lwr = t.test(value, conf.level = 0.95)$conf.int[1],
            upr = t.test(value, conf.level = 0.95)$conf.int[2], 
            n = n()) %>% 
  dplyr::ungroup()

ggplot(effect_size_summaries, aes(x = area_log10_decile, y = mean_value, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_ribbon(alpha = 0.25) +
  facet_wrap(facets = "driver_desc_ggplot", scales = "free_y")






































#### ---- Mediterranean Forest, Woodland & Scrub ---- ####

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_mfws_v3.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))

fires <- 
  fires %>% 
  as.data.frame()

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

# No NAs
apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])

fires <- fires[!(fires$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires[, predictor.variable.names]), 2, is.infinite))

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
  x = fires[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )


# SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires
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

#### ---- Temperate Grasslands, Savanna, & Shrub ---- ####

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_tgss_v3.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))

fires <- 
  fires %>% 
  as.data.frame()

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

# No NAs
apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])

fires <- fires[!(fires$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires[, predictor.variable.names]), 2, is.infinite))

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
  x = fires[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

# SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires
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

#### ---- Desert and Xeric Shrublands ---- ####

fired_drivers_fname <- "data/out/analysis-ready/FIRED-daily-scale-drivers_california_dxs_v3.csv"

fires <-
  data.table::fread(fired_drivers_fname) %>%
  dplyr::select(-max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -cumu_count, -cumu_area_ha,
                -proj_area, -biggest_poly_area_ha, -x_3310, -y_3310, -biggest_poly_frac, -samp_id,
                -ends_with("rtma"), -ends_with("rtma_pct"),
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi")) %>% 
  dplyr::left_join(fired_daily_response) %>% 
  dplyr::mutate(sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha))

fires <- 
  fires %>% 
  as.data.frame()

predictor.variable.names <- c(human_drivers, topography_drivers, weather_drivers, fuel_drivers, interacting_drivers, "sqrt_aoi_tm1")

# No NAs
apply(fires[, predictor.variable.names], MARGIN = 2, FUN = function(x) return(sum(is.na(x))))

# Remove these columns that have more NA's than most so we opt to drop them instead of dropping the rows
na_cols <- colnames(fires[, predictor.variable.names])[which(apply(fires[, predictor.variable.names], 2, function(x) return(sum(is.na(x)))) > 50)]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% na_cols)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(na_cols))

# Now drop all fires that have an NA in any column
bad_fires <- unique(fires[!complete.cases(fires[, predictor.variable.names]), "id"])

fires <- fires[!(fires$id %in% bad_fires), ]

# no columnns with 0 variance (rounded to 4 decimal places)
zero_variance_columns <- colnames(fires[, predictor.variable.names])[round(apply(fires[, predictor.variable.names], MARGIN = 2, FUN = var), 4) == 0]

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]
fires <- 
  fires %>% 
  dplyr::select(-all_of(zero_variance_columns))

# No NaN or Inf when scaling
sum(apply(scale(fires[, predictor.variable.names]), 2, is.nan))
sum(apply(scale(fires[, predictor.variable.names]), 2, is.infinite))

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
  x = fires[, predictor.variable.names],
  cor.threshold = 0.75,
  preference.order = preference.order
) %>% 
  spatialRF::auto_vif(
    vif.threshold = 5,
    preference.order = preference.order
  )

# SET UP DATA SUBSETS WITH THEIR XY MATRICES
data <- fires
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

