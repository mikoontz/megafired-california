library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
library(cpi)
library(rsample)
library(dplyr)
library(data.table)
# remotes::install_github("kormama1/seqknockoff")
library(seqknockoff)

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# Tuned hyperparameters
# Look at the coarse tuning results and see how we can fine tune the hyperparameters
fine_tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  data.table::fread(input = paste0("data/out/rf/rf_ranger_spatial-cv-fine-tuning-metrics_", biome_shortname, ".csv"))
})

fine_tuning_metrics <- data.table::rbindlist(fine_tuning_metrics_l)

tuned_hyperparameters <-
  fine_tuning_metrics %>% 
  filter(.metric == "f_meas") %>% 
  group_by(biome) %>% 
  arrange(desc(lwr)) %>% 
  slice(1)

# Get data
analysis_ready_nonspatial_version <- "v1"
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

counter = 1
biome_shortname <- biome_shortnames[counter]

biome_tuned_hyperparameters <- tuned_hyperparameters[tuned_hyperparameters$biome == biome_shortname, ]

fires <- as.data.frame(fires_all)
fires <- fires[fires$biome_shortname == biome_shortname, ]

# version 17 uses LCMS change and landcover data from 1 year prior to fire and systematic tuning

human_drivers <- c("npl", "concurrent_fires", "friction_walking_only", "road_density_mpha")
topography_drivers <- c("elevation", "rumple_index", "peak_ridge_warm", "peak_ridge", "peak_ridge_cool", "mountain_divide", "cliff", "upper_slope_warm", "upper_slope", "upper_slope_cool", "lower_slope_warm", "lower_slope", "lower_slope_cool", "valley", "valley_narrow", "landform_diversity")
weather_drivers <- c("wind_anisotropy", "max_wind_speed_pct", "min_wind_speed_pct",
                     "max_temp_pct", "min_temp_pct", "bi_pct", "erc_pct",
                     "max_rh_pct", "min_rh_pct", "max_vpd_pct", "min_vpd_pct", "max_soil_water_pct", "min_soil_water_pct",
                     "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "fm100_pct", "fm1000_pct")

fuel_lcms_change_tm01 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm01")
fuel_lcms_change_tm02 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm02")
fuel_lcms_change_tm03 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm03")
fuel_lcms_change_tm04 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm04")
fuel_lcms_change_tm05 <- paste0(c("fuel_slow_loss", "fuel_fast_loss", "fuel_gain", "fuel_stable", "change_diversity"), "_tm05")

fuel_lcms_landcover_tm01 <- paste0(c("trees", "shrubs_trees_mix", "grass_forbs_herb_trees_mix", "barren_trees_mix", "shrubs", "grass_forb_herb_shrub_mix", "barren_shrub_mix", "grass_forb_herb", "barren_grass_forb_herb_mix", "barren", "landcover_diversity"), "_tm01")

fuel_drivers <- c("ndvi", "veg_structure_rumple", fuel_lcms_change_tm01, fuel_lcms_change_tm02, fuel_lcms_change_tm03, fuel_lcms_change_tm04, fuel_lcms_change_tm05, fuel_lcms_landcover_tm01)

interacting_drivers <- c("wind_terrain_anisotropy", "wind_terrain_alignment")

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

fires <-
  fires %>%
  dplyr::select(-all_of(zero_variance_columns))

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]

data <- fires[, c("did", "ewe", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
data$npl <- factor(data$npl, levels = 1:5)
data$concurrent_fires <- as.numeric(data$concurrent_fires)

rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))

# mlr3 approach

class.wgts <- 1 / table(data$ewe)

# https://github.com/mlr-org/mlr3/issues/563
data$wt <- as.numeric(class.wgts[data$ewe + 1])

# Define the learner to be a {ranger} regression and give it the tuned hyperparameters
learner_ewe <- mlr3::lrn("regr.ranger", 
                         mtry = biome_tuned_hyperparameters$mtry,
                         num.trees = biome_tuned_hyperparameters$num.trees,
                         sample.fraction = biome_tuned_hyperparameters$sample.fraction,
                         replace = FALSE,
                         splitrule = "maxstat",
                         alpha = biome_tuned_hyperparameters$alpha,
                         minprop = biome_tuned_hyperparameters$minprop,
                         min.node.size = biome_tuned_hyperparameters$min.node.size,
                         num.threads = 11)

task_ewe <- mlr3::as_task_regr(data[, c("ewe", "wt", "eco_name_daily", predictor.variable.names)], 
                               target = "ewe", 
                               id = "ewe")

# weight column is the "wt" column, to help with class imbalance
task_ewe$set_col_roles(cols = "wt", roles = "weight")
# eco_name_daily becomes a grouping column for the spatial cross validation
task_ewe$set_col_roles(cols = "eco_name_daily", roles = "group")

# Next we build a resampler (we want to continue doing spatial cross validation to check on our variable importance)
resampler_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = 5)
resampler_ewe$instantiate(task = task_ewe)

cl <- parallel::makeCluster(parallel::detectCores() - 1)  
doParallel::registerDoParallel(cl)  

fitted_mod <- resample(task = task_ewe, learner = learner_ewe, resampling = resampler_ewe, store_models = TRUE)

parallel::stopCluster(cl = cl)

# Full model resampling
pred <- lapply(seq_along(fitted_mod$learners), function(i) {
  fitted_mod$learners[[i]]$predict(task_ewe, row_ids = resampler_ewe$test_set(i))
})

truth <- do.call(c, lapply(pred, function(x) x$truth))
response <- do.call(c, lapply(pred, function(x) x$response))
prob <- do.call(rbind, lapply(pred, function(x) x$prob))

fitted_mod$leaners
