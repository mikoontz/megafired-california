library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
library(cpi)
library(rsample)
library(dplyr)

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

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

counter <- 1

biome_shortname <- biome_shortnames[counter]

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

predictor.variable.names <- predictor.variable.names[!(predictor.variable.names %in% zero_variance_columns)]

fires <-
  fires %>%
  dplyr::select(-all_of(zero_variance_columns))

# BEGIN TUNING of the conditional random forest using spatial cross validation
# Build a cforest() model with the best spatially cross-validated AUC value

data <- fires[, c("did", "ewe", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
data$npl <- factor(data$npl, levels = 1:5)
# data$npl <- as.numeric(data$npl)
data$concurrent_fires <- as.numeric(data$concurrent_fires)

rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))

split_data <- rsample::initial_split(data = data)
analysis_data <- rsample::analysis(split_data)
assessment_data <- rsample::assessment(split_data)

# This is the core model that we will want to fit, which has been tuned
class.wgts <- 1 / table(analysis_data$ewe)

fitted_model <- ranger::ranger(formula = rf_formula,
                               data = analysis_data[, c("ewe", predictor.variable.names)],
                               mtry = 36,
                               num.trees = 500,
                               sample.fraction = 0.75,
                               replace = FALSE,
                               splitrule = "maxstat",
                               alpha = 0.9,
                               minprop = 0,
                               min.node.size = 3,
                               case.weights = as.numeric(class.wgts[analysis_data$ewe + 1]),
                               seed = 1)

model_check_ranger <- 
  data.frame(o = assessment_data$ewe, 
             p = predict(object = fitted_model, 
                         data = assessment_data, 
                         type = "response")$predictions) %>% 
  mutate(o_fac = factor(o, levels = 0:1),
         p_fac = factor(ifelse(p > 0.5, yes = 1, no = 0), levels = 0:1))

sum(as.numeric(as.character(model_check_ranger$p_fac)))
sum(model_check_ranger$o)

yardstick::f_meas(model_check_ranger, truth = o_fac, estimate = p_fac)
yardstick::precision(model_check_ranger, truth = o_fac, estimate = p_fac)
yardstick::recall(model_check_ranger, truth = o_fac, estimate = p_fac)
yardstick::specificity(model_check_ranger, truth = o_fac, estimate = p_fac)
yardstick::accuracy(model_check_ranger, truth = o_fac, estimate = p_fac)

# mlr3 approach
data <- fires[, c("did", "ewe", "eco_name_daily", "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names)]
data$npl <- factor(data$npl, levels = 1:5)
# data$npl <- as.numeric(data$npl)
data$concurrent_fires <- as.numeric(data$concurrent_fires)

rf_formula <- as.formula(paste0("ewe ~ ", paste(predictor.variable.names, collapse = " + ")))

split_data <- rsample::initial_split(data = data)
analysis_data <- rsample::analysis(split_data)
assessment_data <- rsample::assessment(split_data)

# This is the core model that we will want to fit, which has been tuned
class.wgts <- 1 / table(analysis_data$ewe)

# https://github.com/mlr-org/mlr3/issues/563
analysis_data$wt <- as.numeric(class.wgts[analysis_data$ewe + 1])

task_ewe <- mlr3::as_task_regr(analysis_data[, c("ewe", "wt", "eco_name_daily", predictor.variable.names)], 
                               target = "ewe", 
                               id = "ewe")

task_ewe$set_col_roles(cols = "wt", roles = "weight")
task_ewe$set_col_roles(cols = "eco_name_daily", roles = "group")

learner_ewe <- mlr3::lrn("regr.ranger")

learner_ewe$param_set$values$mtry <- 36
learner_ewe$param_set$values$num.trees <- 500
learner_ewe$param_set$values$sample.fraction <- 0.75
learner_ewe$param_set$values$replace <- FALSE
learner_ewe$param_set$values$splitrule <- "maxstat"
learner_ewe$param_set$values$alpha <- 0.9
learner_ewe$param_set$values$minprop <- 0
learner_ewe$param_set$values$min.node.size <- 3
learner_ewe$param_set$values$seed <- 1

learner_ewe$train(task = task_ewe)

resampler_ewe <- mlr3::rsmp(.key = "repeated_cv", folds = length(unique(data$eco_name_daily)), repeats = 5)
resampler_ewe$instantiate(task_ewe)

spat_cv_ewe <- 
  mlr3::resample(
  task = task_ewe, learner = learner_ewe,
  resampling = resampler_ewe)
spat_cv_ewe

spat_cv_ewe$aggregate(measures = msr("regr.rmse"))
spat_cv_ewe$score()

model_check_mlr3$truth
model_check_mlr3$response
yardstick::f_meas_vec(truth = factor(model_check_mlr3$truth, levels = 0:1), estimate = factor(ifelse(model_check_mlr3$response > 0.5, yes = 1, no = 0), levels = 0:1))

# remotes::install_github("kormama1/seqknockoff")
# library(seqknockoff)
out <- cpi::cpi(task = task_ewe, 
                learner = learner_ewe, 
                knockoff_fun = seqknockoff::knockoffs_seq, test_data = assessment_data[, c("ewe", predictor.variable.names)],
                measure = "classif.ce", 
                resampling = resampler_ewe,
                test = "fisher",
                B = 10)

out %>% 
  filter(p.value <= 0.05) %>% 
  arrange(desc(CPI))
