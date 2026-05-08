# install from patched version which allows custom resamplers
# See https://github.com/bips-hb/cpi/pull/22
# unloadNamespace("cpi")
# remotes::install_github(repo = "mikoontz/cpi@resampling-fix") 
source("./R/utils.R")

library(mlr3verse)
lgr::get_logger("mlr3")$set_threshold("warn")
# lgr::get_logger("mlr3")$set_threshold("info") # reset to default

tune_varselect_version <- "v0.1.a"
overwrite_model_skill <- FALSE
overwrite_cpi_results <- FALSE

latest_ard_fname <- sort(
  list.files(
    path = here::here("data", "ard"), 
    pattern = "[0-9]",
    full.names = TRUE), 
  decreasing = TRUE
) |> 
  getElement(1)

latest_ard_date <- stringr::str_sub(latest_ard_fname, start = -14, end = -5)

local_out_dir <- here::here("data", "out", "rf", "tuning", latest_ard_date)
dir.create(local_out_dir, recursive = TRUE, showWarnings = FALSE)

ard <- readr::read_csv(
  latest_ard_fname, 
  col_types = list(spatial_fold = "factor")
)

# group by biome
ard_nested <- ard |> 
  dplyr::filter(!validation) |> 
  # Set up an ewe of 1 to be the "positive" class
  dplyr::mutate(ewe = factor(ewe, levels = c("1", "0"))) |> 
  tidyr::nest(.by = "biome_shortname", .key = "ard")

# Set up the pieces of the random forest model input
# The features (i.e., the predictors)
driver_descriptions <- readr::read_csv(
  "data/out/drivers/driver-descriptions.csv"
)

features <- driver_descriptions$variable
# Drop the sqrt_aoi_tm1 feature for this round
# features <- features[features != "sqrt_aoi_tm1"]

# The target (i.e., response variable)
target <- "ewe"

hyperparameters <- tidyr::expand_grid(
  mtry = 3:11,
  num.trees = 1000,
  sample.fraction = c(0.1, 0.3, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
  min.node.size = c(1, 5, 10, 25, 50, 60),
  use_class_weights = TRUE,
  resampling_approach = "resampler",
  tune_varselect_version = tune_varselect_version,
  overwrite_model_skill = overwrite_model_skill,
  overwrite_cpi_results = overwrite_cpi_results,
  target = target,
  features = list(features),
  ard_nested
) |>
  dplyr::arrange(biome_shortname, min.node.size, sample.fraction, mtry)

(start_time <- Sys.time())
future::plan(strategy = future.callr::callr, workers = 8)
out_all <- furrr::future_pmap(
  .l = hyperparameters,
  .f = tune_validate_varselect_assess,
  .progress = TRUE,
  .options = furrr::furrr_options(
    seed = NULL,
    packages = c(
      "mlr3verse"
    ))
)
(end_time <- Sys.time())
(difftime(end_time, start_time))