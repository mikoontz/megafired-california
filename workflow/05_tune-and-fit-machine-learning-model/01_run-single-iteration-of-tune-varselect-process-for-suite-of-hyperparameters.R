# install from patched version which allows custom resamplers
# See https://github.com/bips-hb/cpi/pull/22
# unloadNamespace("cpi")
# remotes::install_github(repo = "mikoontz/cpi@resampling-fix") 
source("./R/utils.R")

library(mlr3verse)
lgr::get_logger("mlr3")$set_threshold("warn")
# lgr::get_logger("mlr3")$set_threshold("info") # reset to default

tune_varselect_version <- "v0.1.7"
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


# hyperparameter_grid_assessment <- out |> 
#   data.table::rbindlist() |> 
#   dplyr::select(mtry, min.node.size, sample.fraction, mcc_mean_important_variables) |> 
#   dplyr::mutate(skill = mcc_mean_important_variables)
# 
# # Assumes hyperparameter_grid_assessment has columns:
# #   mtry, sample.fraction, min.node.size, skill
# 
# # Convert the discrete hyperparameters to factors for clean faceting
# plot_df <- hyperparameter_grid_assessment |>
#   dplyr::mutate(
#     min.node.size   = factor(min.node.size),
#     mtry_fac        = factor(mtry)
#   )
# 
# ggplot2::ggplot(
#   plot_df,
#   ggplot2::aes(x = mtry_fac, y = sample.fraction, fill = skill)
# ) +
#   ggplot2::geom_tile(color = "grey80", linewidth = 0.3) +
#   ggplot2::facet_wrap(~ min.node.size, labeller = ggplot2::label_both) +
#   ggplot2::scale_fill_viridis_c(option = "magma", direction = 1) +
#   ggplot2::scale_y_continuous(breaks = sort(unique(hyperparameter_grid_assessment$sample.fraction))) +
#   ggplot2::labs(
#     x    = "mtry",
#     y    = "sample.fraction",
#     fill = "Skill",
#     title    = "Hyperparameter skill surface",
#     subtitle = "Each panel = one value of min.node.size"
#   ) +
#   ggplot2::theme_minimal(base_size = 11) +
#   ggplot2::theme(
#     panel.grid   = ggplot2::element_blank(),
#     axis.text.x  = ggplot2::element_text(size = 8)
#   )


# out <- out_all |> 
#   data.table::rbindlist()
# 
# table(out$n_important_variables)
# 
# data.table::fwrite(x = out_all, file = paste0(local_out_dir, "/rf_ranger_spatial-cv-tuning-and-variable-selection.csv"))

# 12-core machine with 64GB of RAM for 2023-04-28 version
# [1] "Starting the tcf biome at 2023-04-28 23:34:38"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=09h 13m 59s
# [1] "Starting the mfws biome at 2023-04-29 08:48:39"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07h 44m 37s
# [1] "Starting the dxs biome at 2023-04-29 16:33:18"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=40m 17s
# > (end_time <- Sys.time())
# [1] "2023-04-29 17:13:36 MDT"
# > (difftime(end_time, start_time, units = "hours"))
# Time difference of 17.64931 hours



# # 12-core machine with 64GB of RAM for 2023-06-16 version
# [1] "Starting the tcf biome at 2023-06-16 16:03:56"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=09h 04m 44s
# [1] "Starting the mfws biome at 2023-06-17 01:08:42"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=07h 42m 55s
# [1] "Starting the dxs biome at 2023-06-17 08:51:41"
# |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=39m 50s
# > (end_time <- Sys.time())
# [1] "2023-06-17 09:31:31 MDT"
# > (difftime(end_time, start_time, units = "hours"))
# Time difference of 17.45962 hours

### testing
# Use the validation data to be a fast test of logging
# ard_nested <- ard |> 
#   dplyr::filter(validation) |> 
#   # Set up an ewe of 1 to be the "positive" class
#   dplyr::mutate(ewe = factor(ewe, levels = c("1", "0"))) |> 
#   tidyr::nest(.by = "biome_shortname", .key = "ard")
# 
# hyperparameters <- tidyr::expand_grid(
#   mtry = 3:11,
#   num.trees = 1000,
#   sample.fraction = c(0.3, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
#   min.node.size = c(1, 5, 10, 25, 50, 60),
#   use_class_weights = TRUE,
#   resampling_approach = "resampler",
#   tune_varselect_version = tune_varselect_version,
#   overwrite_model_skill = overwrite_model_skill,
#   overwrite_cpi_results = overwrite_cpi_results,
#   ard_nested
# ) |>
#   dplyr::arrange(biome_shortname, min.node.size, sample.fraction, mtry)
# 
# idx <- 1
# mtry = hyperparameters$mtry[[idx]]
# sample.fraction = hyperparameters$sample.fraction[[idx]]
# min.node.size = hyperparameters$min.node.size[[idx]]
# num.trees = hyperparameters$num.trees[[idx]]
# use_class_weights = hyperparameters$use_class_weights[[idx]]
# resampling_approach = "resampler"
# tune_varselect_version = tune_varselect_version
# overwrite_model_skill = overwrite_model_skill
# overwrite_cpi_results = overwrite_cpi_results
# ard = hyperparameters$ard[[idx]]
# target = hyperparameters$target[[idx]]
# features = hyperparameters$features[[idx]]
# biome_shortname = hyperparameters$biome_shortname[[idx]]
# 
# 
# tictoc::tic()
# test_out = tune_validate_varselect_assess(
#   mtry = hyperparameters$mtry[[idx]],
#   sample.fraction = hyperparameters$sample.fraction[[idx]],
#   min.node.size = hyperparameters$min.node.size[[idx]],
#   num.trees = hyperparameters$num.trees[[idx]],
#   use_class_weights = hyperparameters$use_class_weights[[idx]],
#   resampling_approach = "resampler",
#   tune_varselect_version = tune_varselect_version,
#   overwrite_model_skill = overwrite_model_skill,
#   overwrite_cpi_results = overwrite_cpi_results,
#   ard = hyperparameters$ard[[idx]],
#   biome_shortname = hyperparameters$biome_shortname[[idx]]
# )
# tictoc::toc()

