# Combine all drivers info so they are analysis ready
source("./R/utils.R")

# DEPLOY
set.seed(534)
driver_descriptions <- readr::read_csv("data/out/drivers/driver-descriptions.csv")
tail(driver_descriptions)

predictor.variable.names <- driver_descriptions$variable

fires_drivers_all_biomes <- collate_ard(
  fluc_static_fname = "data/out/drivers/fluc-static-driver-proportion-percentiles.csv", 
  landfire_fname = "data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv",
  wx_fname = "data/out/drivers/weather-drivers-as-percentiles.csv",
  fired_events_fname = "data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020.gpkg",
  other_fires_summary_fname = "data/out/drivers/other-fires-summary.csv",
  fired_behavior_metrics_fname = "data/out/fired/05_daily-with-behavior-metrics/fired_daily_ca_behavior-metrics.csv"
)

# Split by biome
fires_drivers_list <- fires_drivers_all_biomes |> 
  dplyr::filter(biome_shortname %in% c("mfws", "tcf")) |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::group_split()

ard <- purrr::map(
  .x = fires_drivers_list,
  .f = create_ard_by_biome,
  predictor.variable.names = predictor.variable.names
) |> 
  data.table::rbindlist()

# Pull out a validation dataset
validation <- ard |> 
  dplyr::group_by(biome_shortname, spatial_fold) |> 
  dplyr::slice_sample(prop = 0.1) |> 
  dplyr::mutate(validation = "validation") |>
  dplyr::ungroup() |> 
  dplyr::select(did, validation)

ard_with_validation <- dplyr::left_join(ard, validation) |> 
  dplyr::mutate(validation = !is.na(validation))


local_out_dir <- here::here("data/ard")
gdrive_out_dir <- "G:/My Drive/_projects/moore-foundation/megafired-california/data/ard"

ard_basename <- glue::glue("analysis-ready-data_{Sys.Date()}.csv")

data.table::fwrite(
  x = ard_with_validation, 
  file = glue::glue("{local_out_dir}/{ard_basename}")
)

file.copy(
  from = glue::glue("{local_out_dir}/{ard_basename}"),
  to = glue::glue("{gdrive_out_dir}/{ard_basename}"), 
  overwrite = TRUE
)

