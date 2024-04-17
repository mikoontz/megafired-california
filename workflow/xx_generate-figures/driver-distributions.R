library(ggplot2)
library(ggdist)

plot_density <- function(data, variable, title) {
  plot_data <- data |> 
    dplyr::filter(variable == !!variable) |> 
    dplyr::mutate(value = 100 * value)
  
  mean_val = plot_data |> 
    dplyr::summarize(mean_val = mean(value, na.rm = TRUE)) |> 
    dplyr::pull(mean_val)
  
  mean_val = round(mean_val, digits = 1)
  
  out = ggplot(
    data = plot_data, 
    aes(x = value)
  ) +
    geom_histogram(bins = 100) +
    geom_vline(xintercept = mean_val, col = "red") +
    # Themes and Labels
    theme_bw() +
    labs(x = "Percentile",
         y = "Count",
         size = NULL) +
    ggtitle(label = glue::glue("{title} ({mean_val} percentile)"))
  
  return(out)
}

driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

# 10,763 daily spread events between 2009 and 2020
fluc_static = data.table::fread(
  'data/out/drivers/fluc-static-driver-proportion-percentiles.csv'
) |> 
  dplyr::filter(date >= lubridate::ymd('2009-01-01')) |> 
  dplyr::select(
    tidyselect::all_of(c("did", "id", "date")) | 
      tidyselect::any_of(full_predictor_variable_names)
  ) |> 
  dplyr::mutate(date = as.Date(date)) |> 
  tidyr::pivot_longer(
    cols = tidyselect::any_of(full_predictor_variable_names), 
    names_to = "variable", 
    values_to = "value"
  ) |>
  dplyr::left_join(driver_descriptions)

lf = data.table::fread(
  'data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv'
) |> 
  dplyr::select(
    tidyselect::all_of(c("did", "id", "date")) | 
      tidyselect::any_of(full_predictor_variable_names)
  ) |> 
  dplyr::mutate(date = as.Date(date)) |> 
  tidyr::pivot_longer(
    cols = tidyselect::any_of(full_predictor_variable_names), 
    names_to = "variable", 
    values_to = "value"
  ) |>
  dplyr::left_join(driver_descriptions)

weather = data.table::fread(
  'data/out/drivers/weather-drivers-as-percentiles.csv'
) |> 
  dplyr::select(-samp_id) |> 
  dplyr::filter(date >= lubridate::ymd('2009-01-01')) |> 
  dplyr::filter(did %in% lf$did) |> 
  dplyr::select(
    tidyselect::all_of(c("did", "id", "date")) | 
      tidyselect::any_of(full_predictor_variable_names)
  ) |> 
  dplyr::mutate(date = as.Date(date)) |> 
  tidyr::pivot_longer(
    cols = tidyselect::any_of(full_predictor_variable_names), 
    names_to = "variable", 
    values_to = "value"
  ) |>
  dplyr::left_join(driver_descriptions)

drivers = rbind(fluc_static, lf, weather)

drivers = rbind(
  readr::read_csv("data/ard/early/2023-06-21/daily-drivers-of-california-megafires_dxs_early.csv"),
  readr::read_csv("data/ard/late/2023-06-21/daily-drivers-of-california-megafires_dxs_late.csv"),
  readr::read_csv("data/ard/early/2023-06-21/daily-drivers-of-california-megafires_mfws_early.csv"),
  readr::read_csv("data/ard/late/2023-06-21/daily-drivers-of-california-megafires_mfws_late.csv"),
  readr::read_csv("data/ard/early/2023-06-21/daily-drivers-of-california-megafires_tcf_early.csv"),
  readr::read_csv("data/ard/late/2023-06-21/daily-drivers-of-california-megafires_tcf_late.csv"),
  readr::read_csv("data/ard/early/2023-06-21/daily-drivers-of-california-megafires_tgss_early.csv"),
  readr::read_csv("data/ard/late/2023-06-21/daily-drivers-of-california-megafires_tgss_late.csv")
) |> 
  dplyr::select(
    tidyselect::all_of(c("did")) | 
      tidyselect::any_of(full_predictor_variable_names)
  ) |> 
  tidyr::pivot_longer(
    cols = tidyselect::any_of(full_predictor_variable_names), 
    names_to = "variable", 
    values_to = "value"
  ) |>
  dplyr::left_join(driver_descriptions)

full_predictor_variable_names

plot_density(data = drivers, variable = "bi_pct", title = "Burning Index")
plot_density(data = drivers, variable = "max_temp_era5_pct", title = "Maximum temperature")
plot_density(data = drivers, variable = "min_rh_era5_pct", title = "Minimum relative humidity")
plot_density(data = drivers, variable = "max_vpd_era5_pct", title = "Maximum vapor pressure deficit")
plot_density(data = drivers, variable = "max_wind_speed_era5_pct", title = "Maximum wind speed")

plot_density(data = drivers, variable = "barren_tm01", title = "Barren")
plot_density(data = drivers, variable = "ndvi", title = "NDVI")
plot_density(data = drivers, variable = "trees_tm01", title = "Tree cover")
plot_density(data = drivers, variable = "grass_forb_herb_tm01", title = "Grass/forb/herb cover")

plot_density(data = drivers, variable = "landcover_diversity_tm01", title = "Landcover diversity")
drivers |> 
  dplyr::group_by(variable, type) |> 
  dplyr::summarize(mean_val = mean(value, na.rm = TRUE)) |> 
  dplyr::arrange(type, mean_val) |> 
  print(n = 50)
  
  

