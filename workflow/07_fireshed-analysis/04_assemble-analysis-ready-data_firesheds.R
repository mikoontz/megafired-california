### Get fireshed data into the final, analysis-ready format with the correct columns, percentile ranks per biome, etc.

set.seed(1617)

latest_ard_date <- sort(list.files(path = here::here("data", "ard"), pattern = "[0-9]"), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", latest_ard_date)

biome_lookup = 
  tibble::tibble(
    biome_name = c("Temperate Conifer Forests", 
                   "Mediterranean Forests, Woodlands & Scrub", 
                   "Temperate Grasslands, Savannas & Shrublands", 
                   "Deserts & Xeric Shrublands"),
    biome_shortname = c("tcf", "mfws", "tgss", "dxs")
  )

driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

firesheds = readr::read_csv("data/out/firesheds_resolve-biomes.csv") |> 
  dplyr::mutate(date = lubridate::ymd(date)) |> 
  dplyr::left_join(biome_lookup) |> 
  dplyr::select(-samp_id)

calc_percentile_rank = function(vec) {
  return(data.table::frank(vec, ties.method = "random") / length(vec))
}

### Fluctuating and static drivers
fluc_static <- 
  data.table::fread("data/out/drivers/fireshed-fluc-static-driver-proportions.csv") |>
  dplyr::mutate(date = as.Date(date)) |> 
  dplyr::select(did, id, date, 
                elevation, rumple_index, caltrans_road_density_mpha,
                ndvi, veg_structure_rumple, 
                peak_ridge_cliff, valleys, slope_warm, slope_cool, slope_neutral, flat, 
                trees_tm01, shrubs_tm01, grass_forb_herb_tm01, barren_tm01,
                landform_diversity, landcover_diversity_tm01) |> 
  dplyr::left_join(firesheds) |> 
  dplyr::group_by(biome_name, biome_shortname) |> 
  # dplyr::mutate(
  #   dplyr::across(
  #     .cols = tidyselect::all_of(
  #       c("elevation", 
  #         "rumple_index", 
  #         "caltrans_road_density_mpha",
  #         "ndvi", 
  #         "veg_structure_rumple", 
  #         "peak_ridge_cliff", 
  #         "valleys", 
  #         "slope_warm", 
#         "slope_cool", 
#         "slope_neutral", 
#         "flat", 
#         "trees_tm01", 
#         "shrubs_tm01", 
#         "grass_forb_herb_tm01", 
#         "barren_tm01",
#         "landform_diversity", 
#         "landcover_diversity_tm01"
#       )
#     ),
#     .fns = \(x) data.table::frank(x, ties.method = "random"),
#     .names = "{.col}_rank"
#   )
# ) |> 
dplyr::mutate(
  dplyr::across(
    .cols = tidyselect::all_of(
      c("elevation", 
        "rumple_index", 
        "caltrans_road_density_mpha",
        "ndvi", 
        "veg_structure_rumple", 
        "peak_ridge_cliff", 
        "valleys", 
        "slope_warm", 
        "slope_cool", 
        "slope_neutral", 
        "flat", 
        "trees_tm01", 
        "shrubs_tm01", 
        "grass_forb_herb_tm01", 
        "barren_tm01",
        "landform_diversity", 
        "landcover_diversity_tm01"
      )
    ),
    .fns = calc_percentile_rank,
    .names = "{.col}"
  )
) |> 
  dplyr::ungroup()


### Landfire drivers
lf_drivers = data.table::fread(
  here::here(
    "data", 
    "out", 
    "drivers", 
    "landfire-disturbance", 
    "fireshed_daily_disturbance-drivers_v1.csv"
  )
) 

lf_drivers[, `:=`(date = as.Date(date),
                  samp_id = NULL,
                  fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
                  fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10)]

lf_drivers =
  lf_drivers |>
  dplyr::select(did, id, date,
                fire_high_tm01_tm05, fire_high_tm06_tm10,
                fire_not_high_tm01_tm05, fire_not_high_tm06_tm10,
                insect_disease_tm01_tm10) |> 
  dplyr::left_join(firesheds) |> 
  dplyr::group_by(biome_name, biome_shortname) |> 
  # dplyr::mutate(
  #   dplyr::across(
  #     .cols = tidyselect::all_of(
  #       c(
  #         "fire_high_tm01_tm05", 
  #         "fire_high_tm06_tm10", 
  #         "fire_not_high_tm01_tm05", 
  #         "fire_not_high_tm06_tm10", 
  #         "insect_disease_tm01_tm10"
  #       )
  #     ), 
#     .fns = \(x) data.table::frank(x, ties.method = "random"),
#     .names = "{.col}_rank"
#   )
# ) |> 
dplyr::mutate(
  dplyr::across(
    .cols = tidyselect::all_of(
      c(
        "fire_high_tm01_tm05", 
        "fire_high_tm06_tm10", 
        "fire_not_high_tm01_tm05", 
        "fire_not_high_tm06_tm10", 
        "insect_disease_tm01_tm10"
      )
    ), 
    .fns = calc_percentile_rank,
    .names = "{.col}"
  )
) |> 
  dplyr::ungroup()

### Weather drivers and other variables
weather_and_other_drivers <- 
  firesheds |> 
  dplyr::mutate(
    wind_dir_ns_era5 = sqrt(2)/2, # NE winds
    wind_dir_ew_era5 = sqrt(2)/2,
    wind_anisotropy_ns_era5 = 1, 
    wind_anisotropy_ew_era5 = 1,
    min_wind_speed_era5_pct = 0.75, 
    max_wind_speed_era5_pct = 0.975,
    min_rh_era5_pct = 0.025, 
    max_rh_era5_pct = 0.25, 
    min_temp_era5_pct = 0.75, 
    max_temp_era5_pct = 0.975, 
    min_vpd_era5_pct = 0.75, 
    max_vpd_era5_pct = 0.975,
    spei14d = -2, 
    spei30d = -2, 
    spei90d = -2, 
    spei180d = -2, 
    spei270d = -2, 
    spei1y = -2, 
    spei2y = -2, 
    spei5y = -2, 
    pdsi_z = -5,
    erc_pct = 0.975, 
    bi_pct = 0.975, 
    fm100_pct = 0.025, 
    fm1000_pct = 0.025,
    fireline_length_proxy_km = 3,
    short_concurrent_fires = 50,
    npl = 5,
    early_late = 0
  )

drivers <- fluc_static |> 
  dplyr::left_join(lf_drivers) |> 
  dplyr::left_join(weather_and_other_drivers)

readr::write_csv(x = drivers, file = here::here(latest_ard_dir, "fireshed-drivers-of-california-megafires.csv"))

