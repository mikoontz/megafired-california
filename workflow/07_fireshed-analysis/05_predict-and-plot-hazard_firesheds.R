# Get the models that were fit on the whole dataset for each biome
latest_ard_date <- sort(list.files(path = here::here("data", "ard"), pattern = "[0-9]"), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", latest_ard_date)
rf_tuning_dir <- here::here("data", "out", "rf", "tuning", latest_ard_date)

rf_fitted_dir <- here::here("data", "out", "rf", "fitted", latest_ard_date)

ard = readr::read_csv(file = here::here(latest_ard_dir, "fireshed-drivers-of-california-megafires.csv"))

firesheds_conus <- sf::st_read(here::here("data", "out", "firesheds_conus.gpkg"))
ca <- USAboundaries::us_states(states = "California", resolution = "high")
ca_or_nv_id_az <- USAboundaries::us_states(
  states = c("California", "Nevada", "Oregon", "Idaho", "Arizona"), 
  resolution = "high"
) |> 
  sf::st_geometry()

conus <- USAboundaries::us_states(
  states = NULL, 
  resolution = "high"
) |> 
  dplyr::filter(!(state_name %in% c("Alaska", "Hawaii", "Guam", "Puerto Rico", "Virgin Islands", "American Samoa", "Northern Mariana Islands"))) |> 
  sf::st_transform(5070) |> 
  dplyr::mutate(is_california = state_name == "California")

firesheds_ca <-
  firesheds_conus |>
  sf::st_make_valid() |>
  sf::st_filter(ca) |>
  dplyr::mutate(date = lubridate::ymd("2020-08-01"),
                samp_id = 0,
                did = glue::glue("{id}-{date}")) |>
  dplyr::select(did, id, date, samp_id, exposure)

ca_cities = USAboundaries::us_cities(states = "California")

ca_big_cities = ca_cities |> 
  dplyr::filter(population >= 250000)

ca_big_cities

###
conus_inset_tm =
  tmap::tm_shape(conus) +
  tmap::tm_borders() +
  tmap::tm_fill(col = "is_california", palette = c("white", "red"), legend.show = FALSE)

conus_inset_tm

plot_hazard = function(
    model_path, 
    ard_path = here::here(latest_ard_dir, "fireshed-drivers-of-california-megafires.csv"), 
    biome, 
    title
) {
  ard = readr::read_csv(file = ard_path)
  
  fitted_model <- readr::read_rds(model_path)
  
  ard_biome = ard |> 
    dplyr::filter(biome_shortname == {biome})
  
  ard_biome$ewe <- predict(fitted_model, data = ard_biome)$predictions[, 1]
  
  out_biome = ard_biome |>
    dplyr::left_join(firesheds_ca) |> 
    sf::st_as_sf()
  
  out_tm <-
    tmap::tm_shape(ca_or_nv_id_az, bbox = sf::st_bbox(sf::st_buffer(ca, 50000))) +
    tmap::tm_borders() +
    tmap::tm_shape(out_biome) +
    tmap::tm_fill(
      col = "ewe", 
      palette = viridis::inferno(100), 
      style = "cont",
      title = "Relative hazard"
    ) +
    tmap::tm_layout(frame = FALSE, legend.position = c(0, 0.2), title = {title}) +
    tmap::tm_compass(type = "rose", position = c(0, 0.05)) +
    tmap::tm_scale_bar(position = c(0, 0))
  
  print(out_tm)
  print(conus_inset_tm, vp = grid::viewport(x = 0.75, y = 0.8, width = 0.5, height = 0.3))
  
  return(out_tm)
}

# ### April 28, 2023 model
out_tcf_all = plot_hazard(
  model_path = here::here("data/out/rf/fitted/2023-04-28/rf_ranger_fitted-model_rtma_tcf.rds"), 
  biome = "tcf", 
  title = "Temperate Conifer Forest"
)

out_mfws_all = plot_hazard(
  model_path = here::here("data/out/rf/fitted/2023-04-28/rf_ranger_fitted-model_rtma_mfws.rds"), 
  biome = "mfws", 
  title = "Mediterranean Forest Woodland and Scrub"
)


### June 16, 2023 model that includes early/late
out_tcf_early_late = plot_hazard(
  model_path = here::here(rf_fitted_dir, paste0("rf_ranger_fitted-model_rtma_tcf.rds")), 
  biome = "tcf", 
  title = "Temperate Conifer Forest (model includes early/late feature)"
)

out_mfws_early_late = plot_hazard(
  model_path = here::here(rf_fitted_dir, paste0("rf_ranger_fitted-model_rtma_mfws.rds")), 
  biome = "mfws", 
  title = "Mediterranean Forest, Woodland, and Scrub (model includes early/late feature)"
)


### TCF
out_tcf_early <- plot_hazard(
  model_path = here::here("data/out/rf/fitted/early/2023-06-21/rf_ranger_fitted-model_rtma_tcf_early.rds"), 
  biome = "tcf", 
  title = "Temperate Conifer Forest -- Early"
)

out_tcf_late <- plot_hazard(
  model_path = here::here("data/out/rf/fitted/late/2023-06-21/rf_ranger_fitted-model_rtma_tcf_late.rds"), 
  biome = "tcf", 
  title = "Temperate Conifer Forest -- Late"
)

out_mfws_early <- plot_hazard(
  model_path = here::here("data/out/rf/fitted/early/2023-06-21/rf_ranger_fitted-model_rtma_mfws_early.rds"), 
  biome = "mfws", 
  title = "Mediterranean Forest, Woodland, and Scrub -- Early"
)

out_tcf_late <- plot_hazard(
  model_path = here::here("data/out/rf/fitted/late/2023-06-21/rf_ranger_fitted-model_rtma_mfws_late.rds"), 
  biome = "mfws", 
  title = "Mediterranean Forest, Woodland, and Scrub -- Late"
)


# tmap::tmap_save(tm = out_tcf_tm, insets_tm = conus_inset_tm, insets_vp = grid::viewport(x = 0.75, y = 0.8, width = 0.5, height = 0.3))