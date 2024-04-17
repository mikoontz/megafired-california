firesheds <- sf::st_read(here::here("data", "out", "firesheds_conus.gpkg"))
ca <- USAboundaries::us_states(states = "California", resolution = "high")

firesheds_ca <-
  firesheds |> 
  sf::st_make_valid() |> 
  sf::st_filter(ca) |> 
  dplyr::mutate(date = lubridate::ymd("2020-08-01"),
                samp_id = 0,
                did = glue::glue("{id}-{date}")) |> 
  dplyr::select(did, id, date, samp_id) |> 
  sf::st_transform(3310) |> 
  sf::st_set_agr(value = "constant")

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") |>  
  sf::st_transform(3310) |> 
  dplyr::select(BIOME_NAME, ECO_NAME) |> 
  dplyr::rename_all(.funs = tolower) |>  
  sf::st_set_agr(value = "constant")

firesheds_resolve_geo <- 
  firesheds_ca %>% 
  sf::st_join(y = resolve, largest = TRUE)

firesheds_resolve_out <- 
  firesheds_resolve_geo |>  
  sf::st_drop_geometry() |>  
  dplyr::select(did, id, date, samp_id, biome_name, eco_name)

write.csv(
  x = firesheds_resolve_out, 
  file = "data/out/firesheds_resolve-biomes.csv", 
  row.names = FALSE
  )
