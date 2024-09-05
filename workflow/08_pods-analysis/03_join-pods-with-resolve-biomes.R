pods = sf::st_read(here::here("data", "out", "sierra-nevada-pods.gpkg")) |> 
  sf::st_make_valid() |>
  dplyr::select(did, id, date, samp_id) |> 
  sf::st_set_agr(value = "constant")

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") |>  
  sf::st_transform(3310) |> 
  dplyr::select(BIOME_NAME, ECO_NAME) |> 
  dplyr::rename_all(.funs = tolower) |>  
  sf::st_set_agr(value = "constant")

pods_resolve_geo <- 
  pods |>  
  sf::st_join(y = resolve, largest = TRUE)

pods_resolve_out <- 
  pods_resolve_geo |>  
  sf::st_drop_geometry() |>  
  dplyr::select(did, id, date, samp_id, biome_name, eco_name)

write.csv(
  x = pods_resolve_out, 
  file = "data/out/sierra-nevada-pods_resolve-biomes.csv", 
  row.names = FALSE
)
