library(sf)

fetch_ard_data <- function() {
  latest_early_date <- sort(list.files(path = here::here("data", "ard", "early")), 
                            decreasing = TRUE)[1]
  latest_early_ard_dir <- here::here("data", "ard", "early", latest_early_date)
  
  latest_late_date <- sort(list.files(path = here::here("data", "ard", "late")), 
                           decreasing = TRUE)[1]
  latest_late_ard_dir <- here::here("data", "ard", "late", latest_late_date)
  
  early_data_files <- list.files(latest_early_ard_dir, full.names = TRUE)
  late_data_files <- list.files(latest_late_ard_dir, full.names = TRUE)
  
  early_data <- 
    lapply(early_data_files, FUN = data.table::fread) |> 
    data.table::rbindlist() |>
    dplyr::mutate(fire_phase = "early")
  
  late_data <- 
    lapply(late_data_files, FUN = data.table::fread) |>
    data.table::rbindlist() |>
    dplyr::mutate(fire_phase = "late")
  
  out <- rbind(early_data, late_data)
  
  return(out)
}

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") |> 
  sf::st_transform(3310) |> 
  sf::st_geometry()

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") |> 
  sf::st_transform(3310) |>
  dplyr::select(BIOME_NAME, ECO_NAME) |> 
  dplyr::rename_all(.funs = tolower) |>  
  sf::st_set_agr(value = "constant") |>
  sf::st_collection_extract(type = "POLYGON") |>
  sf::st_intersection(ca)


dat <- 
  fetch_ard_data() |>
  sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), 
               remove = FALSE, 
               crs = 3310) |>
  dplyr::mutate(ewe = factor(ewe))

tmap::tm_shape(shp = resolve) +
  tmap::tm_fill(col = "biome_name", title = "Resolve Biome") +
  tmap::tm_shape(shp = dat) +
  tmap::tm_symbols(col = "ewe", 
                   palette = c("black", "red"), 
                   size = 0.25, 
                   title.col = "Spread type",
                   labels = c("Fire spread", "Explosive fire spread"))

summary(dat)
test <- list.files("data/ard/2023-06-16/", full.names = TRUE) |> lapply(FUN = read.csv) |> do.call("rbind", args = _)
nrow(test)

nrow(dat)


drivers <- read.csv("data/out/drivers/driver-descriptions.csv")
nrow(drivers)
