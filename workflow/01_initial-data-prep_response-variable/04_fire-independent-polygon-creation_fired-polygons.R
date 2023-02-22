library(sf)
library(dplyr)
library(stringr)
library(USAboundaries)
library(pbapply)

dir.create("data/out/fired/04_fire-independent-locations", recursive = TRUE, showWarnings = FALSE)

fired_daily_resolve <- read.csv("data/out/fired/03_joined-with-other-data/fired_daily_resolve.csv")

fired_daily <-
  sf::st_read("data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::mutate(samp_id = 0) %>% 
  dplyr::select(did, id, date, samp_id) %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::left_join(fired_daily_resolve) %>% 
  sf::st_set_agr(value = "constant")

# Expand the daily fire perimeters such that they are placed on top of 1000 random points (within each Resolve Biome)
ca <-   
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_geometry()

ca_or_nv_az <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Oregon", "Nevada", "Arizona")) %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_union() %>% 
  sf::st_geometry()

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  dplyr::select(BIOME_NAME, ECO_NAME) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_intersection(y = ca)

set_fire_independent_locations <- function(biome, short_name, buffer = 50000, seed = 1224, n_sets = 5, n_pts = 500, version) {
  
  # Subset the FIRED data to just the biome of interest
  fired_biome <-
    fired_daily %>% 
    dplyr::filter(biome_name == biome) %>% 
    dplyr::select(did, id, date) %>% 
    dplyr::mutate(centroid_x = sf::st_coordinates(sf::st_centroid(.))[, "X"],
                  centroid_y = sf::st_coordinates(sf::st_centroid(.))[, "Y"])
  
  # subset the Resolve geometries to just the biome of interest
  biome_geo <- 
    resolve %>% 
    dplyr::filter(biome_name == biome) %>% 
    sf::st_set_agr("constant")
  
  # Buffer the resolve geometry in from the coast, but not from the internal borders of California with other states
  biome_buff <- sf::st_intersection(x = biome_geo, y = sf::st_buffer(x = ca_or_nv_az, dist = -buffer))
  
  set.seed(seed)
  
  # Figure out where the random, fire-independent point samples will be within the resolve biome
  biome_sample_points <- 
    sf::st_sample(x = biome_buff, size = n_pts, type = "hexagonal") %>% 
    sf::st_sf() %>% 
    dplyr::mutate(samp_id = 1:nrow(.),
                  x_samp = sf::st_coordinates(.)[, "X"],
                  y_samp = sf::st_coordinates(.)[, "Y"]) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate(set = sample(1:n_sets, size = nrow(.), replace = TRUE))
  
  data.table::fwrite(x = biome_sample_points, file = paste0("data/out/fired/04_fire-independent-locations/fire-independent-locations_", short_name, "_", version, ".csv")))

  # Pack each row representing the the actual fire spread geometries with a data.frame
  # that represents the new set of n_pts centroids for those fire spread geometries, then
  # unnest the data to expand it (n_pts rows now for each of the fire/day combinations)
  # Note this could result in millions of geometries
  fired_biome_with_samps <-
    fired_biome %>% 
    dplyr::mutate(samps = list(biome_sample_points)) %>% 
    tidyr::unnest(cols = "samps")
  
  # Split data (randomly) into more manageable sizes 
  # The random set assignments should be at the "sample id" level, such that all samp_id with a
  # given value will go into the same set (and therefore all FIRED polygons will be represented at
  # that particular random location)
  set_names <- stringr::str_pad(string = as.character(1:n_sets), width = 2, side = "left", pad = "0")
  fnames <- paste0("fire-independent-locations_", short_name, "_", version, "_", set_names)
  l <- split(x = fired_biome_with_samps, f = fired_biome_with_samps$set)
  
  out <- pblapply(X = 1:n_sets, cl = n_sets, FUN = function(i) {
    
    # What are the new centroids for each of those geometries?
    samps_geo <-
      l[[i]] %>% 
      sf::st_drop_geometry() %>% 
      sf::st_as_sf(coords = c("x_samp", "y_samp"), crs = 3310) %>%
      sf::st_geometry()
    
    # What are the old centroids of the actual locations of the fire/day combinations? 
    fired_centroids <- 
      l[[i]] %>% 
      sf::st_drop_geometry() %>% 
      sf::st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 3310) %>% 
      sf::st_geometry()
    
    # What does the affine transformation look like to slide each fire/day combination
    # geometry from its actual location to the new centroid (1 of n_pts)?
    affine_transformation <- samps_geo - fired_centroids
    
    # Apply the 6.2 million affine transformations to the original fire/day combination
    # geometries and set the CRS to what we know it is: EPSG3310
    new_fired_geo <- 
      (sf::st_geometry(l[[i]]) + affine_transformation) %>% 
      sf::st_set_crs(3310)
    
    # Updated sf object of 6.2 million geometires now has each fire/day combination geometry
    # located at 1 of 1,000 sample points within the Temperate Conifer Forest biome
    fired_biome_with_new_geo <- sf::st_set_geometry(x = l[[i]], value = new_fired_geo)
    fired_biome_with_new_geo_simple <-
      fired_biome_with_new_geo %>% 
      dplyr::select(did, id, date, samp_id)
    
    # write to disk
    dir.create(paste0("data/out/fired/04_fire-independent-locations/", fnames[i]), recursive = TRUE, showWarnings = FALSE)
    sf::st_write(obj = fired_biome_with_new_geo_simple, 
                 dsn = paste0("data/out/fired/04_fire-independent-locations/", fnames[i], "/", paste0(fnames[i], ".shp")), 
                 delete_dsn = file.exists(paste0("data/out/fired/04_fire-independent-locations/", fnames[i], "/", paste0(fnames[i], ".shp"))))
    
    # v2 used 1,000 points
    # v3 used 1,000 points and the simplified sf object with just did, date, id, samp_id
    # v4 randomizes which of the n_pts go into which of the n_sets sets.
    return(fired_biome_with_new_geo_simple)
  })
  return(out)
}

(start_time <- Sys.time())
set_fire_independent_locations(biome = "Temperate Conifer Forests", short_name = "tcf", 
                               buffer = 50000, seed = 1224, n_sets = 5, n_pts = 500,
                               version = "v4")

(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
# Time difference of 86.65956 mins for 1,000 points in the Temperate Conifer Forest

(start_time <- Sys.time())
set_fire_independent_locations(biome = "Mediterranean Forests, Woodlands & Scrub", 
                               short_name = "mfws", 
                               buffer = 50000, seed = 1224, n_sets = 5, n_pts = 500,
                               version = "v4")

(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))


(start_time <- Sys.time())
set_fire_independent_locations(biome = "Temperate Grasslands, Savannas & Shrublands",
                               short_name = "tgss", 
                               buffer = 50000, seed = 1224, n_sets = 5, n_pts = 500,
                               version = "v4")

(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))

(start_time <- Sys.time())
set_fire_independent_locations(biome = "Deserts & Xeric Shrublands", short_name = "dxs", 
                               buffer = 50000, seed = 1224, n_sets = 5, n_pts = 500,
                               version = "v4")

(end_time <- Sys.time())
(difftime(time1 = end_time, time2 = start_time, units = "mins"))
