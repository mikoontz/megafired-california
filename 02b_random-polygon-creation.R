# Generate variously-sized polygons across California to determine landcover
# characteristics independent of fire

library(sf)
library(dplyr)
library(stringr)
library(USAboundaries)

dir.create("data/out/fire-independent-polygons_tcf_ca", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fire-independent-polygons_tgss_ca", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fire-independent-polygons_mfws_ca", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fire-independent-polygons_dxs_ca", recursive = TRUE, showWarnings = FALSE)

fired_events <- sf::st_read("data/out/fired_events_ca_epsg3310_2003-2020.gpkg")
fired_daily <- sf::st_read("data/out/fired_daily_ca_epsg3310_2003-2020.gpkg")

# Expand the daily fire perimeters such that they are placed on top of 1000 random points (within each Resolve Biome)

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  dplyr::select(BIOME_NAME, ECO_NAME) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  sf::st_set_agr(value = "constant")

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_geometry()

ca_or_nv_az <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Oregon", "Nevada", "Arizona")) %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_union() %>% 
  sf::st_geometry()

# Temperate Conifer Forests
tcf <- 
  resolve %>% 
  dplyr::filter(biome_name == "Temperate Conifer Forests") %>% 
  sf::st_intersection(y = ca) %>% 
  sf::st_set_agr(value = "constant")

# Temperate Grasslands, Savannas, and Shrublands
tgss <- 
  resolve %>% 
  dplyr::filter(biome_name == "Temperate Grasslands, Savannas & Shrublands") %>% 
  sf::st_intersection(y = ca) %>% 
  sf::st_set_agr(value = "constant")

# Mediterranean Forests, Woodlands & Scrub
mfws <- 
  resolve %>% 
  dplyr::filter(biome_name == "Mediterranean Forests, Woodlands & Scrub") %>% 
  sf::st_intersection(y = ca) %>% 
  sf::st_set_agr(value = "constant")

# Deserts and Xeric Shrublands
dxs <- 
  resolve %>% 
  dplyr::filter(biome_name == "Deserts & Xeric Shrublands") %>% 
  sf::st_intersection(y = ca) %>% 
  sf::st_set_agr(value = "constant")

# Generate the variously-sized circle geometries that will be used to collect driver data
# at random points across California
areas_to_represent <- c(250 * 250, 10^5, 10^5.5, 10^6, 10^6.5, 10^7, 10^7.5, 10^8, 10^8.5, 10^9)
radii <- sqrt(areas_to_represent / pi)
num_pts <- 1000

generate_fire_independent_polygons <- function(biome, seed) {
  
  # Buffer in from the coast, but not from the internal borders of California with other states
  # Only buffer by 18,000 meters because our largest radius is smaller than this; 
  # We'll get better coverage of the coastal mountains this way
  biome_buff <- sf::st_intersection(x = biome, 
                                    y = sf::st_buffer(x = ca_or_nv_az, 
                                                      dist = -(round(radii[length(radii)] / 1000) * 1000)))
  
  set.seed(seed = seed)
  biome_sample_points <- 
    sf::st_sample(x = biome_buff, size = num_pts, type = "hexagonal") %>% 
    sf::st_sf() %>% 
    dplyr::mutate(samp_id = 1:nrow(.),
                  x_samp = sf::st_coordinates(.)[, "X"],
                  y_samp = sf::st_coordinates(.)[, "Y"]) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::as_tibble()
  
  (start_time <- Sys.time())
  
  random_polys <- 
    sf::st_multipoint(x = matrix(data = 0, nrow = length(radii), ncol = 2)) %>% 
    sf::st_sfc(crs = 3310) %>% 
    sf::st_sf() %>% 
    sf::st_cast("POINT") %>% 
    sf::st_buffer(dist = radii) %>% 
    dplyr::mutate(id = paste0("random-poly-", str_pad(as.character(1:nrow(.)), width = max(nchar(as.character(1:nrow(.)))), side = "left", pad = "0"))) %>% 
    dplyr::mutate(date = list(lubridate::ymd(paste(2003:2020, "01", "01", sep = "-")))) %>% 
    tidyr::unnest(cols = "date") %>% 
    dplyr::mutate(did = paste0(id, "-", date)) %>% 
    dplyr::select(did, id, date)
  
  # Pack each row representing the the actual fire spread geometries with a data.frame
  # that represents the new set of 1,000 centroids for those fire spread geometries, then
  # unnest the data to expand it (1,000 rows now for each of the 180 year/random geometry
  # combinations)
  # Note this results in 180,000 geometries
  random_polys_with_samps <-
    random_polys %>% 
    dplyr::mutate(samps = list(biome_sample_points)) %>% 
    tidyr::unnest(cols = "samps")
  
  # What are the new centroids for each of those 180,000 geometries?
  samps_geo <-
    random_polys_with_samps %>% 
    sf::st_drop_geometry() %>% 
    sf::st_as_sf(coords = c("x_samp", "y_samp"), crs = 3310) %>%
    sf::st_geometry()
  
  # What does the affine transformation look like to slide each of the 180,000
  # circles (all centered on 0,0) to their new homes?
  # Apply the 180,000 affine transformations to the random circle polygons
  # geometries and set the CRS to what we know it is: EPSG3310
  new_fired_geo <- 
    (sf::st_geometry(random_polys_with_samps) + samps_geo) %>% 
    sf::st_set_crs(3310)
  
  # Updated sf object of 180,000 geometires now has each fire/day combination geometry
  # located at 1 of 1,000 sample points within the Temperate Conifer Forest biome
  random_polys_new_geo <- sf::st_set_geometry(x = random_polys_with_samps, value = new_fired_geo)
  random_polys_new_geo_simple <-
    random_polys_new_geo %>% 
    dplyr::select(did, id, date, samp_id)
  
  return(random_polys_new_geo_simple)
}

# Temperate conifer forests
fire_independent_tcf <- generate_fire_independent_polygons(tcf, seed = 1224)
test_tcf <-
  fire_independent_tcf %>% 
  dplyr::filter(lubridate::year(date) == 2003)

# Temperate grasslands savanna shrublands
fire_independent_tgss <- generate_fire_independent_polygons(tgss, seed = 1644)
test_tgss <-
  fire_independent_tgss %>% 
  dplyr::filter(lubridate::year(date) == 2003)

# Mediterranean woodland forest scrub
fire_independent_mfws <- generate_fire_independent_polygons(mfws, seed = 127)
test_mfws <-
  fire_independent_mfws %>% 
  dplyr::filter(lubridate::year(date) == 2003)

# desert xeric shrublands
fire_independent_dxs <- generate_fire_independent_polygons(dxs, seed = 1645)
test_dxs <-
  fire_independent_dxs %>% 
  dplyr::filter(lubridate::year(date) == 2003)

par(mfrow = c(2,2))

plot(st_geometry(test_tcf))
plot(st_geometry(ca), add = TRUE)

plot(st_geometry(test_tgss))
plot(st_geometry(ca), add = TRUE)

plot(st_geometry(test_mfws))
plot(st_geometry(ca), add = TRUE)

plot(st_geometry(test_dxs))
plot(st_geometry(ca), add = TRUE)

# Write to disk
sf::st_write(obj = fire_independent_tcf, dsn = "data/out/fire-independent-polygons_tcf_ca.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fire_independent_tcf, dsn = "data/out/fire-independent-polygons_tcf_ca/fire-independent-polygons_tcf_ca.shp", delete_dsn = TRUE)

sf::st_write(obj = fire_independent_tgss, dsn = "data/out/fire-independent-polygons_tgss_ca.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fire_independent_tgss, dsn = "data/out/fire-independent-polygons_tgss_ca/fire-independent-polygons_tgss_ca.shp", delete_dsn = TRUE)

sf::st_write(obj = fire_independent_mfws, dsn = "data/out/fire-independent-polygons_mfws_ca.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fire_independent_mfws, dsn = "data/out/fire-independent-polygons_mfws_ca/fire-independent-polygons_mfws_ca.shp", delete_dsn = TRUE)

sf::st_write(obj = fire_independent_dxs, dsn = "data/out/fire-independent-polygons_dxs_ca.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fire_independent_dxs, dsn = "data/out/fire-independent-polygons_dxs_ca/fire-independent-polygons_dxs_ca.shp", delete_dsn = TRUE)
