library(sf)
library(USAboundaries)
library(data.table)
library(lwgeom)
library(rmapshaper)
library(pbapply)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired/01_spatial-subset", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/active-fire", recursive = TRUE, showWarnings = FALSE)

### FIRED ###
# To get FIRED perimeters, we ran the firedpy algorithm using this specific commit:
# https://github.com/earthlab/firedpy/commit/9a585f828a8e8e7510a15171ff0311457defad13
# firedpy -proj_dir H:\dev\megafired_california\data\raw --shapefile -landcover_type 1 -daily yes

fired_daily <- sf::st_read("data/raw/fired_daily.gpkg") %>% sf::st_transform(3310)
fired_events <- sf::st_read("data/raw/fired_events.gpkg") %>% sf::st_transform(3310)

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_geometry()

events_ca <- 
  sf::st_intersection(x = fired_events, y = ca) %>% 
  dplyr::mutate(total_area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  dplyr::rename(geometry = geom)

daily_ca <- 
  fired_daily %>% 
  dplyr::filter(id %in% events_ca$id) %>% 
  dplyr::rename(geometry = geom) %>% 
  mutate(daily_area_ha = as.numeric(sf::st_area(.) / 10000),
         daily_perim_km = as.numeric(lwgeom::st_perimeter_2d(.) / 1000)) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  mutate(daily_area_tminus1_ha = dplyr::lag(daily_area_ha, default = 0),
         cum_area_ha = cumsum(daily_area_ha),
         cum_area_ha_tminus1 = cum_area_ha - daily_area_ha) %>% 
  dplyr::mutate(daily_perim_tminus1_km = dplyr::lag(x = daily_perim_km, default = 0)) %>% 
  dplyr::ungroup()

get_fireline_length <- function(x) {
  
  out <- x
  out$active_fireline_km <- 0
  
  if(nrow(out) > 1) {
    for (i in 2:nrow(out)) {
      geom <- out[i, ]
      geom_tminus1 <- out[i - 1, ]
      
      topology_fix <- 
        terra::snap(x = vect(rbind(geom, geom_tminus1)), tolerance = 100) %>% 
        sf::st_as_sf() %>% 
        sf::st_intersection() %>% 
        dplyr::filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING"))
      
      if(nrow(topology_fix) > 0) {
        intersecting_perims_length <- 
          topology_fix %>% 
          dplyr::mutate(length = sf::st_length(.)) %>% 
          dplyr::summarize(length = as.numeric(sum(length))) %>% 
          dplyr::pull(length)
        
        out$active_fireline_km[i] <- intersecting_perims_length / 1000
        
      } 
    }
  }
  return(out)
}

n_workers <- 10
# Set up parallelization
if (.Platform$OS.type == "windows") {
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(dplyr)
    library(sf)
    library(terra)
    
  })
} else {
  cl <- n_workers # number of workers for Unix-like machines
}

daily_ca_with_perims_l <- 
  daily_ca %>% 
  split(f = .$id) %>% 
  pblapply(cl = cl, FUN = get_fireline_length)

daily_ca_with_perims <- do.call(what = "rbind", args = daily_ca_with_perims_l)

# nrow(daily_ca) ## 20520 day/event unique combinations
# length(unique(daily_ca$id)) ## 4337 unique fire events
# Version 2 of the daily data includes daily perimeter lengths for each event/day combo
# as well as a measure of "active fireline
sf::st_write(daily_ca_with_perims, "data/out/fired/01_spatial-subset/fired_daily_ca_v2.gpkg", delete_dsn = TRUE)
sf::st_write(events_ca, dsn = "data/out/fired/01_spatial-subset/fired_events_ca.gpkg", delete_dsn = TRUE)

###

### MCD14ML MODIS active fire product ###

afd <- 
  data.table::fread(input = "data/raw/DL_FIRE_M-C61_200690/fire_archive_M-C61_200690.csv",
                    colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
  sf::st_transform(3310)

afd_ca <- 
  sf::st_intersection(x = afd, y = ca) %>% 
  dplyr::mutate(pixel_area = track * scan,
                frp_per_area = frp / pixel_area) %>% 
  mutate(x = sf::st_coordinates(.)[, 1],
         y = sf::st_coordinates(.)[, 2]) %>% 
  sf::st_drop_geometry()

data.table::fwrite(x = afd_ca, file = "data/out/active-fire/mcd14ml_ca.csv")