library(sf)
library(dplyr)
library(USAboundaries)
library(raster)
library(fasterize)

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  st_transform(3310)

ca_r <- raster::raster(ca, res = c(1000, 1000))

frap <- 
  sf::st_read("data/out/frap-perims-during-modis-record.gpkg") %>% 
  mutate(source = "frap",
         year = as.numeric(YEAR_),
         external_id = id,
         external_name = FIRE_NAME) %>% 
  rename(geometry = geom)

mtbs <- 
  sf::st_read("data/raw/mtbs_perimeter_data/mtbs_perims_DD.shp") %>% 
  mutate(id = Event_ID,
         source = "mtbs",
         year = lubridate::year(Ig_Date),
         external_id = id,
         external_name = Incid_Name) %>% 
  filter(substr(id, start = 1, stop = 2) == "CA") %>% 
  st_transform(3310)

external_data_sources <- 
  rbind(frap[, c("external_id", "source", "year", "external_name")], 
        mtbs[, c("external_id", "source", "year", "external_name")])

# biggest expansions
# fired <- 
#   sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california_biggest-expansions.gpkg") %>% 
#   mutate(id_fired = id)

# all FIRED in california
fired <- 
  sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california.gpkg") %>% 
  mutate(id_fired = id)

out <- vector(mode = "list", length = nrow(fired))

for (i in 1:nrow(fired)) {
  print(i)
  this_fired <- fired[i, ]
  this_year <- fired$ignition_year[i]
  this_year_external <- 
    external_data_sources[external_data_sources$year == this_year, ] %>% 
    st_cast("MULTIPOLYGON") %>% 
    mutate(external_id = as.factor(external_id))
  
  intersections <- 
    try({
      st_intersection(x = this_fired, y = this_year_external) %>% 
    rename(geometry = geom)
      })
  
  if("try-error" %in% class(intersections)) {
    this_fired_r <- fasterize::fasterize(sf = this_fired, raster = ca_r)
    this_external_r <- fasterize::fasterize(sf = this_year_external, r = ca_r, field = "external_id", by = "source")
    
    intersections <- 
      raster::mask(x = this_external_r, mask = this_fired_r) %>% 
      as.data.frame() %>% 
      tidyr::pivot_longer(cols = c(frap, mtbs), 
                          names_to = "source", 
                          values_to = "external_id") %>% 
      filter(complete.cases(.)) 
    
    best_fit <-
      intersections %>% 
      group_by(source, external_id) %>% 
      summarize(n = n()) %>% 
      filter(n == max(n)) %>% 
      ungroup() %>% 
      mutate(overlapping_area = units::set_units(n * 100, ha),
             overlapping_pct = overlapping_area / this_fired$total_area_ha,
             external_id = levels(this_year_external$external_id)[external_id],
             id_fired = this_fired$id_fired) %>% 
      left_join(y = this_year_external) %>% 
      dplyr::select(id_fired, year, external_id, external_name, source, overlapping_area, overlapping_pct) %>% 
      rename(id = external_id, name = external_name)
      
    out[[i]] <- best_fit
    
  } else {
  
  best_fit <- 
    intersections %>% 
    mutate(overlapping_area = units::set_units(st_area(.), "ha")) %>% 
    group_by(source) %>% 
    mutate(max_overlapping_area = max(overlapping_area)) %>% 
    filter(overlapping_area == max_overlapping_area) %>% 
    st_drop_geometry() %>% 
    mutate(overlapping_pct = overlapping_area / this_fired$total_area_ha) %>% 
    dplyr::select(id_fired, year, external_id, external_name, source, overlapping_area, overlapping_pct) %>% 
    rename(id = external_id, name = external_name)
  
  out[[i]] <- best_fit
  
  }
  
}

out <- 
  out %>% 
  bind_rows() %>% 
  tidyr::pivot_wider(id_cols = c(id_fired, year), names_from = source, values_from = c(id, name, overlapping_area, overlapping_pct))

fired_joined_to_external <- left_join(fired, out)

st_write(obj = fired_joined_to_external, 
         dsn = "data/out/fired_events_conus_nov2001-jan2019_california_frap-mtbs-joined.gpkg")
