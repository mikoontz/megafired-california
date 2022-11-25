library(sf)
library(dplyr)
library(USAboundaries)
library(raster)
library(fasterize)
library(pbapply)

dir.create(path = "data/out/fired/03_joined-with-other-data", recursive = TRUE, showWarnings = FALSE)

### --- Use the Resolve ecoregions to join to the daily fire polygons and the events
resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  dplyr::select(BIOME_NAME, ECO_NAME) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  sf::st_set_agr(value = "constant")

events_resolve_geo <- 
  sf::st_read(dsn = "data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020.gpkg") %>% 
  sf::st_join(y = resolve, largest = TRUE)

daily_resolve_geo <- 
  sf::st_read(dsn = "data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  sf::st_join(y = resolve, largest = TRUE)

events_resolve_out <- 
  events_resolve_geo %>% 
  sf::st_drop_geometry() %>% 
  dplyr::as_tibble()

daily_resolve_out <- 
  daily_resolve_geo %>% 
  sf::st_drop_geometry() %>% 
  dplyr::rename(biome_name_daily = biome_name, eco_name_daily = eco_name) %>% 
  dplyr::left_join(sf::st_drop_geometry(events_resolve_out), by = "id") %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(did, id, date, samp_id, biome_name, eco_name, biome_name_daily, eco_name_daily)

write.csv(x = events_resolve_out, file = "data/out/fired/03_joined-with-other-data/fired_events_resolve.csv", row.names = FALSE)
write.csv(x = daily_resolve_out, file = "data/out/fired/03_joined-with-other-data/fired_daily_resolve.csv", row.names = FALSE)

### Separately, join to MTBS and FRAP
fired <- sf::st_read("data/out/fired/01_spatial-subset/fired_events_ca.gpkg") %>% mutate(id_fired = id)

frap <- 
  st_read("data/raw/fire20_1.gdb", layer = "firep20_1", as_tibble = FALSE) %>% 
  mutate(ALARM_DATE = lubridate::with_tz(time = ALARM_DATE, tzone = "UTC"),
         CONT_DATE = lubridate::with_tz(time = CONT_DATE, tzone = "UTC")) %>% 
  filter(ALARM_DATE > lubridate::ymd(min(fired$ignition_date))) %>% 
  mutate(id = 1:nrow(.)) %>% 
  mutate(source = "frap",
         year = as.numeric(YEAR_),
         external_id = id,
         external_name = FIRE_NAME) %>% 
  rename(geometry = Shape) %>% 
  dplyr::select(id, everything())

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  st_transform(3310)

ca_r <- raster::raster(ca, res = c(1000, 1000))

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
  tidyr::pivot_wider(id_cols = c(id_fired, year), names_from = source, values_from = c(id, name, overlapping_area, overlapping_pct)) %>% 
  dplyr::select(id_fired, id_frap, id_mtbs, year, name_frap, name_mtbs, everything())

write.csv(x = out, file = "data/out/fired/03_joined-with-other-data/fired-frap-mtbs-join.csv", row.names = FALSE)
