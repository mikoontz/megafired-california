library(dplyr)
library(data.table)
library(sf)
library(pbapply)
library(parallel)

viirs <- 
  data.table::fread("data/out/viirs_suomi_ca.csv") %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_transform(3310)

# There are some "MULTISURFACE" geometry types that need to be dealt with
# by casting them to MULTIPOLYGONS
wildfires <- 
  sf::read_sf("data/raw/fire21_1.gdb/", layer = "firep21_1") %>%
  sf::st_transform(3310) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  dplyr::mutate(year = as.numeric(year_)) %>% 
  dplyr::filter(year >= 2012) %>% 
  dplyr::select(-year_) %>% 
  dplyr::mutate(alarm_date = lubridate::with_tz(alarm_date, tzone = "UTC"),
                cont_date = lubridate::with_tz(cont_date, tzone = "UTC")) %>% 
  st_cast(to = "MULTIPOLYGON")

rxfires <- 
  sf::read_sf("data/raw/fire21_1.gdb/", layer = "rxburn21_1") %>%
  sf::st_transform(3310) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  dplyr::mutate(year = as.numeric(year_)) %>% 
  dplyr::filter(year >= 2012) %>% 
  dplyr::select(-year_) %>% 
  dplyr::mutate(start_date = lubridate::with_tz(start_date, tzone = "UTC"),
                end_date = lubridate::with_tz(end_date, tzone = "UTC")) %>% 
  st_cast(to = "MULTIPOLYGON")

n_workers <- 10
# Set up parallelization
if (.Platform$OS.type == "windows") {
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(dplyr)
    library(sf)
    library(lubridate)
    library(data.table)
    
    viirs <- 
      data.table::fread("data/out/viirs_suomi_ca.csv") %>% 
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
      sf::st_transform(3310)
    
    # There are some "MULTISURFACE" geometry types that need to be dealt with
    # by casting them to MULTIPOLYGONS
    wildfires <- 
      sf::read_sf("data/raw/fire21_1.gdb/", layer = "firep21_1") %>%
      sf::st_transform(3310) %>% 
      dplyr::rename_all(.funs = tolower) %>% 
      dplyr::mutate(year = as.numeric(year_)) %>% 
      dplyr::filter(year >= 2012) %>% 
      dplyr::select(-year_) %>% 
      dplyr::mutate(alarm_date = lubridate::with_tz(alarm_date, tzone = "UTC"),
                    cont_date = lubridate::with_tz(cont_date, tzone = "UTC")) %>% 
      st_cast(to = "MULTIPOLYGON")
    
    rxfires <- 
      sf::read_sf("data/raw/fire21_1.gdb/", layer = "rxburn21_1") %>%
      sf::st_transform(3310) %>% 
      dplyr::rename_all(.funs = tolower) %>% 
      dplyr::mutate(year = as.numeric(year_)) %>% 
      dplyr::filter(year >= 2012) %>% 
      dplyr::select(-year_) %>% 
      dplyr::mutate(start_date = lubridate::with_tz(start_date, tzone = "UTC"),
                    end_date = lubridate::with_tz(end_date, tzone = "UTC")) %>% 
      st_cast(to = "MULTIPOLYGON")
  })
} else {
  cl <- n_workers # number of workers for Unix-like machines
}


wildfires_viirs <-
  pblapply(X = 1:nrow(wildfires), cl = cl, FUN = function(i) {
  
  this_wildfire <- wildfires[i, ]
  this_wildfire_viirs_time_olap <- 
    viirs %>% 
    dplyr::filter(acq_datetime >= this_wildfire$alarm_date & acq_datetime <= this_wildfire$cont_date)
  
  this_wildfire_viirs_spacetime_olap <-
    this_wildfire %>% 
    sf::st_intersection(y = this_wildfire_viirs_time_olap)
  
  return(this_wildfire_viirs_spacetime_olap)
})

parallel::stopCluster(cl = cl)

test <- do.call("rbind", wildfires_viirs)
unique_fires_with_viirs <-
  unique(test$inc_num)

wildfires <-
  wildfires %>% 
  mutate(with_viirs = ifelse(inc_num %in% unique_fires_with_viirs, "yes", "no"))

ggplot(wildfires, aes(y = log10(gis_acres), x = with_viirs)) +
  geom_violin()

wildfires %>% 
  st_drop_geometry() %>% 
  filter(!is.na(alarm_date) & !is.na(cont_date) & !(inc_num %in% c("002595", "00003951"))) %>% 
  group_by(with_viirs) %>% 
  summarize(max_size = max(gis_acres))

# Cedar Fire of 2016 has wrong containment date; should be 2016-09-30
# Whaleback Fire of 2018 should be 2018-07-27 to 2018-08-07
bad <- wildfires %>% 
  filter(!is.na(alarm_date) & !is.na(cont_date) & !(inc_num %in% c("002595", "00003951"))) %>% 
  filter(with_viirs == "no") %>% 
  filter(gis_acres == max(gis_acres))


events <- 
  sf::st_read("data/out/fired_events_ca.gpkg") %>% 
  dplyr::select(id, ignition_date, ignition_day, ignition_month, ignition_year, last_date, event_duration, total_area_ha) %>% 
  dplyr::filter(ignition_date >= lubridate::ymd("2012-01-01"))

modis_afd <- 
  data.table::fread("data/out/mcd14ml_ca.csv", colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
  dplyr::mutate(acq_date = lubridate::ymd(acq_date)) %>% 
  dplyr::filter(acq_date >= lubridate::ymd("2012-01-01"))

n_workers <- 10

# maximum scan is 4.8 km, meaning the centroid could represent
# a fire 2.4 km on either side ~east/west
# maximum track is 2 km, meaning the centroid could represent
# a fire 1 km on either side ~north/south
# So the hypotenuse of the 1 km x 2.4 km rectangle represents
# the longest possible distance a centroid could be from a fire
# and still represent an active fire within the defined burned area
# is 2.6 km. Of course, there could be additional uncertainties in
# the exact locations of the fire perimeters, too.
fired_with_afd <- 
  sf::st_join(sf::st_buffer(x = wildfires, dist = 2600), modis_afd) %>% 
  filter(acq_date >= alarm_date & acq_date <= cont_date) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

both <- 
  test %>% 
  dplyr::select(inc_num, frp, satellite) %>%
  dplyr::rename(geometry = shape) %>% 
  rbind(fired_with_afd[, c("inc_num", "frp", "satellite")])

both_summary <-
  both %>% 
  group_by(inc_num) %>% 
  tally()

length(unique(fired_with_afd$inc_num))
length(unique(test$inc_num))
length(unique(both$inc_num))
length(unique(wildfires$inc_num))

fired_with_afd %>% 
  filter(satellite == "Terra") %>%
  st_drop_geometry() %>% 
  summarize(length(unique(inc_num)))
