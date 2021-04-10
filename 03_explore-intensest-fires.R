# Get daily FRP for California FIRED data

library(sf)
library(dplyr)
library(ggplot2)
library(pbapply)
library(USAboundaries)
library(purrr)

fired_ca <- sf::st_read("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
fired_ca_event <- sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california.gpkg") %>% dplyr::select(id)
modis_afd <- sf::st_read("data/raw/DL_FIRE_M6_184262/fire_archive_M6_184262.shp") %>% sf::st_transform(sf::st_crs(fired_ca))
big <- sf::st_read("data/out/fired_conus_daily_nov2001-jan2019_california_biggest-expansions.gpkg")
ca <- USAboundaries::us_states(resolution = "high", states = "California") %>% sf::st_transform(sf::st_crs(fired_ca))

# function to create square buffer around fire points ---------------------
st_square_buffer <- function(obj, radius = NULL) {
  pts <- st_coordinates(obj)
  
  if(!is.null(radius)) {  
    xmin <- pts[, "X"] - radius
    xmax <- pts[, "X"] + radius
    ymin <- pts[, "Y"] - radius
    ymax <- pts[, "Y"] + radius
  } else {
    xmin <- pts[, "X"] - (pull(obj, SCAN) * 1000 / 2)
    xmax <- pts[, "X"] + (pull(obj, SCAN) * 1000 / 2)
    ymin <- pts[, "Y"] - (pull(obj, TRACK) * 1000 / 2)
    ymax <- pts[, "Y"] + (pull(obj, TRACK) * 1000 / 2)
  }
  
  corners <- tibble(xmin, xmax, ymin, ymax)
  
  square_polys <- 
    corners %>% 
    pmap(.f = function(xmin, xmax, ymin, ymax) {
      square_poly <- st_polygon(list(matrix(c(xmin, ymax, 
                                              xmax, ymax, 
                                              xmax, ymin, 
                                              xmin, ymin, 
                                              xmin, ymax), 
                                            byrow = TRUE, 
                                            ncol = 2)))
      return(square_poly)
    })
  
  new_obj <-
    obj %>%
    st_drop_geometry() %>% 
    dplyr::mutate(geometry = st_sfc(square_polys, crs = st_crs(obj))) %>% 
    st_as_sf()
  
  return(new_obj) 
}

fired_list <-
  fired_ca %>%
  group_by(date) %>% 
  group_split()

# Example date with lots of fires (10) and detections (>1000)
# fired <- fired_list[[634]]

spacetime_join <- function(fired) {
  this_date <- unique(fired$date)
  afd_on_date <- modis_afd %>% 
    dplyr::filter(ACQ_DATE == this_date) %>% 
    dplyr::mutate(x = st_coordinates(.)[, 1],
                  y = st_coordinates(.)[, 2])
  
  this_fired_with_afd <-
    fired %>%
    dplyr::select(id, date) %>% 
    sf::st_join(afd_on_date) %>% 
    dplyr::mutate(frp_per_area = FRP / (TRACK * SCAN))
  
  return(this_fired_with_afd)
}

fired_with_afd_list <- pblapply(X = fired_list, FUN = spacetime_join)
fired_with_afd <- bind_rows(fired_with_afd_list)

fired_with_afd_by_event <- 
  fired_with_afd %>%
  dplyr::filter(complete.cases(.)) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
  st_square_buffer() %>% 
  group_by(id) %>% 
  group_split() 

designate_front_and_residual_burning <- function(x) {
  if(nrow(x) == 1) return(mutate(x, behavior_type = "front"))
  
  x %<>% mutate(datetime = clock::naive_time_parse(paste(ACQ_DATE, ACQ_TIME), format = "%Y-%m-%d %H%M"))
  
  afd_datetimes <- sort(unique(x$datetime))
  
  first_detection <- afd_datetimes[1]

  first_perimeter <- st_union(x[x$datetime == first_detection, ])
  }

x <- fired_with_afd_by_event[[2]]

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
cntrd = st_centroid(st_geometry(x))
## Warning in st_centroid.sfc(ncg): st_centroid does not give correct centroids for
## longitude/latitude data
rot_x = (st_geometry(x) - cntrd) * rot(-98.2098 * pi / 180) * 1.1 + cntrd
plot(rot_x, col = "red")
plot(cntrd, add = TRUE)
plot(st_geometry(x), add = TRUE)

# Top half percentile
intense_fires <-
  fired_with_afd %>% 
  filter(sum_frp >= quantile(fired_with_afd$sum_frp, probs = 0.995))

ggplot() +
  geom_sf(data = intense_fires) +
  geom_sf(data = ca, fill = NA)

summary(fired_with_afd)
fired_with_afd %>% 
  st_drop_geometry() %>% 
  group_by(lc_name) %>% 
  summarize(mean_sum_frp = mean(sum_frp),
            sum_frp_80 = quantile(sum_frp, probs = 0.8),
            sum_frp_90 = quantile(sum_frp, probs = 0.9))

ggplot(fired_with_afd, aes(x = sum_frp, fill = lc_name)) + 
  geom_density(alpha = 0.2) +
  scale_x_log10()

ids <- unique(intense_fires$id)

st_write(obj = intense_fires, dsn = "data/out/fired_conus_daily_nov2001-jan2019_california_intensest-days.gpkg", delete_dsn = TRUE)

st_write(obj = fired_ca[fired_ca$id %in% ids, ], dsn = "data/out/fired_conus_daily_nov2001-jan2019_california_intensest-days_whole-event.gpkg", delete_dsn = TRUE)

# Just the final perimeters of the events that had the biggest expansions
california_fired_events <- sf::read_sf("data/out/fired_events_conus_nov2001-jan2019_california.gpkg")

st_write(obj = california_fired_events[california_fired_events$id %in% ids, ], dsn = "data/out/fired_events_conus_nov2001-jan2019_california_intensest-days.gpkg", delete_dsn = TRUE)
