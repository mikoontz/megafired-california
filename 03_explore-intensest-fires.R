# Get daily FRP for California FIRED data

library(sf)
library(dplyr)
library(magrittr)
library(ggplot2)
library(pbapply)
library(USAboundaries)
library(purrr)
library(parallel)
library(gganimate)
library(geosphere)
library(fields)


fired_ca <- sf::st_read("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
# fired_ca_event <- sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california.gpkg") %>% dplyr::select(id)
fired_ca_event <- 
  sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california.gpkg") %>% 
  dplyr::select(id, ignition_date, last_date, max_growth_ha, final_perimeter_m, duration, total_area_ha)

big <- sf::st_read("data/out/fired_conus_daily_nov2001-jan2019_california_biggest-expansions.gpkg")
ca <- USAboundaries::us_states(resolution = "high", states = "California") %>% sf::st_transform(sf::st_crs(fired_ca))
fired_frap_mtbs <- sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california_frap-mtbs-joined.gpkg")

modis_afd <- 
  sf::st_read("data/raw/DL_FIRE_M6_184262/fire_archive_M6_184262.shp") %>% 
  sf::st_transform(sf::st_crs(fired_ca)) %>% 
  dplyr::mutate(x = st_coordinates(.)[, 1],
                y = st_coordinates(.)[, 2])

n_workers <- 10

# # function to create square buffer around fire points ---------------------
# st_square_buffer <- function(obj, radius = NULL) {
#   pts <- st_coordinates(obj)
#   
#   if(!is.null(radius)) {  
#     xmin <- pts[, "X"] - radius
#     xmax <- pts[, "X"] + radius
#     ymin <- pts[, "Y"] - radius
#     ymax <- pts[, "Y"] + radius
#   } else {
#     xmin <- pts[, "X"] - (pull(obj, SCAN) * 1000 / 2)
#     xmax <- pts[, "X"] + (pull(obj, SCAN) * 1000 / 2)
#     ymin <- pts[, "Y"] - (pull(obj, TRACK) * 1000 / 2)
#     ymax <- pts[, "Y"] + (pull(obj, TRACK) * 1000 / 2)
#   }
#   
#   corners <- tibble(xmin, xmax, ymin, ymax)
#   
#   square_polys <- 
#     corners %>% 
#     pmap(.f = function(xmin, xmax, ymin, ymax) {
#       square_poly <- st_polygon(list(matrix(c(xmin, ymax, 
#                                               xmax, ymax, 
#                                               xmax, ymin, 
#                                               xmin, ymin, 
#                                               xmin, ymax), 
#                                             byrow = TRUE, 
#                                             ncol = 2)))
#       return(square_poly)
#     })
#   
#   # Now that the rectangles are built, we can rotate them properly here based on the
#   # orbital characteristics of the particular satellite that the detection came from
#   # Ascending/descending based on time + satellite combination
#   # Orbital inclination (and thus rotation) based on satellite + ascending/descending
#   # An enhancement for another time.
#   
#   # rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
#   # cntrd = st_centroid(st_geometry(x))
#   # rot_x = (st_geometry(x) - cntrd) * rot(-98.2098 * pi / 180) * 1.1 + cntrd
#   
#   new_obj <-
#     obj %>%
#     st_drop_geometry() %>% 
#     dplyr::mutate(geometry = st_sfc(square_polys, crs = st_crs(obj))) %>% 
#     st_as_sf()
#   
#   return(new_obj) 
# }

# 
# rim_with_afd_by_event <-
#   fired_with_afd %>% 
#   filter(id == 57206) %>% 
#   dplyr::filter(complete.cases(.)) %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
#   st_square_buffer()
# 
# king_with_afd_by_event <-
#   fired_with_afd %>% 
#   filter(id == 60932) %>% 
#   dplyr::filter(complete.cases(.)) %>% 
#   sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
#   st_square_buffer()
# 
# x = king_with_afd_by_event

################################
fired_with_afd <- 
  sf::st_join(fired_ca_event, modis_afd) %>% 
  filter(ACQ_DATE >= ignition_date & ACQ_DATE <= last_date) %>% 
  dplyr::mutate(pixel_area = TRACK * SCAN,
                frp_per_area = FRP / pixel_area,
                diag_length = 1000 * sqrt(SCAN ^ 2 + TRACK ^ 2)) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)
  
### testing
# king <- 
#   fired_with_afd %>% 
#   filter(id == 60932)

biggest_frp <-
  fired_with_afd %>% 
  group_by(id) %>% 
  filter(CONFIDENCE > 90 & pixel_area < 2 & frp_per_area == max(frp_per_area)) %>% 
  dplyr::select(id, frp_per_area, everything()) %>% 
  arrange(desc(frp_per_area)) %>% 
  st_drop_geometry()

fired_ca_event_with_frp <-
  fired_ca_event %>% 
  left_join(biggest_frp)

# Max FRP vs. biggest daily growth
ggplot(fired_ca_event_with_frp, aes(x = max_growth_ha, y = FRP)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth()

# Max FRP vs. total size
ggplot(fired_ca_event_with_frp, aes(x = total_area_ha, y = FRP)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth()

# Max FRP vs. duration
ggplot(fired_ca_event_with_frp, aes(x = duration, y = FRP)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth()

fired_ca_event_with_frp %>% arrange(desc(duration))

# Max FRP vs. longest perimeter
ggplot(fired_ca_event_with_frp, aes(x = final_perimeter_m, y = FRP)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth()



fired_with_afd_by_event <-
  fired_with_afd %>%
  # st_square_buffer() %>%
  group_by(id) %>%
  group_split()

king <- 
  fired_with_afd %>% 
  filter(id == 60932)

rim <-
  fired_with_afd %>%
  filter(id == 57206)

ggplot(biggest_frp, aes(x = frp_per_area)) + geom_histogram()
ggplot(biggest_frp, aes(x = pixel_area, y = frp_per_area)) + geom_point() + geom_smooth()


# What grouping should each fire detection belong to? Groups are designated
# if buffered points (based on the diagonal distance of the variably-sized
# pixels) overlap at all
cluster_detections <- function(afd) {
  
  afd %>% 
    st_buffer(dist = .$diag_length / 2) %>% # buffer based on half the max distance in each pixel
    st_union() %>% # "dissolve" polygons into multipolygon
    st_cast(to = "POLYGON") %>% # separate out any multipolygons into individual polys
    st_intersects(x = afd, y = ., sparse = FALSE) %>% # which polys intersect with each other?
    # each row is a detection, each column is a polygon grouping
    # to which column (group) does each detection belong? (which column is TRUE)
    apply(MARGIN = 1, FUN = which) 
}


# Note that this algorithm doesn't currently take into account individual detections at a given datetime
# that overlap *with each other*. That is, two detections can overlap, which could mean that a single
# fire on the ground is detected as each pixel, but the two pixels show up as a much larger footprint
# when they are combined.
# I'm not convinced this is a problem.

designate_edge_and_residual_burning <- function(x) {
  
  # ggplot() + 
  #   geom_sf(data = st_geometry(x), color = "black") + 
  #   geom_sf(data = x, color = "red") + 
  #   facet_wrap(facets = vars(as.factor(datetime)))
  
  # If there is only one active fire detection (one pixel) for the event, then it has to be a "edge" burning pixel
  # set it as such and move on!
  if(nrow(x) == 1) {
    x <-
      x %>% 
      mutate(datetime = clock::naive_time_parse(paste(ACQ_DATE, ACQ_TIME), format = "%Y-%m-%d %H%M"), 
             behavior_type = "edge",
             hrs_since_prior_afd = 0,
             hrs_since_first_afd = 0,
             diag_length = sqrt(SCAN ^ 2 + TRACK ^ 2))
    
    return(x)
  }
  
  # add the datetime to the active fire detections
  x <- 
    x %>% 
    mutate(datetime = lubridate::ymd_hm(paste(ACQ_DATE, ACQ_TIME)))
  
  # what are all the unique datetimes with detections during the event?
  afd_datetimes <- sort(unique(x$datetime))
  
  detections_list <- vector(mode = "list", length = length(afd_datetimes))
  perimeters_list <- vector(mode = "list", length = length(afd_datetimes))
  
  # the first time during the event with an active fire detection
  first_detection_datetime <- afd_datetimes[1]
  
  # first detections
  first_detections <- x[x$datetime == first_detection_datetime, ]
  
  first_detections <-
    first_detections %>% 
    mutate(cluster_id = cluster_detections(.))
  
  # # clustering based on distance (older version; have to set a single distance)
  # # https://stackoverflow.com/questions/28672399/spatial-clustering-in-r-simple-example
  # first_detections <-
  #   first_detections %>%
  #   mutate(cluster_id = cutree(hclust(dist(cbind(x, y)), method = "single"), h = sqrt(2) * 1000))
  
  # Create LINESTRING geometries that represent the outlines of the discrete "patches" of active fire detections
  first_perimeters <-
    first_detections %>%
    group_by(cluster_id) %>%
    summarize() %>%
    st_convex_hull() %>% 
    st_geometry()
  
  # # Create LINESTRING geometries that represent the outlines of the discrete "patches" of active fire detections
  # first_perimeters <- st_union(first_detections) %>% st_cast(to = "POLYGON") %>% st_cast(to = "LINESTRING")
  
  # The active fire detections from the first datetime that touch any of the perimeter(s) should all be "edge burning"
  idx_edge_burning <-
    sf::st_touches(x = first_detections, y = first_perimeters, sparse = FALSE) %>%
    apply(MARGIN = 1, FUN = any)
  
  first_detections <-
    first_detections %>% 
    mutate(behavior_type = ifelse(idx_edge_burning, yes = "edge", no = "residual"),
           hrs_since_prior_afd = 0,
           hrs_since_first_afd = 0)
  
  detections_list[[1]] <- first_detections
  perimeters_list[[1]] <- first_perimeters %>% st_union()
  
  if (length(detections_list) == 1) {
    return(first_detections)
  } else {
    for (i in 2:length(detections_list)) {
      prior_perimeters <- perimeters_list[[i - 1]]
      
      # current detections
      current_detections <- x[x$datetime == afd_datetimes[i], ]
      
      # current_detections <-
      #   current_detections %>% 
      #   mutate(cluster_id = cluster_detections(.))
      
      # which detections occur further than 1000 m * sqrt(2) from the bounds of the previous perimeters?
      idx_outside_prior_perimeters <- 
        st_distance(x = current_detections, y = prior_perimeters, sparse = FALSE) %>% 
        apply(MARGIN = 1, FUN = function(row) return(!any(row < (sqrt(2) * 1000))))
      
      cumulative_detections <- x[x$datetime %in% afd_datetimes[1:i], ]
      cumulative_detections <-
        cumulative_detections %>% 
        mutate(cluster_id = cluster_detections(.))
      
      # What is the current convex hull perimeter of the fire, accounting for all
      # active fire detections thus far?
      current_perimeters <- 
        cumulative_detections %>% 
        group_by(cluster_id) %>%
        summarize() %>%
        st_convex_hull() %>% 
        st_geometry()
      
      # which detections are within 500 m of the outermost perimeter(s) of the 
      # accumulated detections (through time)?
      current_perims_as_unfilled_geoms <-
        lapply(current_perimeters, FUN = function(obj) {
          if(sf::st_geometry_type(obj) == "POLYGON") 
            return(sf::st_cast(obj, to = "LINESTRING"))
          return(obj)
        }) %>% st_as_sfc(crs = 3310)
      
      idx_edge_burning <-
        sf::st_distance(x = current_detections, y = current_perims_as_unfilled_geoms, sparse = FALSE) %>% 
        apply(MARGIN = 1, FUN = function(row) return(any(row < sqrt(2) * 1000)))
      
      # edge burning pixels are those that are both *outside* the prior perimeter AND which 
      # are on the outside of any new discrete patch of detections made at that specific datetime
      current_detections <-
        current_detections %>% 
        mutate(behavior_type = ifelse(idx_outside_prior_perimeters, yes = "edge", no = "residual"),
               hrs_since_prior_afd = as.numeric(difftime(time1 = afd_datetimes[i], time2 = afd_datetimes[i - 1], units = "hours")),
               hrs_since_first_afd = as.numeric(difftime(time1 = afd_datetimes[i], time2 = afd_datetimes[1], units = "hours")))
      
      # ggplot() + geom_sf(data = current_detections, aes(color = behavior_type))
      
      detections_list[[i]] <- current_detections
      perimeters_list[[i]] <- current_perimeters
    }
    
    augmented_detections <- bind_rows(detections_list)
    
    return(augmented_detections)
  }
} # end function

king_behavior <- designate_edge_and_residual_burning(king)
rim_behavior <- designate_edge_and_residual_burning(rim)

king_behavior_gg <-
  ggplot() + 
  geom_sf(data = st_geometry(king_behavior)) +
  geom_sf(data = king_behavior, aes(color = behavior_type)) + 
  facet_wrap(facets = vars(as.factor(datetime)), ncol = 8)

king_behavior_gg

ggsave(filename = "figs/king-edge-vs-residual.pdf", plot = king_behavior_gg)

rim_behavior_gg <-
  ggplot() + 
  geom_sf(data = st_geometry(rim_behavior)) +
  geom_sf(data = rim_behavior, aes(color = behavior_type)) + 
  facet_wrap(facets = vars(as.factor(datetime)), ncol = 8)

ggsave(filename = "figs/rim-edge-vs-residual.pdf", plot = rim_behavior_gg)


# Regression model to look at the departures from expectation
library(mgcv)
fired_ca_aug <- 
  fired_ca %>% 
  mutate(prior_cum_area_ha = (100 * cum_area_km2) - daily_area_ha)

fm1 <- gam(daily_area_ha ~ s(prior_cum_area_ha) + s(id, bs = "re"), data = fired_ca_aug) 
ggplot(fired_ca, aes(x = (100 * cum_area_km2) - daily_area_ha, y = daily_area_ha, group = id)) +
         geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 2)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_bw()


# Next step
# Include a "time since last detection" column 
# Could use this to filter to just "edge" pixels that occurred <4 hours since last detection to have better
# confidence that the pixel is actually "edge"
# Could also plot through time. When does the FRP of edge pixels peak? Probably pretty low at first, since
# the initial detections are all considered "edge" and the fire probably hasn't gained much steam yet.

# setup parallelization
if (.Platform$OS.type == "windows") {
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(dplyr)
    library(sf)
  })
} else {
  cl <- n_workers
}



fi_by_detection_list <- pblapply(X = fired_with_afd_by_event, FUN = designate_edge_and_residual_burning, cl = cl)  
parallel::stopCluster(cl)

fi_by_detection <- bind_rows(fi_by_detection_list)

king_augmented_detections <-
  fi_by_detection %>% 
  dplyr::filter(id == 60932)

king_gg <-
  ggplot() +
  geom_sf(data = st_geometry(king_augmented_detections)) +
  geom_sf(data = mutate(king_augmented_detections, datetime = as.POSIXct(datetime)), aes(fill = behavior_type)) +
  facet_wrap(facets = "datetime")

king_gg

king_gg <-
  ggplot(mutate(king_augmented_detections, datetime = as.POSIXct(datetime)), aes(fill = behavior_type, group = datetime)) +
  geom_sf() +
  gganimate::transition_time(datetime) +
  gganimate::shadow_mark(alpha = 0.2)

king_gg

ggplot(fi_by_detection, aes(x = hrs_since_first_afd, y = ))

fi_by_event <- 
  fi_by_detection %>% 
  st_drop_geometry() %>% 
  mutate(fi_per_pixel = FRP / diag_length) %>% 
  group_by(id, datetime, behavior_type, hrs_since_first_afd, hrs_since_prior_afd) %>% 
  summarize(n = n(), 
            fi_kW_per_m_pixel = sum(fi_per_pixel),
            fi_kW_per_m = sum(FRP) / sum(diag_length)) %>% 
  ungroup()

fi_spatial <-
  fi_by_event %>% 
  filter(behavior_type == "edge") %>% 
  left_join(fired_frap_mtbs, by = "id") %>% 
  st_as_sf() %>% 
  arrange(desc(fi_kW_per_m)) %>% 
  dplyr::select(id, datetime, fi_kW_per_m, fi_kW_per_m_pixel, n, name_frap, name_mtbs, lc_name)

fi <-
  st_drop_geometry(fi_spatial) %>% 
  as.data.frame()

fi_by_detection %>% 
  mutate(fi_per_pixel = FRP / diag_length) %>% 
  left_join(st_drop_geometry(fired_frap_mtbs)) %>% 
  filter(name_mtbs == "KING") %>% 
  arrange(desc(fi_per_pixel)) %>% 
  dplyr::select(id, datetime, behavior_type, FRP, SCAN, TRACK, CONFIDENCE, fi_per_pixel, name_frap, name_mtbs, lc_name) %>% 
  dplyr::select(FRP, fi_per_pixel) %>% 
  plot()

plot(st_geometry(ca))
plot(fi_spatial[1:50, "fi_kW_per_m"], add = TRUE)
fi_spatial[1:50, ]
plot(st_geometry(ca), add = TRUE)
write.csv(file = "data/out/fireline-intensity-by-detection")

fi_by_event %>% 
  filter(behavior_type == "edge") %>% 
  left_join(fired_frap_mtbs, by = "id") %>% 
  st_as_sf()

max_fi_by_event <-
  fi_by_event %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  filter(fi_kW_m == max(fi_kW_m))

named_fi_events <-
  max_fi_by_event %>% 
  arrange(desc(fi_kW_m)) %>% 
  dplyr::select(id, year, datetime, fi_kW_m, total_area_ha, l1_ecoregion, lc_name, name_frap, name_mtbs) %>% 
  as.data.frame()

ggplot(named_fi_events, aes(fi_kW_m, total_area_ha)) + 
  geom_point() + 
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()

ggplot(named_fi_events, aes(lubridate::yday(as.Date(datetime)), fi_kW_m)) + 
  geom_point() + 
  geom_smooth() +
  coord_polar() +
  scale_y_log10()


named_fi_events %>% filter(name_mtbs == "RIM")
fired_frap_mtbs %>% filter(name_mtbs == "RIM")


rim_fi_by_detection <- designate_edge_and_residual_burning(rim_with_afd_by_event)

rim_fi_by_event <-
  rim_fi_by_detection %>% 
  mutate(diag_length = sqrt(SCAN ^ 2 + TRACK ^ 2),
         fi = FRP / diag_length) %>% 
  group_by(id, datetime, behavior_type) %>% 
  summarize(n = n(), fi_kW_m = sum(fi)) %>%
  ungroup() %>% 
  filter(behavior_type == "edge") %>% 
  st_drop_geometry() %>% 
  filter(fi_kW_m == max(fi_kW_m)) %>% 
  left_join(fired_frap_mtbs, by = "id") %>% 
  st_as_sf()

intensest_fires <-
  fi_by_event %>%
  ungroup() %>% 
  filter(fi_kW_m > quantile(fi_kW_m, probs = c(0.995))) %>% 
  arrange(desc(fi_kW_m)) %>% 
  left_join(fired_frap_mtbs, by = "id") %>% 
  st_as_sf()





ggplot(intensest_fires, aes(x = fi_kW_m, fill = behavior_type)) + geom_histogram(bins = 50, alpha = 0.1)







plot(st_geometry(intensest_fires))



















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
