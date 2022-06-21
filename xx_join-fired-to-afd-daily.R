library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

FIRED <- sf::st_read("data/out/fired_daily_ca.gpkg")
events <- sf::st_read("data/out/fired_events_ca.gpkg")

modis_afd <- 
  data.table::fread("data/out/mcd14ml_ca.csv", colClasses = c(acq_time = "character")) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE) %>% 
  dplyr::mutate(acq_date = lubridate::ymd(acq_date)) %>% 
  dplyr::mutate(local_datetime = lubridate::ymd_hm(paste0(acq_date, " ", acq_time))  - lubridate::hours(x = 7),
                local_hour = lubridate::hour(local_datetime),
                local_minute = lubridate::minute(local_datetime),
                local_date = lubridate::ymd(paste(lubridate::year(local_datetime),
                                                  lubridate::month(local_datetime),
                                                  lubridate::day(local_datetime),
                                                  sep = "-")))

fired_frap_mtbs_join <- read.csv("data/out/fired-frap-mtbs-join.csv")

fires <- left_join(FIRED, fired_frap_mtbs_join, by = c(id = "id_fired")) 

fired_with_afd <- 
  sf::st_join(sf::st_buffer(x = fires, dist = 2600), modis_afd) %>% 
  filter(local_date == date) %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)

sf::st_write(obj = fired_with_afd, dsn = "incubator/fired-joined-with-mcd14ml.gpkg", delete_dsn = TRUE)

fires
rim <- fires %>% dplyr::filter(id == 90182)
rim_afd <- fired_with_afd[which(fired_with_afd$id == 90182), ]

ggplot() +
  geom_sf(data = rim[rim$event_day %in% 7:12, ]) +
  geom_sf(data = rim_afd[rim_afd$event_day %in% 7:12, ], col = "red", size = 0.25) +
  facet_wrap(facets = "event_day")

ggplot() +
  geom_sf(data = rim[rim$event_day %in% 7:12, ]) +
  geom_sf(data = rim_afd[rim_afd$event_day %in% 7:12, ], mapping = aes(col = daynight), size = 0.25) +
  facet_wrap(facets = "event_day")

ggplot() +
  geom_sf(data = rim[rim$event_day %in% 7:12, ]) +
  geom_sf(data = rim_afd[rim_afd$event_day %in% 7:12, ], mapping = aes(col = local_hour), size = 0.25) +
  facet_wrap(facets = "event_day") +
  scale_color_viridis_c()

ggplot() +
  geom_sf(data = events[events$id == 90182, ]) +
  geom_sf(data = rim_afd, color = "red") +
  theme_bw()

ggplot() +
  geom_sf(data = events[events$id == 90182, ]) +
  geom_sf(data = rim_afd, mapping = aes(color = log(frp_per_area))) +
  theme_bw() +
  scale_color_viridis_c(option = "inferno") +
  labs(color = "log(FRP)\n(proxy for fire intensity)")

round((as.numeric(rim_afd$acq_time) - 700) / 100)

king <- fires %>% dplyr::filter(id == 96078)
king_afd <- fired_with_afd[which(fired_with_afd$id == 96078), ]

ggplot() +
  geom_sf(data = king[king$event_day %in% 1:10, ]) +
  geom_sf(data = king_afd[king_afd$event_day %in% 1:10, ], col = "red", size = 0.25) +
  facet_wrap(facets = "event_day", nrow = 2) +
  theme_bw()
