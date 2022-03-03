# Subset FPA-FOD data to California

library(dplyr)
library(sf)
library(ggplot2)

fired_events <- sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>% dplyr::filter(ig_year >= 2003)
fired_events <- sf::st_transform(fired_events, 3310)
megafires <- fired_events %>% dplyr::filter(megafire == "megafire")
cedar <- fired_events[fired_events$id == 15863, ]

fires <- sf::st_read("data/raw/RDS-2013-0009.5_GPKG/Data/FPA_FOD_20210617.gpkg")
ca_fires <- fires %>% filter(STATE == "CA")
ca_fires <- sf::st_transform(ca_fires, 3310)
modis_ca_fires <- ca_fires %>% dplyr::filter(FIRE_YEAR >= 2002)

out <- vector(mode = "list", length = nrow(fired_events))

for(i in 1:nrow(fired_events)) {

  out[[i]] <- 
    modis_ca_fires %>% 
    dplyr::mutate(difftime_fpa_fired_days = as.numeric(abs(difftime(DISCOVERY_DATE, fired_events$ig_date[i], units = "day")))) %>% 
    dplyr::filter(difftime_fpa_fired_days <= 10) %>%
    dplyr::mutate(diffdist_fpa_fired_km = as.numeric(st_distance(x = ., y = sf::st_geometry(fired_events[i, ])))  / 1000) %>% 
    dplyr::filter(diffdist_fpa_fired_km <= 3)
      
  # dplyr::filter(sf::st_is_within_distance(x = ., y = sf::st_geometry(fired_events[i, ]), sparse = FALSE, dist = 1000)) %>% 
    # dplyr::filter()
  
  }

matches <- unlist(lapply(out, nrow))
with_ig_points <- fired_events[which(matches > 0), ]

ggplot(with_ig_points, aes(x = tot_hect)) +
  geom_histogram() +
  scale_x_log10()
fired_events <-
  fired_events %>% 
  mutate(num_igs = matches)


ggplot(fired_events, aes(x = ig_year, y = num_igs)) +
  geom_point() +
  geom_jitter(width = 0.1, height = 0.1)

ggplot(fired_events %>% filter(num_igs > 0), aes(x = ig_year, y = num_igs)) +
  geom_point() +
  geom_jitter(width = 0.2, height = 0.2)

fired_events <-
  fired_events %>% 
  mutate(megafire_binary = ifelse(megafire == "megafire", 1, 0))
fm1 <- glm(megafire_binary ~ num_igs, data = fired_events, family = "binomial")
summary(fm1)

fired_events %>% 
  st_drop_geometry() %>% 
  group_by(megafire) %>% 
  summarize(mean_igs = mean(num_igs))

fired_events

ggplot(fired_events %>% filter(ig_year <= 2018), aes(x = tot_hect, fill = (num_igs > 0))) +
  geom_histogram() +
  scale_x_log10()

fired_events[fired_events$num_igs == 0, ] %>% arrange(desc(tot_hect))
small_boi <- with_ig_points %>% arrange(tot_hect) %>% slice(1)
plot(st_geometry(small_boi))
which(matches > 0)
table(matches)
which.max(matches)
fired_events[1426, ]
out[[1426]]
range(out[[i]]$diffdist_fpa_fired)
plot(sf::st_geometry(fired_events[i, ]))
plot(sf::st_geometry(out[[i]]), add = TRUE, col = "red", pch = 19)
plot(sf::st_geometry(out[[i]][1, ]), add = TRUE, col = "blue", pch = 19)


test <- modis_ca_fires %>% 
  dplyr::filter(abs(difftime(DISCOVERY_DATE, fired_events$ig_date[i], units = "day")) <= 45)

test$MTBS_FIRE_NAME
