library(sf)
library(dplyr)
library(USAboundaries)

ca <- USAboundaries::us_states(resolution = "high", states = "California")

frap <- sf::st_read("data/out/frap-during-modis-record.gpkg")
fired_big <- sf::st_read("data/out/fired_events_conus_nov2001-jan2019_california_biggest-expansions.gpkg")

out <- vector(mode = "list", length = nrow(fired_big))

for (i in 1:nrow(fired_big)) {
  print(i)
  this_fired <- fired_big[i, ]
  this_year <- fired_big$ignition_year[i]
  this_year_frap <- frap[frap$YEAR_ == this_year, ]
  
  intersections <- st_difference(x = this_fired, y = this_year_frap)
  
  best_fit <- 
    intersections %>% 
    filter(st_area(.) == max(st_area(.)))
  
  out[i] <- best_fit
}

any(frap$FIRE_NAME == "BLUE")
frap[frap$FIRE_NAME == "BLUE", ]

blue <- 
  frap %>% 
  filter(FIRE_NAME == "OBSERVATION") %>% 
  arrange(desc(GIS_ACRES))

ggplot() + 
  geom_sf(data = this_year_frap) + 
  geom_sf(data = ca, fill = NA) + 
  geom_sf(data = this_fired, fill = "red") +
  geom_sf(data = blue, fill = "blue")
