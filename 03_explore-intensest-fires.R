# Get daily FRP for California FIRED data

library(sf)
library(dplyr)
library(pbapply)

fired_ca <- sf::st_read("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
modis_afd <- sf::st_read("data/raw/DL_FIRE_M6_184262/fire_archive_M6_184262.shp") %>% sf::st_transform(sf::st_crs(fired_ca))

fired_list <-
  fired_ca %>% 
  group_by(date) %>% 
  group_split()

spacetime_join <- function(fired) {
  this_date <- unique(fired$date)
  afd_on_date <- filter(modis_afd, ACQ_DATE == this_date)
  
  afd_within_fired <- sf::st_intersection(x = fired, y = afd_on_date)
  fired_with_afd <- sf::st_join(fired, afd_on_date)
  
  return(fired_with_afd)
}

test <- fired_list[[3]]

test_joined <- spacetime_join(test)

glimpse(test_joined)
