# Preliminary survival analysis

library(sf)
library(dplyr)
library(data.table)

# Fire Size Classes

# https://www.nwcg.gov/term/glossary/size-class-of-fire
# Class A - one-fourth acre or less;
# Class B - more than one-fourth acre, but less than 10 acres;
# Class C - 10 acres or more, but less than 100 acres;
# Class D - 100 acres or more, but less than 300 acres;
# Class E - 300 acres or more, but less than 1,000 acres;
# Class F - 1,000 acres or more, but less than 5,000 acres;
# Class G - 5,000 acres or more.

fire_size_classes <- tibble(class = LETTERS[1:7], 
                            size_lwr_ac = c(0, 0.25, 10, 100, 300, 1000, 5000),
                            size_upr_ac = c(0.25, 10, 100, 300, 1000, 5000, NA),
                            size_lwr_ha = size_lwr_ac / 2.47105,
                            size_upr_ha = size_upr_ac / 2.47105)

fire_size_classes

fired <- 
  sf::st_read("data/out/fired_daily_ca_ewe_rank.gpkg") %>% 
  dplyr::mutate(daily_area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  group_by(id) %>% 
  mutate(cum_area_ha = cumsum(daily_area_ha)) %>% 
  mutate(cum_area_ha_tminus1 = cum_area_ha - daily_area_ha) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(date >= lubridate::ymd("2003-01-01"))

summary(fired)
range(fired$date)

fired_nonspatial <- sf::st_drop_geometry(fired)

length(unique(fired_nonspatial$id))
fired_nonspatial %>% nrow()

sub <- 
  fired_nonspatial %>% 
  filter(tot_hect >= 405)

length(unique(sub$id))
length(unique(sub$id)) / length(unique(fired_nonspatial$id))

sub %>% nrow()


length(unique(sub$id)) / length(unique(fired_nonspatial$id))
sub %>% nrow() / fired_nonspatial %>% nrow()
