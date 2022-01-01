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
                            size_lwr_ha = 2.47105 * size_lwr_ac,
                            size_upr_ha = 2.47105 * size_upr_ac)

fire_size_classes

fired <- 
  sf::st_read("data/out/fired_daily_ca_ewe_rank.gpkg") %>% 
  dplyr::mutate(daily_area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  group_by(id) %>% 
  mutate(cum_area_ha = cumsum(daily_area_ha)) %>% 
  mutate(cum_area_ha_tminus1 = cum_area_ha - daily_area_ha) %>% 
  dplyr::ungroup()

