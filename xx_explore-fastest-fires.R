library(ggplot2)
library(sf)

california_fired_daily <- sf::read_sf("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
california_fired <- sf::read_sf("data/raw/fired_events_conus_nov2001-jan2019.gpkg")

california_fired_daily$daily_area_ha <- california_fired_daily$daily_area_km2 * 100

nrow(california_fired_daily)
length(unique(california_fired_daily$id))

ggplot(california_fired_daily, aes(x = daily_area_ha)) +
  geom_histogram(bins = 100) +
  scale_x_log10()

big_days <- california_fired_daily[california_fired_daily$daily_area_ha >= quantile(california_fired_daily$daily_area_ha, probs = 0.995), ]

# What percent of blow up event-days result in over 10,000 hectares per day
# of fire growth?  0.3 %
1 - ecdf(california_fired_daily$daily_area_ha)(10000)

# What is the fire growth threshold for the top 0.5% of blow up event-days?
# 7216 hectares
quantile(california_fired_daily$daily_area_ha, probs = 0.995)

# Percent of all area burned that come during the biggest (top 0.5%) blow up events
# almost 21%!
sum(big_days$daily_area_ha) / sum(california_fired_daily$daily_area_ha)

# how many unique event-days in the top 0.5% of fire growth?
# 78
nrow(big_days)

# how many unique events represented by the top 0.5% of blow up event-days?
# 39
length(unique(big_days$id))

# how many unique days represented by the top 0.5% of blow up event-days?
# 66
length(unique(big_days$date))

# What is the top blowup event? Cedar Fire of 2003.
cedar <- california_fired_daily[california_fired_daily$id == 9810, ]
plot(st_transform(cedar[, "date"], 3310))

# what are the FIRED event IDs represented by the top 0.5% of blow up events?
ids <- unique(big_days$id)

st_write(obj = big_days, dsn = "data/out/fired_conus_daily_nov2001-jan2019_california_biggest-expansions.gpkg", delete_dsn = TRUE)

st_write(obj = california_fired_daily[california_fired_daily$id %in% ids, ], dsn = "data/out/fired_conus_daily_nov2001-jan2019_california_biggest-expansions_whole-event.gpkg", delete_dsn = TRUE)

# Just the final perimeters of the events that had the biggest expansions
california_fired_events <- sf::read_sf("data/out/fired_events_conus_nov2001-jan2019_california.gpkg")

st_write(obj = california_fired_events[california_fired_events$id %in% ids, ], dsn = "data/out/fired_events_conus_nov2001-jan2019_california_biggest-expansions.gpkg", delete_dsn = TRUE)
