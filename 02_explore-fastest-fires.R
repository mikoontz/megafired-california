library(ggplot2)
library(sf)

california_fired <- sf::read_sf("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
california_fired$daily_area_ha <- california_fired$daily_area_km2 * 100

ggplot(california_fired, aes(x = daily_area_ha)) +
  geom_histogram(bins = 100) +
  scale_x_log10()

ha_per_day <- california_fired$daily_area_km2 * 100

california_fired[order(california_fired$daily_area_ha, decreasing = TRUE), c("date", "ignition_date", "id", "total_area_km2", "daily_area_ha")]

big_boi <- california_fired[california_fired$id == 9810, ]

plot(st_transform(big_boi[, "date"], 3310))
ecdf(ha_per_day)(10000)
sort(california_fired$daily_area_ha, decreasing = TRUE)
plot(california_fired$geom)

