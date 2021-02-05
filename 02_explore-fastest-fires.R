library(ggplot2)
library(sf)

california_fired <- sf::read_sf("data/out/fired_conus_daily_nov2001-jan2019_california.gpkg")
california_fired$daily_area_ha <- california_fired$daily_area_km2 * 100

nrow(california_fired)
length(unique(california_fired$id))

ggplot(california_fired, aes(x = daily_area_ha)) +
  geom_histogram(bins = 100) +
  scale_x_log10()

ha_per_day <- california_fired$daily_area_km2 * 100

california_fired[order(california_fired$daily_area_ha, decreasing = TRUE), c("date", "ignition_date", "id", "total_area_km2", "daily_area_ha")]

big_boi <- california_fired[california_fired$id == 9810, ]

plot(st_transform(big_boi[, "date"], 3310))
1 - ecdf(california_fired$daily_area_ha)(10000)

quantile(california_fired$daily_area_ha, probs = 0.995)

big_days <- california_fired[california_fired$daily_area_ha >= quantile(california_fired$daily_area_ha, probs = 0.995), ]

# Percent of all area burned that come during the biggest (top 0.5%) blow up events
# almost 21%!
sum(big_days$daily_area_ha) / sum(california_fired$daily_area_ha)

length(unique(big_days$id))

sort(california_fired$daily_area_ha, decreasing = TRUE)
plot(california_fired$geom)

