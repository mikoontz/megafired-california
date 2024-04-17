library(sf)
library(spData)
library(tmap)
library(ceramic)

ca <- USAboundaries::us_states(states = "California", resolution = "high") |> sf::st_transform(3310)
ca_or_nv_id_az <- USAboundaries::us_states(
  states = c("California", "Nevada", "Oregon", "Idaho", "Arizona"), 
  resolution = "high"
) |> 
  sf::st_geometry() |> 
  sf::st_transform(3310)

conus <- USAboundaries::us_states(
  states = NULL, 
  resolution = "high"
) |> 
  dplyr::filter(!(state_name %in% c("Alaska", "Hawaii", "Guam", "Puerto Rico", "Virgin Islands", "American Samoa", "Northern Mariana Islands"))) |> 
  sf::st_transform(5070) |> 
  dplyr::mutate(is_california = state_name == "California")

fired_creek = sf::st_read(here::here('data/out/figure-data/fired_creek_2020-09-07.shp')) |> sf::st_transform(3310)
fi_creek = sf::st_read(here::here('data/out/figure-data/fi_creek_2020-09-07.shp')) |> sf::st_transform(3310)

conus_inset_tm =
  tmap::tm_shape(conus) +
  tmap::tm_borders() +
  tmap::tm_fill(col = "is_california", palette = c("white", "red"), legend.show = FALSE)

conus_inset_tm

basemap <- ceramic::cc_location(loc = fired_creek)
basemap_out <- ceramic::cc_location(loc = sf::st_buffer(fired_creek, dist = 30000))
basemap_ca <- ceramic::cc_location(loc = ca)

png(here::here('figs/creek-geometry.png'), width = )
plot(sf::st_geometry(fired_creek))

tmap::tm_shape(basemap) +
  tmap::tm_rgb() +
  tmap::tm_shape(fired_creek) +
  tmap::tm_fill(col = "red")

tmap::tm_shape(basemap_out) +
  tmap::tm_rgb() +
  tmap::tm_shape(fired_creek) +
  tmap::tm_fill(col = "red")

tmap::tm_shape(basemap_out) +
  tmap::tm_rgb() +
  tmap::tm_shape(fi_creek) +
  tmap::tm_fill(col = "blue") +
  tmap::tm_shape(fired_creek) +
  tmap::tm_fill(col = "red")

tmap::tm_shape(basemap_ca) +
  tmap::tm_rgb() +
  tmap::tm_shape(ca_or_nv_id_az) +
  tmap::tm_borders(col = "black", lwd = 1.25) +
  tmap::tm_shape(fi_creek) +
  tmap::tm_fill(col = "blue") +
  tmap::tm_shape(fired_creek) +
  tmap::tm_fill(col = "red")


print(conus_inset_tm, vp = grid::viewport(x = 0.75, y = 0.8, width = 0.5, height = 0.3))

