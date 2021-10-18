library(dplyr)
library(data.table)
library(broom)
library(ggplot2)
library(ggrepel)
library(pbapply)

# Tidyverse PCA
# https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
events <- sf::st_read("data/out/fired_events_ca.gpkg")

out <- vector(mode = "list", length = nrow(events))

for (i in 1:nrow(events)) {
  this_event <- events[i, ]
  concurrent_fires <- sum(events$ignition_date >= this_event$ignition_date & events$ignition_date <= this_event$last_date)
  
  this_year_events_todate <-
    events %>% 
    filter(ignition_year == this_event$ignition_year & 
             id != this_event$id &
             ignition_date < this_event$ignition_date)
  
  out[[i]] <- data.frame(id = this_event$id, 
                         concurrent_fire_count = concurrent_fires,
                         cumulative_fire_count = nrow(this_year_events_todate),
                         cumulative_fire_area = as.numeric(sum(sf::st_area(this_year_events_todate))))
  
}

human_factors <- data.table::rbindlist(out)
human_factors_mega <- 
  human_factors %>% 
  dplyr::rename(id = this_event.id,
                driver_concurrent_fire_count = concurrent_fire_count,
                driver_cumulative_fire_count = cumulative_fire_count,
                driver_cumulative_fire_area = cumulative_fire_area) %>% 
  dplyr::mutate(driver_cumulative_fire_area = driver_cumulative_fire_area / 1000000)

hourly_drivers <- 
  data.table::fread("data/raw/FIRED-megafires-drivers_california.csv")

hourly_drivers[, .geo := NULL]

"forest_structure_rumple"
"mean_ndvi" # (multiplied by 100, so divide by 100 to get to usual scale)
"rumple_index"
"temperature_2m" # in Kelvin; subtract 273.15 to get Celsius
"volumetric_soil_water_layer_1"
"vpd_hPa"
"wind_aspect_alignment_rad"
"wind_speed"

top_number <- 10

event_drivers <-
  hourly_drivers %>% 
  dplyr::mutate(rumple_index = surf_area / proj_area) %>% 
  group_by(id, ewe_rank, size_rank, aoir_rank, frp_rank) %>% 
  summarize(driver_forest_structure_rumple = unique(forest_structure_rumple),
            driver_mean_ndvi = unique(mean_ndvi),
            driver_rumple_index = unique(rumple_index),
            driver_temperature_2m = quantile(temperature_2m - 273.15, probs = 0.9, names = FALSE),
            driver_volumetric_soil_water_layer_1 = quantile(volumetric_soil_water_layer_1, probs = 0.1, names = FALSE),
            driver_vpd_hPa_hi = quantile(vpd_hPa, probs = 0.9, names = FALSE),
            driver_vpd_hPa_lo = quantile(vpd_hPa, probs = 0.1, names = FALSE),
            driver_wind_aspect_alignment_rad = quantile(cos(2 * wind_aspect_alignment_rad), probs = 0.9, names = FALSE, na.rm = TRUE),
            driver_wind_speed_hi = quantile(wind_speed, probs = 0.9, names = FALSE),
            driver_wind_speed_lo = quantile(wind_speed, probs = 0.1, names = FALSE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ewe_top = ifelse(ewe_rank <= top_number, "top", "lower"),
                size_top = ifelse(size_rank <= top_number, "size_top", "size_lower"),
                aoir_top = ifelse(aoir_rank <= top_number, "aoir_top", "aoir_lower"),
                frp_top = ifelse(frp_rank <= top_number, "frp_top", "frp_lower")) %>% 
  dplyr::left_join(human_factors_mega, by = "id")


event_drivers[is.na(event_drivers$driver_wind_aspect_alignment_rad), "driver_wind_aspect_alignment_rad"] <- 0

pca_data <- dplyr::select(event_drivers, dplyr::contains("driver_"))

pca <- 
  prcomp(pca_data, center = TRUE, scale = TRUE)

pca_augmented <-
  pca %>% 
  broom::augment(event_drivers)

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = ewe_top)) +
  geom_point()

pca_rotation <-
  pca %>%
  broom::tidy(matrix = "rotation") %>%
  tidyr::pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
ggplot(pca_rotation, aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text_repel(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-0.5, 0.5) + ylim(-0.5, 0.5) +
  coord_fixed() # fix aspect ratio to 1:1


