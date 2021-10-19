library(dplyr)
library(data.table)
library(broom)
library(ggplot2)
library(ggrepel)
library(pbapply)

# Tidyverse PCA
# https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/

if(!file.exists("data/out/human-drivers-summary.csv")) {
  events <- sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg")
  out <- vector(mode = "list", length = nrow(events))
  
  for (i in 1:nrow(events)) {
    this_event <- events[i, ]
    concurrent_fires <- sum(events$ig_date >= this_event$ig_date & events$ig_date <= this_event$last_date)
    
    this_year_events_todate <-
      events %>% 
      filter(ig_year == this_event$ig_year & 
               id != this_event$id &
               ig_date < this_event$ig_date)
    
    out[[i]] <- data.frame(id = this_event$id, 
                           concurrent = concurrent_fires,
                           cum_count = nrow(this_year_events_todate),
                           cum_area = as.numeric(sum(sf::st_area(this_year_events_todate))))
    
  }
  
  human_factors <- data.table::rbindlist(out)
  
  write.csv(x = human_factors, file = "data/out/human-drivers-summary.csv", row.names = FALSE)
}

human_factors <- read.csv("data/out/human-drivers-summary.csv")

ewe_ranks <- read.csv("data/out/extreme-wildfire-events-ranking.csv")

human_factors_mega <- 
  human_factors %>% 
  dplyr::rename(driver_concurrent_fire_count = concurrent,
                driver_cumulative_fire_count = cum_count,
                driver_cumulative_fire_area = cum_area) %>% 
  dplyr::mutate(driver_cumulative_fire_area = driver_cumulative_fire_area / 1000000)

event_drivers <- data.table::fread("data/out/ee/megaFIRED-event-scale-drivers_california.csv")
daily_drivers <- data.table::fread("data/out/ee/megaFIRED-daily-drivers_california.csv")
hourly_drivers <- data.table::fread("data/out/ee/megaFIRED-hourly-drivers_california.csv")

event_drivers[, `:=`(.geo = NULL,
                     `system:index` = NULL)]
event_drivers <-
  event_drivers %>% 
  dplyr::rename(driver_friction = friction)

daily_drivers[, .geo := NULL]
hourly_drivers[, .geo := NULL]

# "forest_structure_rumple"
# "mean_ndvi" # (multiplied by 100, so divide by 100 to get to usual scale)
# "rumple_index"
# "temperature_2m" # in Kelvin; subtract 273.15 to get Celsius
# "volumetric_soil_water_layer_1"
# "vpd_hPa"
# "wind_aspect_alignment_rad"
# "wind_speed"

top_number <- 10

daily_drivers_summary <-
  daily_drivers %>% 
  dplyr::mutate(rumple_index = surf_area / proj_area) %>%
  group_by(id) %>% 
  dplyr::summarize(driver_forest_structure_rumple = quantile(forest_structure_rumple, probs = 0.9, names = FALSE, na.rm = TRUE),
                   driver_mean_ndvi = quantile(ndvi, probs = 0.9, names = FALSE, na.rm = TRUE),
                   driver_rumple_index = quantile(rumple_index, probs = 0.9, names = FALSE, na.rm = TRUE),
                   driver_pdsi = quantile(pdsi, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei14d = quantile(spei14d, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei30d = quantile(spei30d, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei90d = quantile(spei90d, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei180d = quantile(spei180d, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei270d = quantile(spei270d, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei1y = quantile(spei1y, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei2y = quantile(spei2y, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_spei5y = quantile(spei5y, probs = 0.1, names = FALSE, na.rm = TRUE)) %>% 
  dplyr::ungroup()

hourly_drivers_summary <-
  hourly_drivers %>% 
  group_by(id) %>% 
  summarize(driver_temperature_2m = quantile(temperature_2m - 273.15, probs = 0.9, names = FALSE, na.rm = TRUE),
            driver_volumetric_soil_water_layer_1 = quantile(volumetric_soil_water_layer_1, probs = 0.1, names = FALSE, na.rm = TRUE),
            driver_vpd_hPa_hi = quantile(vpd_hPa, probs = 0.9, names = FALSE, na.rm = TRUE),
            driver_vpd_hPa_lo = quantile(vpd_hPa, probs = 0.1, names = FALSE, na.rm = TRUE),
            driver_wind_aspect_alignment_rad = quantile(cos(2 * wind_aspect_alignment_rad), probs = 0.9, names = FALSE, na.rm = TRUE),
            driver_wind_speed_hi = quantile(wind_speed, probs = 0.9, names = FALSE, na.rm = TRUE),
            driver_wind_speed_lo = quantile(wind_speed, probs = 0.1, names = FALSE, na.rm = TRUE)) %>% 
  dplyr::ungroup()

megafire_drivers <-
  event_drivers %>% 
  left_join(daily_drivers_summary, by = "id") %>% 
  left_join(hourly_drivers_summary, by = "id") %>% 
  dplyr::left_join(ewe_ranks, by = "id") %>% 
  dplyr::left_join(human_factors_mega, by = "id") %>% 
  dplyr::mutate(ewe_top = ifelse(ewe_rank <= top_number, "top", "lower"),
                size_top = ifelse(size_rank <= top_number, "size_top", "size_lower"),
                aoir_top = ifelse(aoir_rank <= top_number, "aoir_top", "aoir_lower"),
                frp_top = ifelse(frp_rank <= top_number, "frp_top", "frp_lower"))

megafire_drivers[is.na(megafire_drivers$driver_wind_aspect_alignment_rad), "driver_wind_aspect_alignment_rad"] <- 0

pca_data <- dplyr::select(megafire_drivers, dplyr::contains("driver_"))

pca <- 
  prcomp(pca_data, center = TRUE, scale = TRUE)

pca_augmented <-
  pca %>% 
  broom::augment(megafire_drivers)

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = ewe_rank)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = size_rank)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = aoir_rank)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(pca_augmented, aes(x = .fittedPC1, y = .fittedPC2, color = frp_rank)) +
  geom_point() +
  scale_color_viridis_c()

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

pca$sdev / sum(pca$sdev)
