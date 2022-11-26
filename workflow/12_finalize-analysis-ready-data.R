# Combine all drivers info so they are analysis ready
library(dplyr)
library(sf)
library(tidyr)
library(data.table)
library(tidyselect)


weather_drivers <- 
  data.table::fread("data/out/drivers/weather-drivers-as-percentiles.csv") |>
  dplyr::select(!tidyselect::contains("era5")) |>
  dplyr::select(-samp_id,
                -bi, -erc, -fm100, -fm1000, -pdsi, 
                -wind_terrain_alignment_rtma, -min_wind_terrain_alignment_rtma, -max_wind_terrain_alignment_rtma,
                -tidyselect::starts_with("spi"), -tidyselect::starts_with("eddi"))

other_fires_summary <- 
  data.table::fread("data/out/drivers/other-fires-summary.csv") |>
  dplyr::select(-npl_at_ignition, -cumu_count, -cumu_area_ha)

fluc_static <- 
  data.table::fread("data/out/drivers/fluc-static-driver-proportion-percentiles.csv") |>
  dplyr::select(!c(peak_ridge_warm, peak_ridge, peak_ridge_cool, mountain_divide, cliff, 
                   upper_slope_warm, upper_slope, upper_slope_cool, upper_slope_flat, 
                   lower_slope_warm, lower_slope, lower_slope_cool, lower_slope_flat,
                   valley_original, valley_narrow,
                   proj_area, 
                   trees_original_tm01, shrubs_trees_mix_tm01, grass_forb_herb_trees_mix_tm01, barren_trees_mix_tm01,
                   shrubs_original_tm01, grass_forb_herb_shrub_mix_tm01, barren_tm01, snow_or_ice_tm01)) |>
  dplyr::select(!tidyselect::contains("tm00")) |>
  dplyr::select(!tidyselect::contains("stable")) |>
  dplyr::select(!tidyselect::contains("slow_loss")) |>
  dplyr::select(!tidyselect::contains("fast_loss")) |>
  dplyr::select(!tidyselect::contains("gain"))

# 2020 Creek Fire
# landfire[id == 135921, ]
# Use full 10 year disturbance history for more lumping
# |>
#  dplyr::select(did, id, date, 
#                fire_high_tm01_tm10,
#                fire_tm01_tm10,
#                clearcut_harvest_othermech_tm01_tm10,
#                fuel_trt_tm01_tm10,
#                insect_disease_high_tm01_tm10, 
#                insect_disease_tm01_tm10)

landfire <- 
  data.table::fread("data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv") |>
  dplyr::select(did, id, date,
                fire_high_tm01_tm05, fire_high_tm06_tm10,
                fire_not_high_tm01_tm05, fire_not_high_tm06_tm10,
                clearcut_harvest_othermech_tm01_tm05, clearcut_harvest_othermech_tm06_tm10,
                fuel_trt_tm01_tm05, fuel_trt_tm06_tm10,
                insect_disease_high_tm01_tm05, insect_disease_high_tm06_tm10,
                insect_disease_not_high_tm01_tm05, insect_disease_not_high_tm06_tm10)

# Remove fires that never reached more than 121 hectares (300 acres)
target_event_ids <-
  sf::read_sf("data/out/fired/02_time-filter-crs-transform/fired_events_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>% 
  dplyr::filter(area_ha >= 121.406) %>% 
  dplyr::pull(id)

drivers <- 
  merge(x = weather_drivers, y = other_fires_summary, by = c("did", "id", "date"), all = TRUE) |>
  merge(y = fluc_static, by = c("did", "id", "date"), all = TRUE) |>
  merge(y = landfire, by = c("did", "id", "date"), all = TRUE) |>
  dplyr::filter(id %in% target_event_ids) |>
  dplyr::filter(date >= as.Date("2011-01-01"))

# For defining "ewe" or not, what is the percentage threshold? E.g., 0.95 means an "ewe" is in the top
# 5th percentile for daily area of increase
pct_threshold <- 0.95

# Defining "ewe" as whether daily area of increase was >95th percentile for the biome
biome_lookup <- 
  tibble::tibble(biome_name_daily = c("Temperate Conifer Forests", 
                                      "Mediterranean Forests, Woodlands & Scrub", 
                                      "Temperate Grasslands, Savannas & Shrublands", 
                                      "Deserts & Xeric Shrublands"),
                 biome_shortname = c("tcf", "mfws", "tgss", "dxs"))

fires <-
  data.table::fread("data/out/fired/05_daily-with-behavior-metrics/fired_daily_ca_behavior-metrics.csv") |>
  dplyr::select(did, id, date, daily_area_ha, biome_name_daily) |>
  dplyr::mutate(area_log10 = log10(daily_area_ha)) |>
  dplyr::group_by(biome_name_daily) %>% 
  dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10),
                ewe = ifelse(area_log10_pct >= pct_threshold, yes = 1, no = 0)) |> 
  dplyr::ungroup() |>
  dplyr::left_join(biome_lookup)

# 95th percentile growth for temperate conifer forests is 1557 ha per day
# 95th percentile growth for Mediterranean Forest, Woodland & Scrub is 2973 ha per day
# 95th percentile growth for temperate grasslands, savannas & shrublands is 173 ha per day
# 95th percentile growth for desert & xeric shrublands is 970 ha per day
fires %>%
  filter(ewe == 1) %>%
  group_by(biome_name_daily) %>%
  filter(daily_area_ha == min(daily_area_ha)) %>%
  select(biome_name_daily, daily_area_ha)

out <-
  merge(drivers, fires, by = c("did", "id", "date"))


data.table::fwrite(x = fires_all, file = "data/ard/daily-drivers-of-california-megafires.csv")

out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(insect_disease_high_tm01_tm05))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(insect_disease_high_tm06_tm10))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(insect_disease_tm01_tm05))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(insect_disease_tm06_tm10))


#fire
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(fire_high_tm01_tm05))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(fire_high_tm06_tm10))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(fire_tm01_tm05))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(fire_tm06_tm10))

# clearcut_harvest_othermech
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(clearcut_harvest_othermech_tm01_tm05))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(clearcut_harvest_othermech_tm06_tm10))

#
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(fuel_trt_tm01_tm05))
out %>% 
  group_by(biome_shortname, ewe) %>% 
  summarize(mean(fuel_trt_tm06_tm10))

