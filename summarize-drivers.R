library(dplyr)
library(data.table)
library(broom)
library(ggplot2)
library(ggrepel)
library(pbapply)
library(lubridate)

# Tidyverse PCA
# https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
npl <- 
  read.csv("data/out/national-preparedness-level.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste0(year, "-", month, "-", day)))

if(!file.exists("data/out/human-drivers-summary.csv")) {
  events <- 
    sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>% 
    sf::st_make_valid() %>% 
    dplyr::filter(ig_year <= 2020)
  
  out <- vector(mode = "list", length = nrow(events))
  
  for (i in 1:nrow(events)) {
    this_event <- events[i, ]
    concurrent_fires <- sum(events$ig_date >= this_event$ig_date & events$ig_date <= this_event$last_date)
    
    this_year_events_todate <-
      events %>% 
      filter(ig_year == this_event$ig_year & 
               id != this_event$id &
               ig_date < this_event$ig_date)
    
    starting_npl <- npl[npl$date == this_event$ig_date, "npl"]
    npl[npl$date == this_event$ig_date, "npl"]
    
    mean_npl <- 
      npl %>% 
      dplyr::filter(date >= this_event$ig_date & date <= this_event$last_date) %>% 
      dplyr::summarize(npl = mean(npl)) %>% 
      as.numeric()
    
    out[[i]] <- data.frame(id = this_event$id, 
                           concurrent = concurrent_fires,
                           cum_count = nrow(this_year_events_todate),
                           cum_area = as.numeric(sum(sf::st_area(this_year_events_todate))),
                           starting_npl = starting_npl,
                           mean_npl = mean_npl)
    
  }
  
  human_factors <- data.table::rbindlist(out)
  
  write.csv(x = human_factors, file = "data/out/human-drivers-summary.csv", row.names = FALSE)
}

human_factors <- read.csv("data/out/human-drivers-summary.csv")

ewe_ranks <- read.csv("data/out/extreme-wildfire-events-ranking.csv")

human_factors_mega <- 
  human_factors %>% 
  dplyr::rename(driver_starting_npl = starting_npl,
                driver_mean_npl = mean_npl,
                driver_concurrent_fire_count = concurrent,
                driver_cumulative_fire_count = cum_count,
                driver_cumulative_fire_area = cum_area) %>% 
  dplyr::mutate(driver_cumulative_fire_area = driver_cumulative_fire_area / 1000000)

# event_drivers <- data.table::fread("data/out/ee/megaFIRED-event-scale-drivers_california.csv")
# daily_drivers <- data.table::fread("data/out/ee/megaFIRED-daily-drivers_california.csv")
# hourly_drivers <- data.table::fread("data/out/ee/megaFIRED-hourly-drivers_california.csv")
event_drivers <- data.table::fread("data/out/ee/FIRED-event-scale-drivers_california.csv")
hourly_drivers <- data.table::fread("data/out/ee/FIRED-hourly-drivers_california.csv")

daily_drivers_list <- 
  list.files("data/out/ee/", 
             pattern = "FIRED-daily-drivers_california_", 
             full.names = TRUE) %>% 
  lapply(FUN = fread)

names_of_incomplete_vars <- names(daily_drivers_list[[1]])[which(!(names(daily_drivers_list[[1]]) %in% names(daily_drivers_list[[19]])))]

lapply(daily_drivers_list[-19], FUN = function(x) {
  data.table::set(x = x, j = names_of_incomplete_vars, value = NULL)
})

daily_drivers <- data.table::rbindlist(daily_drivers_list)

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
                   driver_erc = quantile(erc, probs = 0.9, names = FALSE, na.rm = TRUE),
                   driver_fm100 = quantile(fm100, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_fm1000 = quantile(fm1000, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_bi = quantile(bi, probs = 0.9, names = FALSE, na.rm = TRUE),
                   driver_pdsi = quantile(pdsi, probs = 0.1, names = FALSE, na.rm = TRUE),
                   driver_pdsi_z = quantile(z, probs = 0.1, names = FALSE, na.rm = TRUE),
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
                frp_top = ifelse(frp_rank <= top_number, "frp_top", "frp_lower")) %>% 
  dplyr::filter(ignition_year != 2021) %>% 
  dplyr::mutate(idx = 1:nrow(.))


# # Human
# friction
# concurrent_fire_count        
# cumulative_fire_count
# cumulative_fire_area         
# starting_npl
# mean_npl
# 
# # Fuel
# forest_structure_rumple      
# mean_ndvi    
# 
# # Weather
# pdsi                         
# pdsi_z                       
# spei14d                       
# spei30d                      
# spei90d                       
# spei180d                     
# spei270d                      
# spei1y                       
# spei2y                        
# spei5y                       
# temperature_2m                
# vpd_hPa_hi  
# vpd_hPa_lo                   
# wind_speed_hi     
# wind_speed_lo        
# 
# # Topography
# rumple_index                 
# 
# # Integrated
# erc               
# fm100                        
# fm1000                     
# bi                        
# volumetric_soil_water_layer_1
# wind_aspect_alignment_rad     