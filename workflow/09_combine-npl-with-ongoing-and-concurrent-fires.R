# assemble daily-scale drivers

library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(pbapply)
library(USAboundaries)

#### --- input FIRED data
fired_daily_response <- 
  data.table::fread("data/out/fired_daily_ca_response-vars.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(date))

# We need the ignition date for each event, so we go back to some of the original output from the
# FIRED algorithm
fired_events <- 
  sf::st_read("data/out/fired_events_ca.gpkg") %>% 
  dplyr::select(id, ignition_date, last_date, ignition_year, ignition_month, ignition_day) %>% 
  sf::st_drop_geometry()

fired_daily <- 
  sf::st_read("data/out/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  sf::st_set_agr(value = "constant") %>% 
  dplyr::left_join(fired_daily_response, by = c("did", "id", "date", "samp_id")) %>% 
  dplyr::left_join(fired_events, by = "id") %>% 
  dplyr::rename(act_aoi = daily_area_ha,
                ig_year = ignition_year,
                ig_month = ignition_month,
                ig_day = ignition_day,
                ig_date = ignition_date,
                c_area_tm1 = cum_area_ha_tminus1)
  
npl <- 
  read.csv("data/out/national-preparedness-level.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste0(year, "-", month, "-", day))) %>% 
  dplyr::select(-c(year, month, day)) %>% 
  as.data.table()


# get some of the human factors in tidy shape
# Took 48 minutes; would have taken ~3 hours with a simple for loop
# start_time <- Sys.time()
other_fires <- function(DT, target_fire) {
  DT[, difftime_days := difftime(DT$date, target_fire$date, units = "days")]
  
  # number of concurrent fires for the target_fire (a fire/day combo) is the number of fires with
  # an ignition date before or equal to the day of interest and a last date after or
  # equal to the day of interest; AND that isn't the target fire itself
  concurrent_fires <- DT[id != target_fire$id & ig_date <= target_fire$date & last_date >= target_fire$date & difftime_days <= 0]
  if (nrow(concurrent_fires) == 0) {
    concurrent_fires <- 0
  } else {
    concurrent_fires <- nrow(concurrent_fires[, .SD[difftime_days == max(difftime_days, na.rm = TRUE)], , by = .(id)])
  }
  
  # number of events to date for the target_fire (a fire/day combo) is the events with
  # an ignition date in the same year but prior to the target day and with an id
  # that isn't the target fire's ID. We also subset to just fire/day combos where the difference in the 
  # target fire/day date and the other fires/days is less than 0 so we can filter and get the cumulative
  # area of the fires that meet these criteria 
  this_year_events_to_date <- DT[id != target_fire$id & ig_date < target_fire$date & ig_year == target_fire$ig_year & difftime_days < 0]
  this_year_events_to_date[, cumu_area_ha := c_area_tm1 + act_aoi]
  if (nrow(this_year_events_to_date) == 0) {
    cumu_count <- 0
    cumu_area_ha <- 0
  } else {
    this_year_events_to_date <- this_year_events_to_date[, .SD[difftime_days == max(difftime_days, na.rm = TRUE)], , by = .(id)]
    cumu_area_ha <- sum(this_year_events_to_date$cumu_area_ha)
    cumu_count <- nrow(this_year_events_to_date)
  }
  
  return(list(concurrent_fires, cumu_count, cumu_area_ha))
  
}

other_fires_summary <-
  fired_daily %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(did, id, date, ig_year, ig_date, last_date, c_area_tm1, act_aoi) %>% 
  as.data.table()

other_fires_summary[, c("concurrent_fires", "cumu_count", "cumu_area_ha") := other_fires(DT = data.table::copy(other_fires_summary), target_fire = .SD), by = seq_len(NROW(other_fires_summary))]

# join with the NPL date matching the ignition date to see what the NPL was on the day of ignition
npl[, `:=`(ig_date = date, date = NULL)]
other_fires_summary <- npl[other_fires_summary, on = "ig_date"]
other_fires_summary[, `:=`(npl_at_ignition = npl, npl = NULL)]

# join with the NPL date matching the actual date of the fire/day combo to see what the NPL was on that day
npl[, `:=`(date = ig_date, ig_date = NULL)]
other_fires_summary <- npl[other_fires_summary, on = "date"]
other_fires_summary[, c("ig_date", "ig_year", "last_date", "c_area_tm1", "act_aoi") := NULL]
other_fires_summary <- dplyr::select(other_fires_summary, did, id, date, npl, npl_at_ignition, everything())

write.csv(x = other_fires_summary, file = "data/out/other-fires-summary.csv", row.names = FALSE)

# end_time <- Sys.time()
# print(difftime(end_time, start_time, units = "mins"))

