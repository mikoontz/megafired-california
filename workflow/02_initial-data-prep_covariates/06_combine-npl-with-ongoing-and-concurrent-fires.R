# assemble daily-scale drivers

library(dplyr)
library(sf)
library(data.table)
library(lubridate)
library(pbapply)
library(tidyr)
library(ggplot2)


#### --- input FIRED data
# We need the ignition date for each event, so we go back to some of the original output from the
# FIRED algorithm
fired_daily <- 
  sf::st_read("data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  sf::st_set_agr(value = "constant") %>%
  dplyr::mutate(aoi = as.numeric(sf::st_area(.)) / 10000) %>% 
  sf::st_drop_geometry()

fired_events <- 
  sf::st_read("data/out/fired/01_spatial-subset/fired_events_ca.gpkg") %>% 
  dplyr::select(id, ignition_date, last_date, ignition_year, ignition_month, ignition_day) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(ignition_date = lubridate::ymd(ignition_date), 
                last_date = lubridate::ymd(last_date)) %>% 
  dplyr::filter(ignition_year <= 2020)

if(!file.exists("data/out/drivers/fired-concurrent-fires-by-day.csv")) {

  fired_events <- as.data.table(fired_events)
  
  fired_events_long_form <- 
    fired_events[, .(id = id,
                     ignition_date = ignition_date,
                     last_date = last_date,
                     ignition_year = ignition_year,
                     date = seq(from = ignition_date, to = last_date, by = "day")), 
                 by = seq_len(nrow(fired_events))]
  
  fired_events_long_form[, new_ignition := as.numeric(ignition_date == date)]
  
  fired_derived_concurrent_fires <- 
    fired_events_long_form %>% 
    dplyr::left_join(fired_daily) %>% 
    tidyr::replace_na(replace = list(aoi = 0)) %>% 
    dplyr::arrange(date) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarize(sum_aoi = sum(aoi),
                     concurrent_fires = length(date),
                     sum_new_ignitions = sum(new_ignition)) %>% 
    tidyr::complete(date = seq(from = lubridate::ymd("2011-01-01"), to = lubridate::ymd("2020-12-31"), by = "day"), 
                    fill = list(concurrent_fires = 0, sum_aoi = 0, sum_new_ignitions = 0)) %>% 
    dplyr::mutate(year = lubridate::year(date),
                  doy = lubridate::yday(date),
                  sum_aoi_tm1 = dplyr::lag(sum_aoi),
                  sum_new_ignitions_tm1 = lag(sum_new_ignitions)) %>% 
    dplyr::arrange(date) %>% 
    dplyr::group_by(year) %>% 
    tidyr::replace_na(replace = list(sum_aoi_tm1 = 0, sum_new_ignitions_tm1 = 0)) %>% 
    dplyr::mutate(cumu_area_ha = cumsum(sum_aoi_tm1),
                  cumu_count = cumsum(sum_new_ignitions_tm1)) %>% 
    dplyr::ungroup()
  
  write.csv(x = fired_derived_concurrent_fires, file = "data/out/drivers/fired-concurrent-fires-by-day.csv", row.names = FALSE)
}

if(!file.exists("data/out/short-fpa-fod_concurrent-fires-by-day.csv")) {
  # We might rather use the Short et al., database to understand the number of concurrent
  # fires on a given day thoughout our study period
  # https://www.fs.usda.gov/rds/archive/catalog/RDS-2013-0009.6
  short <- sf::st_read("data/raw/RDS-2013-0009.6_GPKG/Data/FPA_FOD_20221014.gpkg")
  
  short_ca <-
    short %>% 
    sf::st_drop_geometry() %>% 
    as.data.table()
  
  # Subset to only data in california with both a reported discovery date and containment date
  # and must be in 2011 or later (the period of analysis)
  short_ca <- short_ca[STATE == "CA" & !is.na(DISCOVERY_DATE) & !is.na(CONT_DOY), ]
  short_ca <- short_ca[, c("FOD_ID", "FPA_ID", "FIRE_YEAR", "DISCOVERY_DATE", "CONT_DATE")]
  short_ca[, `:=`(DISCOVERY_DATE = lubridate::mdy(DISCOVERY_DATE), CONT_DATE = lubridate::mdy(CONT_DATE))]
  
  short_ca_long_form <- 
    short_ca[, .(fod_id = FOD_ID,
                 fpa_id = FPA_ID,
                 fire_year = FIRE_YEAR,
                 discovery_date = DISCOVERY_DATE,
                 cont_date = CONT_DATE,
                 active_date = seq(from = DISCOVERY_DATE, to = CONT_DATE, by = "day")), 
             by = seq_len(nrow(short_ca))]
  
  short_concurrent_fires <-
    table(short_ca_long_form$active_date) %>% 
    as.data.frame() %>% 
    setNames(c("date", "concurrent_fires")) %>% 
    dplyr::mutate(date = lubridate::ymd(date)) %>% 
    dplyr::filter(date < lubridate::ymd("2021-01-01")) %>% 
    tidyr::complete(date = seq(from = lubridate::ymd("1992-01-01"), to = lubridate::ymd("2020-12-31"), by = "day"), 
                    fill = list(concurrent_fires = 0))
  
  write.csv(x = short_concurrent_fires, file = "data/out/short-fpa-fod_concurrent-fires-by-day.csv", row.names = FALSE)
}


fired_concurrent_fires <- 
  data.table::fread("data/out/drivers/fired-concurrent-fires-by-day.csv") %>% 
  dplyr::select(date, concurrent_fires, cumu_area_ha, cumu_count) %>% 
  dplyr::arrange(date)

short_concurrent_fires <- 
  data.table::fread("data/out/short-fpa-fod_concurrent-fires-by-day.csv") %>% 
  dplyr::rename(short_concurrent_fires = concurrent_fires) %>% 
  dplyr::arrange(date)

concurrent_fires <-
  fired_concurrent_fires %>% 
  dplyr::full_join(y = short_concurrent_fires) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::arrange(date)

# Get National Preparedness Level data
npl <- 
  read.csv("data/out/drivers/national-preparedness-level.csv") %>% 
  dplyr::mutate(date = lubridate::ymd(paste0(year, "-", month, "-", day))) %>% 
  dplyr::select(-c(year, month, day)) %>% 
  as.data.table()

# join with the NPL date matching the ignition date to see what the NPL was on the day of ignition
other_fires_summary <-
  fired_daily %>% 
  dplyr::filter(id %in% fired_events$id) %>% 
  dplyr::select(-aoi) %>% 
  dplyr::left_join(concurrent_fires) %>% 
  dplyr::left_join(npl)

write.csv(x = other_fires_summary, file = "data/out/drivers/other-fires-summary.csv", row.names = FALSE)
