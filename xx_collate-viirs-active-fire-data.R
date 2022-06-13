library(dplyr)
library(sf)
library(USAboundaries)
library(data.table)
library(lubridate)
library(oce)
library(stringr)

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310)

viirs <- data.table::fread("data/raw/DL_FIRE_SV-C2_275002/fire_archive_SV-C2_275002.csv", colClasses = c(acq_time = "character"))
viirs[, acq_datetime := lubridate::ymd_hm(paste(acq_date, acq_time), tz = "UTC")]
# What are the time zone names?
# base::OlsonNames()

viirs[, local_datetime := lubridate::with_tz(time = acq_datetime, tzone = "US/Pacific")]
viirs[, solar_ang := oce::sunAngle(t = acq_datetime,
                                   longitude = longitude,
                                   latitude = latitude,
                                   useRefraction = FALSE)$altitude]

viirs[, dn_detect := ifelse(solar_ang >= 0, yes = "day", no = "night")]

viirs <- 
  viirs %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_transform(crs = 3310)

viirs_ca <-
  viirs %>% 
  sf::st_intersection(sf::st_geometry(ca))

viirs_ca_df <-
  viirs_ca %>% 
  sf::st_drop_geometry()

viirs_ca_df[, `:=`(local_year = lubridate::year(local_datetime), 
                   local_month = stringr::str_pad(string = lubridate::month(local_datetime), width = 2, side = "left", pad = "0"),
                   local_day = stringr::str_pad(string = lubridate::hour(local_datetime), width = 2, side = "left", pad = "0"),
                   local_hour = stringr::str_pad(string = lubridate::hour(local_datetime), width = 2, side = "left", pad = "0"),
                   local_minute = stringr::str_pad(string = lubridate::minute(local_datetime), width = 2, side = "left", pad = "0"))]
viirs_ca_df[, `:=`(local_date = paste(local_year, local_month, local_day, sep = "-"),
                   local_time = paste0(local_hour, local_minute))]

data.table::fwrite(x = viirs_ca_df, file = "data/out/viirs_suomi_ca.csv")

