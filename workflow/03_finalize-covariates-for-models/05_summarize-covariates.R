library(dplyr)
library(data.table)
library(sf)
library(here)

lf_dist_by_resolve <- 
  data.table::fread(here::here("data", "out", "drivers", "landfire-disturbance", "landfire-disturbance-history-resolve.csv"))
lf_dist_by_resolve[, `:=`(.geo = NULL,
                          `system:index` = NULL)]

static_by_resolve <- 
  data.table::fread(here::here("data", "out", "drivers", "RESOLVE-biomes-static-drivers_california_v4.csv"))
static_by_resolve[, `:=`(.geo = NULL,
                         `system:index` = NULL)]
static_by_resolve <- static_by_resolve[samp_id != -999, ]

fluc_by_resolve <-
  data.table::fread(here::here("data", "out", "drivers", "RESOLVE-biomes-fluctuating-drivers_california_v5.csv"))
fluc_by_resolve[, `:=`(.geo = NULL,
                       `system:index` = NULL)]

roads_by_resolve <-
  data.table::fread(here::here("data", "out", "drivers", "RESOLVE-biomes-daily-roads-drivers_california_v1.csv"))
roads_by_resolve[, `:=`(.geo = NULL,
                        `system:index` = NULL)]

