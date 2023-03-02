library(dplyr)
library(data.table)

convert_proportions_to_percentiles <- function(fired_drivers, fi_drivers, out_fname = NULL) {
  
  # data.table wide to long format reshaping (equivalent to tidyr::tidy_longer)
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
  fi_drivers_long <- data.table::melt(data = fi_drivers, id.vars = c("did", "date", "id"))
  
  # data.table nesting (equivalent to tidyr::nest)
  # https://stackoverflow.com/questions/25430986/create-nested-data-tables-by-collapsing-rows-into-new-data-tables
  fi_drivers_long <- fi_drivers_long[, list(data = list(.SD), 
                                            expected_value = mean(value, na.rm = TRUE),
                                            expected_value_median = median(value, na.rm = TRUE)), 
                                     by = c("did", "id", "date", "variable")]  
  
  # Just pivot the fired drivers from wide to long using data.table::melt(); no need
  # to also nest (because it's only one value-- the one that will be compared against the
  # ~500 nested values from the fire-independent locations)
  fired_drivers_long <- data.table::melt(data = fired_drivers, id.vars = c("did", "date", "id"))
  
  # merge fire-independent drivers data with the fired drivers data
  fires_and_fi <- merge(x = fi_drivers_long, 
                        y = fired_drivers_long, 
                        on = c("did", "date", "id", "driver"))
  
  # We will calculate the difference between raw measured value and expected value
  fires_and_fi[, `:=`(diff = value - expected_value,
                      diff_median = value - expected_value_median)]

    # convert raw proportional cover values to percentiles based on comparing measured value within the
  # fire footprint to the measured value if you were to re-locate that fire footprint to 500 other 
  # locations within the biome
  # https://stackoverflow.com/questions/49939936/how-to-do-operations-on-list-columns-in-an-r-data-table-to-output-another-list-c
  # Old versions used empirical cumulative distribution function of fire-independent
  # measures and evaluated at the measurement within actual fire polygon
  # Pr(X <= x)
  # fires_and_fi[, adj := mapply(data, value, 
  #                              FUN = function(fi_value, fire_value) {
  #                                ecdf(fi_value$value)(fire_value)
  #                              })]
  
  # But really we care about the percentile rank
  fires_and_fi[, adj := mapply(data, value,
                               FUN = function(fi_value, fire_value) {
                                 vec <- c(fire_value, fi_value$value)
                                 return((data.table::frank(vec) / length(vec))[1])
                               })]
  
  names(fires_and_fi)[names(fires_and_fi) == "value"] <- "measured_value"
  
  # drop the data column representing the proportions at the ~500 fire-independent locations
  fires_and_fi[, data := NULL]
  
  fires_and_fi_wide <- data.table::dcast(data = fires_and_fi, formula = did + id + date ~ variable, value.var = "adj")
  
  if (!is.null(out_fname)) {
    data.table::fwrite(x = fires_and_fi_wide, file = out_fname)
    data.table::fwrite(x = fires_and_fi, file = gsub(x = out_fname, pattern = ".csv", replacement = "_long.csv"))
    return(NULL)
  } else {
    return(fires_and_fi)
  }
}

### Convert fluctuating and static driver proportions from Earth Engine to percentiles
# read data
static_fluc_fired_drivers <-data.table::fread("data/out/drivers/fired-fluc-static-driver-proportions.csv")
static_fluc_fi_drivers <- data.table::fread("data/out/drivers/fi-fluc-static-driver-proportions.csv")

# modify any columns as necessary
static_fluc_fi_drivers[, `:=`(date = as.Date(date))]
static_fluc_fired_drivers[, `:=`(date = as.Date(date))]

# drop unnecessary columns
keep_cols <- c("did", "id", "date",
               "elevation", "rumple_index",
               "caltrans_road_density_mpha",
               "ndvi", "veg_structure_rumple", 
               "peak_ridge_cliff", "valleys", "slope_warm", "slope_cool", "slope_neutral", "flat", 
               "trees_tm01", "shrubs_tm01", "grass_forb_herb_tm01", "barren_tm01",
               "landform_diversity", "landcover_diversity_tm01")

static_fluc_fi_drivers <- static_fluc_fi_drivers[, .SD, .SDcols = keep_cols]
static_fluc_fired_drivers <- static_fluc_fired_drivers[, .SD, .SDcols = keep_cols]

# convert to percentiles
convert_proportions_to_percentiles(fired_drivers = static_fluc_fired_drivers,
                                   fi_drivers = static_fluc_fi_drivers,
                                   out_fname = "data/out/drivers/fluc-static-driver-proportion-percentiles.csv")

### LANDFIRE disturbance
gc()

# Read data
lf_fi_drivers <- 
  lapply(list.files("data/out/drivers/landfire-disturbance/fire-independent-locations/", full.names = TRUE), 
         FUN = data.table::fread) |> 
  data.table::rbindlist(fill = TRUE)

lf_fired_drivers <- data.table::fread("data/out/drivers/landfire-disturbance/fired_daily_disturbance-drivers_v1.csv")

# Some NAs seem to still be a part of these data; not sure why since the following line was run after extracting the 
# raw proportions. Run it again here
data.table::setnafill(x = lf_fi_drivers, type = "const", fill = 0, cols = names(lf_fi_drivers)[!(names(lf_fi_drivers) %in% c("did", "id", "date", "samp_id"))])
data.table::setnafill(x = lf_fired_drivers, type = "const", fill = 0, cols = names(lf_fired_drivers)[!(names(lf_fired_drivers) %in% c("did", "id", "date", "samp_id"))])

# modify any columns as necessary
lf_fi_drivers[, `:=`(date = as.Date(date),
                     samp_id = NULL,
                     fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
                     fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10)]

lf_fired_drivers[, `:=`(date = as.Date(date),
                        samp_id = NULL,
                        fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
                        fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10)]

# drop any unnecessary columns
keep_cols <- c("did", "id", "date",
               "fire_high_tm01_tm05", "fire_high_tm06_tm10",
               "fire_not_high_tm01_tm05", "fire_not_high_tm06_tm10",
               "insect_disease_tm01_tm10")

# From late December 2022 run, where we used too many sparely represented categories
# # modify any columns as necessary
# lf_fi_drivers[, `:=`(date = as.Date(date),
#                      samp_id = NULL,
#                      fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
#                      fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10,
#                      insect_disease_not_high_tm01_tm05 = insect_disease_tm01_tm05 - insect_disease_high_tm01_tm05,
#                      insect_disease_not_high_tm06_tm10 = insect_disease_tm06_tm10 - insect_disease_high_tm06_tm10)]
# 
# lf_fired_drivers[, `:=`(date = as.Date(date),
#                         samp_id = NULL,
#                         fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
#                         fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10,
#                         insect_disease_not_high_tm01_tm05 = insect_disease_tm01_tm05 - insect_disease_high_tm01_tm05,
#                         insect_disease_not_high_tm06_tm10 = insect_disease_tm06_tm10 - insect_disease_high_tm06_tm10)]
# 
# # drop any unnecessary columns
# keep_cols <- c("did", "id", "date",
#                "fire_high_tm01_tm05", "fire_high_tm06_tm10",
#                "fire_not_high_tm01_tm05", "fire_not_high_tm06_tm10",
#                "clearcut_harvest_othermech_tm01_tm05", "clearcut_harvest_othermech_tm06_tm10",
#                "fuel_trt_tm01_tm05", "fuel_trt_tm06_tm10",
#                "insect_disease_high_tm01_tm05", "insect_disease_high_tm06_tm10",
#                "insect_disease_not_high_tm01_tm05", "insect_disease_not_high_tm06_tm10")

# We also subset to just the dids that are in the static_fluc drivers variables because
# those have been properly subset to remove nonwater area mismatches between
# the FIRED perimeters and the fire-independent perimeters
lf_fi_drivers <- lf_fi_drivers[, .SD, .SDcols = keep_cols]
lf_fi_drivers <- lf_fi_drivers[did %in% static_fluc_fired_drivers$did, ]

lf_fired_drivers <- lf_fired_drivers[, .SD, .SDcols = keep_cols]
lf_fired_drivers <- lf_fired_drivers[did %in% static_fluc_fired_drivers$did, ]

convert_proportions_to_percentiles(fi_drivers = lf_fi_drivers,
                                   fired_drivers = lf_fired_drivers, 
                                   out_fname = "data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv")

# ### Convert roads data to percentiles
# # read data
# roads_fired_drivers <-data.table::fread("data/out/drivers/roads/fired_daily_road-drivers_v1.csv")
# roads_fi_drivers <- 
#   list.files(path = "data/out/drivers/roads/fire-independent-locations/", 
#              pattern = ".csv", 
#              full.names = TRUE) %>% 
#   lapply(FUN = data.table::fread) %>% 
#   data.table::rbindlist()
# 
# # modify any columns as necessary
# roads_fi_drivers[, `:=`(date = as.Date(date))]
# roads_fired_drivers[, `:=`(date = as.Date(date))]
# 
# # drop unnecessary columns
# keep_cols <- c("did", "id", "date",
#                "elevation", "friction", "friction_walking_only", "rumple_index", "road_density_mpha",
#                "ndvi", "veg_structure_rumple", 
#                "peak_ridge_cliff", "valleys", "slope_warm", "slope_cool", "slope_neutral", "flat", 
#                "trees_tm01", "shrubs_tm01", "grass_forb_herb_tm01", "barren_tm01",
#                "landform_diversity", "landcover_diversity_tm01")
# 
# roads_fi_drivers <- roads_fi_drivers[, .SD, .SDcols = keep_cols]
# roads_fired_drivers <- roads_fired_drivers[, .SD, .SDcols = keep_cols]
# 
# # convert to percentiles
# convert_proportions_to_percentiles(fired_drivers = roads_fired_drivers,
#                                    fi_drivers = roads_fi_drivers,
#                                    out_fname = "data/out/drivers/fluc-static-driver-proportion-percentiles.csv")
