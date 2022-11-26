library(dplyr)
library(data.table)

convert_proportions_to_percentiles <- function(fired_drivers, fi_drivers, out_fname = NULL) {
  
  # drop unnecessary columns
  fi_drivers[, `:=`(date = as.Date(date),
                    samp_id = NULL)]
  
  fired_drivers[, `:=`(date = as.Date(date),
                       samp_id = NULL)]
  
  # data.table wide to long format reshaping (equivalent to tidyr::tidy_longer)
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
  fi_drivers_long <- data.table::melt(data = fi_drivers, id.vars = c("did", "date", "id"))
  
  # data.table nesting (equivalent to tidyr::nest)
  # https://stackoverflow.com/questions/25430986/create-nested-data-tables-by-collapsing-rows-into-new-data-tables
  fi_drivers_long <- fi_drivers_long[, list(data = list(.SD)), by = c("did", "id", "date", "variable")]  
  
  # Just pivot the fired drivers from wide to long using data.table::melt(); no need
  # to also nest (because it's only one value-- the one that will be compared against the
  # ~500 nested values from the fire-independent locations)
  fired_drivers_long <- data.table::melt(data = fired_drivers, id.vars = c("did", "date", "id"))
  
  # merge fire-independent drivers data with the fired drivers data
  fires_and_fi <- merge(x = fi_drivers_long, 
                        y = fired_drivers_long, 
                        on = c("did", "date", "id", "driver"))
  
  # convert raw proportional cover values to percentils based on comparing measured value within the
  # fire footprint to the measured value if you were to re-locate that fire footprint to 500 other 
  # locations within the biome
  # https://stackoverflow.com/questions/49939936/how-to-do-operations-on-list-columns-in-an-r-data-table-to-output-another-list-c
  fires_and_fi[, adj := mapply(data, value, 
                               FUN = function(fi_value, fire_value) {ecdf(fi_value$value)(fire_value)})]
  
  # drop the data column representing the proportions at the ~500 fire-independent locations
  fires_and_fi[, `:=`(data = NULL,
                      value = NULL)]
  
  fires_and_fi <- data.table::dcast(data = fires_and_fi, formula = did + id + date ~ variable, value.var = "adj")
  
  if (!is.null(out_fname)) {
    data.table::fwrite(x = fires_and_fi, file = out_fname)
    return(NULL)
  } else {
    return(fires_and_fi)
  }
}

# Convert fluctuating and static driver proportions from Earth Engine to percentiles
static_fluc_fired_drivers <-data.table::fread("data/out/drivers/fired-fluc-static-driver-proportions.csv")
static_fluc_fi_drivers <- data.table::fread("data/out/drivers/fi-fluc-static-driver-proportions.csv")

convert_proportions_to_percentiles(fired_drivers = static_fluc_fired_drivers,
                                   fi_drivers = static_fluc_fi_drivers,
                                   out_fname = "data/out/drivers/fluc-static-driver-proportion-percentiles.csv")

# Landfire disturbance data is a bit bigger and can't all fit in memory so we break it up by biome
gc()

biome_name_lookup <- tibble::tibble(biome_shortname = c("tcf", "mfws", "tgss", "dxs"),
                                    biome_fullname = c("Temperate Conifer Forests", 
                                                       "Mediterranean Forests, Woodlands & Scrub", 
                                                       "Temperate Grasslands, Savannas & Shrublands", 
                                                       "Deserts & Xeric Shrublands"))

fired_daily_resolve <- 
  read.csv("data/out/fired/03_joined-with-other-data/fired_daily_resolve.csv") %>% 
  dplyr::left_join(biome_name_lookup, by = c("biome_name_daily" = "biome_fullname")) %>% 
  dplyr::select(did, biome_shortname)

lf_fired_drivers <- data.table::fread("data/out/drivers/landfire-disturbance/fired_daily_disturbance-drivers_v1.csv")
lf_fired_drivers[, `:=`(fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
                       fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10,
                       insect_disease_not_high_tm01_tm05 = insect_disease_tm01_tm05 - insect_disease_high_tm01_tm05,
                       insect_disease_not_high_tm06_tm10 = insect_disease_tm06_tm10 - insect_disease_high_tm06_tm10)]

lf_fi_files <- list.files("data/out/drivers/landfire-disturbance/fire-independent-locations/",  full.names = TRUE)
lf_fi_drivers <- lapply(lf_fi_files, FUN = data.table::fread) |> data.table::rbindlist(fill = TRUE)

lf_fi_drivers <- merge(x = lf_fi_drivers, y = fired_daily_resolve, by = "did")
lf_fi_drivers_l <- split(x = lf_fi_drivers, by = "biome_shortname")
# lf_fi_drivers_l <- lapply(lf_fi_drivers_l, FUN = function(x) set(x, j = "biome_shortname", value = NULL))

# Convert landfire disturbance proportions to percentiles
# out <- vector(mode = "list", length = length(lf_fi_files))

out <- 
  lapply(X = lf_fi_drivers_l, FUN = function(x) {
    
    data.table::set(x, j = "biome_shortname", value = NULL)
    data.table::setnafill(x = x, type = "const", fill = 0, cols = names(x)[!(names(x) %in% c("did", "id", "date", "samp_id"))])
    
    x[, `:=`(fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
             fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10,
             insect_disease_not_high_tm01_tm05 = insect_disease_tm01_tm05 - insect_disease_high_tm01_tm05,
             insect_disease_not_high_tm06_tm10 = insect_disease_tm06_tm10 - insect_disease_high_tm06_tm10)]
    
    return(convert_proportions_to_percentiles(fi_drivers = x,
                                              fired_drivers = lf_fired_drivers, 
                                              out_fname = NULL))
  })

out_all <- data.table::rbindlist(out, fill = TRUE)

data.table::fwrite(x = out_all, file = "data/out/drivers/landfire-disturbance-driver-proportion-percentiles.csv")