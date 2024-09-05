# Prepare roads drivers

library(sf)
library(data.table)
library(dplyr)
library(pbapply)

roads_version <- "v1"

dir.create(here::here("data", "out", "drivers", "roads", "fire-independent-locations"), 
           recursive = TRUE,
           showWarnings = FALSE)

n_workers <- 6L

set.seed(1103)

# https://gisdata-caltrans.opendata.arcgis.com/datasets/cf4982ddf16c4c9ca7242364c94c7ad6_0/explore
if (!file.exists("data/out/roads.gpkg")) {
  gdal_utils(util = "vectortranslate",
             source = "data/raw/CRS_-_Functional_Classification/HWY_CRS.shp",
             destination = "data/out/roads.gpkg",
             options = c("-dim", "XY",
                         "-nlt", "MULTILINESTRING",
                         "-t_srs", "EPSG:3310",
                         "-overwrite"))
  
  roads <- sf::st_read("data/out/roads.gpkg")
}

roads <- sf::st_read("data/out/roads.gpkg")

# Our cutoff is fires from 2011, because that's when RTMA weather data starts
fired_list <- 
  sf::st_read("data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg") %>%  
  dplyr::filter(lubridate::year(date) %in% 2011:2020) %>% 
  dplyr::select(did, id, date, samp_id) %>% 
  dplyr::mutate(group = sample(x = 1:n_workers, size = nrow(.), replace = TRUE)) %>% 
  dplyr::group_by(group) %>% 
  dplyr::group_split()

cl <- parallel::makeCluster(n_workers)
parallel::clusterEvalQ(cl = cl, expr = {
  library(dplyr)
  library(sf)
  library(data.table)
})
parallel::clusterExport(cl, c("roads"))

if (!file.exists(here::here("data", "out", "drivers", "roads", paste0("fired_daily_road-drivers_", roads_version, ".csv")))) {
  (start <- Sys.time())
  out_fired <- 
    pbapply::pblapply(X = fired_list, cl = cl, FUN = function(fired) {
      
      inter <- 
        sf::st_intersection(x = fired, y = roads) %>% 
        dplyr::mutate(length = sf::st_length(.)) %>% 
        sf::st_drop_geometry() %>% 
        data.table::as.data.table()
      
      out <- inter[, .(road_length_m = as.numeric(sum(length))), by = .(did, id, date, samp_id)]
      
      return(out)
    }) %>% 
    data.table::rbindlist(fill = TRUE)
  (end <- Sys.time())
  (difftime(end, start, units = "mins"))
  
  data.table::fwrite(x = out_fired, file = here::here("data", "out", "drivers", "roads", "fired_daily_road-drivers_v1.csv"))
}

fi_version <- "v4"
fi_files <- list.files("data/out/fired/04_fire-independent-locations/", pattern = paste0("((", fi_version, ").*(.shp))"), full.names = TRUE, recursive = TRUE)
basenames <- gsub(x = basename(fi_files), pattern = paste0("_", fi_version), replacement = "")
fi_out_names <- here::here("data", "out", "drivers", "roads", "fire-independent-locations", gsub(x = basenames, pattern = ".shp", replacement = "_roads-drivers_v1.csv"))

for(k in seq_along(fi_files)[!file.exists(fi_out_names)]) {
  
  fi_out_name <- fi_out_names[k]
  
  (start <- Sys.time())
  print(start)
  gc()
  fi_list <-
    sf::st_read(fi_files[k]) %>%
    dplyr::filter(lubridate::year(date) %in% 2011:2020) %>% 
    dplyr::select(did, id, date, samp_id) %>% 
    dplyr::mutate(group = sample(x = 1:n_workers, size = nrow(.), replace = TRUE)) %>% 
    dplyr::group_by(group) %>% 
    dplyr::group_split()
  
  cl <- parallel::makeCluster(n_workers)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(dplyr)
    library(sf)
    library(data.table)
  })
  parallel::clusterExport(cl, c("roads"))
  
  out_fi <- 
    pblapply(X = fi_list, cl = cl, FUN = function(fi) {
      
      inter <- 
        sf::st_intersection(x = fi, y = roads) %>% 
        dplyr::mutate(length = sf::st_length(.)) %>% 
        sf::st_drop_geometry() %>% 
        data.table::as.data.table()
      
      out <- inter[, .(road_length_m = as.numeric(sum(length))),by = .(did, id, date, samp_id)]
      
      return(out)
    }) %>% 
    data.table::rbindlist(fill = TRUE)
  (end <- Sys.time())
  (difftime(end, start, units = "mins"))
  
  data.table::fwrite(x = out_fi, file = fi_out_name) 
}

parallel::stopCluster(cl)

### Fireshed analysis
firesheds <- sf::st_read(here::here("data", "out", "firesheds_conus.gpkg"))
ca <- USAboundaries::us_states(states = "California", resolution = "high")

firesheds_ca <-
  firesheds |> 
  sf::st_make_valid() |> 
  sf::st_filter(ca) |> 
  dplyr::mutate(date = lubridate::ymd("2020-08-01"),
                samp_id = 0,
                did = glue::glue("{id}-{date}")) |> 
  dplyr::select(did, id, date, samp_id) |> 
  sf::st_transform(sf::st_crs(roads))

firesheds_ca_list <- 
  firesheds_ca |>
  dplyr::mutate(group = sample(x = 1:n_workers, size = dplyr::n(), replace = TRUE)) |> 
  dplyr::group_by(group) |> 
  dplyr::group_split()

cl <- parallel::makeCluster(n_workers)
parallel::clusterEvalQ(cl = cl, expr = {
  library(dplyr)
  library(sf)
  library(data.table)
})
parallel::clusterExport(cl, c("roads"))

if (!file.exists(here::here("data", "out", "drivers", "roads", paste0("fireshed_road-drivers_", roads_version, ".csv")))) {
  (start <- Sys.time())
  out_fired <- 
    pbapply::pblapply(X = firesheds_ca_list, cl = cl, FUN = function(fired) {
      
      inter <- 
        sf::st_intersection(x = fired, y = roads) %>% 
        dplyr::mutate(length = sf::st_length(.)) %>% 
        sf::st_drop_geometry() %>% 
        data.table::as.data.table()
      
      out <- inter[, .(road_length_m = as.numeric(sum(length))), by = .(did, id, date, samp_id)]
      
      return(out)
    }) %>% 
    data.table::rbindlist(fill = TRUE)
  (end <- Sys.time())
  (difftime(end, start, units = "mins"))
  
  data.table::fwrite(x = out_fired, file = here::here("data", "out", "drivers", "roads", "fireshed_road-drivers_v1.csv"))
}

parallel::stopCluster(cl)

### PODS analysis
### Fireshed analysis
pods = sf::st_read(here::here("data", "out", "sierra-nevada-pods.gpkg")) |> 
  sf::st_make_valid() |>
  dplyr::select(did, id, date, samp_id) 

pods_list = 
  pods |>
  dplyr::mutate(group = sample(x = 1:n_workers, size = dplyr::n(), replace = TRUE)) |> 
  dplyr::group_by(group) |> 
  dplyr::group_split()

cl <- parallel::makeCluster(n_workers)
parallel::clusterEvalQ(cl = cl, expr = {
  library(dplyr)
  library(sf)
  library(data.table)
})
parallel::clusterExport(cl, c("roads"))

if (!file.exists(here::here("data", "out", "drivers", "roads", paste0("pods_road-drivers_", roads_version, ".csv")))) {
  (start <- Sys.time())
  out_fired <- 
    pbapply::pblapply(X = pods_list, cl = cl, FUN = function(fired) {
      
      inter <- 
        sf::st_intersection(x = fired, y = roads) %>% 
        dplyr::mutate(length = sf::st_length(.)) %>% 
        sf::st_drop_geometry() %>% 
        data.table::as.data.table()
      
      out <- inter[, .(road_length_m = as.numeric(sum(length))), by = .(did, id, date, samp_id)]
      
      return(out)
    }) %>% 
    data.table::rbindlist(fill = TRUE)
  (end <- Sys.time())
  (difftime(end, start, units = "mins"))
  
  data.table::fwrite(x = out_fired, file = here::here("data", "out", "drivers", "roads", "pods_road-drivers_v1.csv"))
}

parallel::stopCluster(cl)