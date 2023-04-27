# extract historical disturbance data for all FIRED events as well as all 
# FIRED events randomly located in 500 additional fire-independent locations
# within each respective biome

library(dplyr)
library(terra)
library(USAboundaries)
library(sf)
library(lubridate)
library(stringr)
library(tidyr)
library(data.table)
library(exactextractr)
library(pbapply)
library(here)
library(parallel)

dir.create(here::here("data", "out", "drivers", "landfire-disturbance", "fire-independent-locations"), 
           showWarnings = FALSE, 
           recursive = TRUE)

# Our cutoff is fires from 2011, because that's when RTMA weather data starts
# So to use a t-10 year window, we'll need to get Landfire disturbance data back to 2001
n_cores <- 12L

relevant_files <- 
  read.csv(here::here("data", "out", "landfire-disturbance", "file-directory_landfire-disturbance_conus.csv")) %>% 
  dplyr::mutate(out_path_ca = gsub(x = out_path, pattern = "conus", replacement = "ca"))

# # Potential disturbance type groupings:
# unique(rat$dist_type)
new_dist_type <-
  tibble::tribble(
    ~dist_type,          ~new_dist_type,                  ~new_dist,
    "Wildfire",          "fire",                          1,
    "Wildland Fire Use", "fire",                          1,
    "Prescribed Fire",   "fire",                          1,
    "Wildland Fire",     "fire",                          1,
    "Fire",              "fire",                          1,
    "Insects",           "insect_disease",                2,
    "Disease",           "insect_disease",                2,
    "Insects/Disease",   "insect_disease",                2,
    "Biological",        "insect_disease",                2,
    "Clearcut",          "clearcut_harvest_othermech",    3,
    "Harvest",           "clearcut_harvest_othermech",    3,
    "Other Mechanical",  "clearcut_harvest_othermech",    3,
    "Thinning",          "fuel_trt",                      4,
    "Mastication",       "fuel_trt",                      4,
    "Weather",           "weather",                       5,
    "Development",       "development",                   6,
    "Chemical",          "chemical",                      7,
    "Herbicide" ,        "chemical",                      7,
    "Insecticide",       "insecticide",                   8,
    "Unknown",           "unknown",                       9)

# # Potential severity groupings:
# unique(rat$severity)
new_sev_type <-
  tibble::tribble(
    ~severity,         ~new_sev_type,           ~new_sev,
    "Unburned/Low",    "low",                   1,
    "Low",             "low",                   1,
    "No Severity",     "low",                   1,
    "Medium",          "medium",                2,
    "Low-Medium",      "medium",                2,
    "Medium-Low",      "medium",                2,
    "Medium-High",     "medium",                2,
    "High-Medium",     "medium",                2,
    "High",            "high",                  3,
    "Increased Green", "increased_green",       4)

new_dist_sev_table <- 
  tidyr::expand_grid(new_dist_type, new_sev_type) %>% 
  # This makes the 51, 52, and 53 categories lumped versions of "everything else at low severity, medium severity, and high severity"
  dplyr::mutate(new_val = dplyr::case_when((10 * new_dist + new_sev) %in% c(51, 61, 71, 81, 91) ~ 51,
                                           (10 * new_dist + new_sev) %in% c(52, 62, 72, 82, 92) ~ 52,
                                           (10 * new_dist + new_sev) %in% c(53, 63, 73, 83, 93) ~ 53,
                                           (10 * new_dist + new_sev) %in% c(54, 64, 74, 84, 94) ~ 54,
                                           TRUE ~ (10 * new_dist + new_sev))) %>% 
  dplyr::mutate(new_dist_type = dplyr::case_when((new_val %in% 51:54) ~ "other",
                                                 TRUE ~ new_dist_type)) %>% 
  dplyr::mutate(new_cat = paste(new_dist_type, new_sev_type, sep = "_"),
                new_sev_name = paste(new_sev_type, "sev", sep = "_"))

rat <-
  read.csv(here::here("data", "out", "landfire-disturbance", "raster-attribute-table_landfire-disturbance_conus.csv")) %>% 
  dplyr::left_join(new_dist_sev_table, by = c("dist_type", "severity"))

paste(sort(unique(rat$value[rat$new_dist_type == "fuel_trt" & rat$severity == "No Severity"])), collapse = ", ")
paste(sort(unique(rat$value[rat$new_dist_type == "fuel_trt" & rat$severity == "Unburned/Low"])), collapse = ", ")
paste(sort(unique(rat$value[rat$new_dist_type == "fuel_trt" & rat$severity == "Low"])), collapse = ", ")

years <- (min(relevant_files$year) + 10):2020

new_dist_sev_table_simple <-
  new_dist_sev_table %>% 
  dplyr::group_by(new_val, new_cat, new_dist_type, new_sev_type, new_sev_name) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  dplyr::select(new_val, new_cat, new_dist_type, new_sev_type, new_sev_name) %>% 
  data.table::as.data.table()

extract_disturbance_fracs <-
  function(new_val, area) {
    out_dt <- data.table(new_val, area, keep.rownames = TRUE)
    out_dt[, cell_area_frac_of_tot := area / sum(area)]
    out_dt[, area := NULL]

    out_dt_long <- data.table::melt(data = out_dt,
                                    id.vars = c("rn", "cell_area_frac_of_tot"),
                                    value.name = "new_val",
                                    variable.name = "time",
                                    na.rm = TRUE)
    out_dt_long <- data.table::merge.data.table(x = new_dist_sev_table_simple, y = out_dt_long, by = "new_val")

    ###
    out_dt_fire_insect_tm01_tm10 <-
      out_dt_long[
        new_dist_type %in% c("fire", "insect_disease"),
        .(.N),
        by = .(rn, cell_area_frac_of_tot)]
    
    out_dt_fire_insect_tm01_tm10 <- data.table(out_dt_fire_insect_tm01_tm10 = sum(out_dt_fire_insect_tm01_tm10$cell_area_frac_of_tot))

    # Proportion of daily fire footprint covered by each disturbance type/severity combination across the tm01 to tm05 period
    out_dt_dist_sev_tm01_tm05 <-
      out_dt_long[
        time %in% c("tm01", "tm02", "tm03", "tm04", "tm05"),
        .(.N),
        by = .(rn, cell_area_frac_of_tot, new_cat)][
          , .(frac = sum(cell_area_frac_of_tot)),
          by = .(new_cat)]
    if(nrow(out_dt_dist_sev_tm01_tm05) > 0) {
      out_dt_dist_sev_tm01_tm05 <- data.table::dcast(data = out_dt_dist_sev_tm01_tm05, formula = . ~ new_cat, value.var = "frac")
      out_dt_dist_sev_tm01_tm05[, . := NULL]
      names(out_dt_dist_sev_tm01_tm05) <- paste(names(out_dt_dist_sev_tm01_tm05), "tm01_tm05", sep = "_")
    } else {
      out_dt_dist_sev_tm01_tm05 <- data.table::data.table(fire_low_tm01_tm05 = NA)
    }

    # Proportion of daily fire footprint covered by each disturbance type across the tm01 to tm05 period
    out_dt_dist_tm01_tm05 <-
      out_dt_long[
        time %in% c("tm01", "tm02", "tm03", "tm04", "tm05"),
        .(.N),
        by = .(rn, cell_area_frac_of_tot, new_dist_type)][
          , .(frac = sum(cell_area_frac_of_tot)),
          by = .(new_dist_type)]
    if(nrow(out_dt_dist_tm01_tm05) > 0) {
      out_dt_dist_tm01_tm05 <- data.table::dcast(data = out_dt_dist_tm01_tm05, formula = . ~ new_dist_type, value.var = "frac")
      out_dt_dist_tm01_tm05[, . := NULL]
      names(out_dt_dist_tm01_tm05) <- paste(names(out_dt_dist_tm01_tm05), "tm01_tm05", sep = "_")
    } else {
      out_dt_dist_tm01_tm05 <- data.table::data.table(fire_tm01_tm05 = NA)
    }

    # Proportion of daily fire footprint covered by each disturbance type/severity combination across the tm06 to tm10 period
    out_dt_dist_sev_tm06_tm10 <-
      out_dt_long[
        time %in% c("tm06", "tm07", "tm08", "tm09", "tm10"),
        .(.N),
        by = .(rn, cell_area_frac_of_tot, new_cat)][
          , .(frac = sum(cell_area_frac_of_tot)),
          by = .(new_cat)]
    if(nrow(out_dt_dist_sev_tm06_tm10) > 0) {
      out_dt_dist_sev_tm06_tm10 <- data.table::dcast(data = out_dt_dist_sev_tm06_tm10, formula = . ~ new_cat, value.var = "frac")
      out_dt_dist_sev_tm06_tm10[, . := NULL]
      names(out_dt_dist_sev_tm06_tm10) <- paste(names(out_dt_dist_sev_tm06_tm10), "tm06_tm10", sep = "_")
    } else {
      out_dt_dist_sev_tm06_tm10 <- data.table::data.table(fire_low_tm06_tm10 = NA)
    }

    # Proportion of daily fire footprint covered by each disturbance type across the tm06 to tm10 period
    out_dt_dist_tm06_tm10 <-
      out_dt_long[
        time %in% c("tm06", "tm07", "tm08", "tm09", "tm10"),
        .(.N),
        by = .(rn, cell_area_frac_of_tot, new_dist_type)][
          , .(frac = sum(cell_area_frac_of_tot)),
          by = .(new_dist_type)]
    if(nrow(out_dt_dist_tm06_tm10) > 0) {
      out_dt_dist_tm06_tm10 <- data.table::dcast(data = out_dt_dist_tm06_tm10, formula = . ~ new_dist_type, value.var = "frac")
      out_dt_dist_tm06_tm10[, . := NULL]
      names(out_dt_dist_tm06_tm10) <- paste(names(out_dt_dist_tm06_tm10), "tm06_tm10", sep = "_")
    } else {
      out_dt_dist_tm06_tm10 <- data.table::data.table(fire_tm06_tm10 = NA)
    }


    # Proportion of daily fire footprint covered by each disturbance type across the tm01 to tm10 period
    out_dt_dist_tm01_tm10 <-
      out_dt_long[
        ,
        .(.N),
        by = .(rn, cell_area_frac_of_tot, new_dist_type)][
          , .(frac = sum(cell_area_frac_of_tot)),
          by = .(new_dist_type)]
    if(nrow(out_dt_dist_tm01_tm10) > 0) {
      out_dt_dist_tm01_tm10 <- data.table::dcast(data = out_dt_dist_tm01_tm10, formula = . ~ new_dist_type, value.var = "frac")
      out_dt_dist_tm01_tm10[, . := NULL]
      names(out_dt_dist_tm01_tm10) <- paste(names(out_dt_dist_tm01_tm10), "tm01_tm10", sep = "_")
    } else {
      out_dt_dist_tm01_tm10 <- data.table::data.table(fire_tm01_tm10 = NA)
    }


    out <- cbind(out_dt_fire_insect_tm01_tm10, 
                 out_dt_dist_sev_tm01_tm05,
                 out_dt_dist_tm01_tm05,
                 out_dt_dist_sev_tm06_tm10,
                 out_dt_dist_tm06_tm10,
                 out_dt_dist_tm01_tm10)
    
    return(out)
  }

# extract_disturbance_fracs <-
#   function(new_val, area) {
#     out_dt <- data.table(new_val, area, keep.rownames = TRUE)
#     out_dt[, cell_area_frac_of_tot := area / sum(area)]
#     out_dt[, area := NULL]
#     
#     out_dt_long <- data.table::melt(data = out_dt,
#                                     id.vars = c("rn", "cell_area_frac_of_tot"),
#                                     value.name = "new_val",
#                                     variable.name = "time",
#                                     na.rm = TRUE)
#     out_dt_long <- data.table::merge.data.table(x = new_dist_sev_table_simple, y = out_dt_long, by = "new_val")
#     
#     ###
#     # # Proportion of daily fire footprint covered by each disturbance type across the tm01 to tm10 period
#     out_dt_dist_tm01_tm10 <-
#       out_dt_long[
#         new_dist_type %in% c("fire", "insect_disease"),
#         .(.N),
#         by = .(rn, cell_area_frac_of_tot)]
#     
#     out <- data.table(fire_insect_disease_tm01_tm10 = sum(out_dt_dist_tm01_tm10$cell_area_frac_of_tot))
#     
#     # if(nrow(out_dt_dist_tm01_tm10) > 0) {
#     #   out_dt_dist_tm01_tm10 <- out_dt_dist_tm01_tm10$frac 
#     #   # out_dt_dist_tm01_tm10 <- data.table::dcast(data = out_dt_dist_tm01_tm10, formula = . ~ new_dist_type, value.var = "frac")
#     #   # out_dt_dist_tm01_tm10[, . := NULL]
#     #   names(out_dt_dist_tm01_tm10) <- "fire_insect_disease_tm01_tm10"
#     # } else {
#     #   out_dt_dist_tm01_tm10 <- data.table::data.table(fire_insect_disease_tm01_tm10 = NA)
#     # }
#     # 
#     # # Proportion of daily fire footprint covered by any disturbance across the tm01 to tm10 period
#     # out_dt_any_tm01_tm10 <- 
#     #   out_dt_long[
#     #     , 
#     #     .(.N), 
#     #     by = .(rn, cell_area_frac_of_tot)]
#     # 
#     # if(nrow(out_dt_any_tm01_tm10) > 0) {
#     #   out_dt_any_tm01_tm10 <- data.table::data.table(disturbance_tm01_tm10 = sum(out_dt_any_tm01_tm10$cell_area_frac_of_tot))
#     # } else {
#     #   out_dt_any_tm01_tm10 <- data.table::data.table(disturbance_tm01_tm10 = NA)
#     # }
#     # 
#     # out <- cbind(out_dt_dist_tm01_tm10,
#     #              out_dt_any_tm01_tm10)
#     
#     return(out)
#   }

### Extract disturbance data for FIRED data
# Note this takes about 5.5 minutes across 12 cores.
set.seed(1103)
fired_list <- 
  sf::st_read("data/out/fired/02_time-filter-crs-transform/fired_daily_ca_epsg3310_2003-2020.gpkg") %>%  
  dplyr::filter(lubridate::year(date) %in% years) %>% 
  dplyr::select(did, id, date, samp_id) %>% 
  dplyr::mutate(group = sample(x = 1:n_cores, size = nrow(.), replace = TRUE)) %>% 
  dplyr::group_by(group) %>% 
  dplyr::group_split()

test_ftr <- 
  fired_list[[4]] %>% 
  dplyr::mutate(area = sf::st_area(.)) %>% 
  dplyr::filter(area == max(area)) %>% 
  dplyr::select(-area)

test_ftr <- 
  tail(fired_list[[4]])[1, ]

test_ftr_yr <- unique(lubridate::year(test_ftr$date))
test_relevant_files_yr <- 
  relevant_files %>% 
  dplyr::mutate(tm_year = test_ftr_yr - year,
                tm_name = ifelse(tm_year >= 0, 
                                 yes = paste0("tm", stringr::str_pad(string = tm_year, width = 2, side = "left", pad = "0")),
                                 no = paste0("tp", stringr::str_pad(string = abs(tm_year), width = 2, side = "left", pad = "0")))) %>% 
  dplyr::filter(tm_year %in% 1:10) %>% 
  dplyr::arrange(tm_year)

test_landfire_yr <-
  terra::rast(test_relevant_files_yr$out_path_ca) %>% 
  setNames(nm = test_relevant_files_yr$tm_name)

test_ftr <-
  test_ftr %>% 
  sf::st_transform(crs = sf::st_crs(test_landfire_yr))

test_out <- 
  exactextractr::exact_extract(x = test_landfire_yr, y = test_ftr,
                               fun = extract_disturbance_fracs,
                               coverage_area = TRUE,
                               include_area = FALSE,
                               stack_apply = FALSE,
                               append_cols = c("did", "id", "date", "samp_id")) %>% 
  as.data.table()

#   out_dt_dist_tm01_tm10[, . := NULL]
#   names(out_dt_dist_tm01_tm10) <- paste(names(out_dt_dist_tm01_tm10), "tm01_tm10", sep = "_")

cl <- parallel::makeCluster(n_cores)
parallel::clusterEvalQ(cl = cl, expr = {
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(exactextractr)
  library(terra)
  library(data.table)
})
parallel::clusterExport(cl, c("new_dist_sev_table_simple", "relevant_files", "extract_disturbance_fracs"))

(start <- Sys.time())
out_fired <- 
  pblapply(X = fired_list, cl = cl, FUN = function(fired) {
    fired_unique_years <- unique(lubridate::year(fired$date))
    out_group <- vector(mode = "list", length = length(fired_unique_years))
    
    for (i in seq_along(fired_unique_years)) {
      relevant_files_idx <- match(x = fired_unique_years[i], table = relevant_files$year)
      
      fired_year <- 
        fired %>% 
        dplyr::filter(lubridate::year(date) == fired_unique_years[i]) %>% 
        sf::st_transform(crs = sf::st_crs(terra::rast(relevant_files$out_path_ca[relevant_files_idx])))
      
      relevant_files_year <- 
        relevant_files %>% 
        dplyr::mutate(tm_year = fired_unique_years[i] - year,
                      tm_name = ifelse(tm_year >= 0, 
                                       yes = paste0("tm", stringr::str_pad(string = tm_year, width = 2, side = "left", pad = "0")),
                                       no = paste0("tp", stringr::str_pad(string = abs(tm_year), width = 2, side = "left", pad = "0")))) %>% 
        dplyr::filter(tm_year %in% 1:10) %>% 
        dplyr::arrange(tm_year)
      
      landfire_year <-
        terra::rast(relevant_files_year$out_path_ca) %>% 
        setNames(nm = relevant_files_year$tm_name)
      
      out_group[[i]] <- 
        exactextractr::exact_extract(x = landfire_year, y = fired_year,
                                     fun = extract_disturbance_fracs,
                                     coverage_area = TRUE,
                                     include_area = FALSE,
                                     stack_apply = FALSE,
                                     append_cols = c("did", "id", "date", "samp_id"))
      
      
    }
    
    out_group <- data.table::rbindlist(out_group, fill = TRUE)
    
    return(out_group)
  }) %>% 
  data.table::rbindlist(fill = TRUE)
(end <- Sys.time())
(difftime(end, start, units = "mins"))

data.table::setnafill(x = out_fired, type = "const", fill = 0, cols = names(out_fired)[!(names(out_fired) %in% c("did", "id", "date", "samp_id"))])
parallel::stopCluster(cl)

data.table::fwrite(x = out_fired, file = here::here("data", "out", "drivers", "landfire-disturbance", "fired_daily_disturbance-drivers_v1.csv"))


### Extract disturbance data for fire-independent data
# Note this takes a *long* time, and I think I should have written it in Earth Engine
# 5.5 minutes, as above, but multiplied by 500 because the randomly located polygons
# have 500 representations of each of the FIRED polygons (i.e., several days, much babysitting)
fi_version <- "v4"
fi_files <- list.files("data/out/fired/04_fire-independent-locations/", pattern = paste0("((", fi_version, ").*(.shp))"), full.names = TRUE, recursive = TRUE)
basenames <- gsub(x = basename(fi_files), pattern = paste0("_", fi_version), replacement = "")
fi_out_names <- here::here("data", "out", "drivers", "landfire-disturbance", "fire-independent-locations", gsub(x = basenames, pattern = ".shp", replacement = "_disturbance-drivers_v1.csv"))

for(k in seq_along(fi_files)[!file.exists(fi_out_names)]) {
  
  fi_out_name <- fi_out_names[k]
  
  (start <- Sys.time())
  print(start)
  gc()
  fi_list <-
    sf::st_read(fi_files[k]) %>%
    dplyr::filter(lubridate::year(date) %in% years) %>%
    dplyr::select(did, id, samp_id, date) %>%
    dplyr::mutate(group = sample(x = 1:(n_cores), size = nrow(.), replace = TRUE)) %>% 
    dplyr::group_by(group) %>% 
    dplyr::group_split()
  
  cl <- parallel::makeCluster(n_cores)
  parallel::clusterEvalQ(cl = cl, expr = {
    library(dplyr)
    library(lubridate)
    library(stringr)
    library(tidyr)
    library(exactextractr)
    library(terra)
    library(data.table)
  })
  parallel::clusterExport(cl, c("new_dist_sev_table_simple", "relevant_files", "extract_disturbance_fracs"))
  
  out_fi <- 
    pblapply(X = fi_list, cl = cl, FUN = function(fi) {
      fi_unique_years <- unique(lubridate::year(fi$date))
      out_group <- vector(mode = "list", length = length(fi_unique_years))
      
      for (i in seq_along(fi_unique_years)) {
        relevant_files_idx <- match(x = fi_unique_years[i], table = relevant_files$year)
        
        fi_year <- 
          fi %>% 
          dplyr::filter(lubridate::year(date) == fi_unique_years[i]) %>% 
          sf::st_transform(crs = sf::st_crs(terra::rast(relevant_files$out_path_ca[relevant_files_idx])))
        
        relevant_files_year <- 
          relevant_files %>% 
          dplyr::mutate(tm_year = fi_unique_years[i] - year,
                        tm_name = ifelse(tm_year >= 0, 
                                         yes = paste0("tm", stringr::str_pad(string = tm_year, width = 2, side = "left", pad = "0")),
                                         no = paste0("tp", stringr::str_pad(string = abs(tm_year), width = 2, side = "left", pad = "0")))) %>% 
          dplyr::filter(tm_year %in% 1:10) %>% 
          dplyr::arrange(tm_year)
        
        landfire_year <-
          terra::rast(relevant_files_year$out_path_ca) %>% 
          setNames(nm = relevant_files_year$tm_name)
        
        out_group[[i]] <- 
          exactextractr::exact_extract(x = landfire_year, y = fi_year,
                                       fun = extract_disturbance_fracs,
                                       coverage_area = TRUE,
                                       include_area = FALSE,
                                       stack_apply = FALSE,
                                       append_cols = c("did", "id", "date", "samp_id"))
        
      }
      
      out_group <- data.table::rbindlist(out_group, fill = TRUE)
      
      return(out_group)
    }) %>% 
    data.table::rbindlist(fill = TRUE)
  
  data.table::setnafill(x = out_fi, type = "const", fill = 0, cols = names(out_fi)[!(names(out_fi) %in% c("did", "id", "date", "samp_id"))])
  data.table::fwrite(x = out_fi, file = fi_out_name)
  
  parallel::stopCluster(cl)
  (end <- Sys.time())
  (difftime(end, start, units = "mins"))
  
}


