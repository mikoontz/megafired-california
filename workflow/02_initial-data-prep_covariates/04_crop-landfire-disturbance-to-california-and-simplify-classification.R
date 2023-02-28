library(dplyr)
library(terra)
library(USAboundaries)
library(here)

dir.create("data/out/landfire-disturbance/ca", showWarnings = FALSE, recursive = TRUE)

relevant_files <- read.csv(here::here("data", "out", "landfire-disturbance", "file-directory_landfire-disturbance_conus.csv"))

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

# Use this version only if you want a simplified version of these disturbance types
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

unique(rat$value[which(rat$year %in% 2015:2019 & rat$new_val == 41)])

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(sf::st_crs(terra::rast(relevant_files$out_path[1]))) %>% 
  sf::st_geometry() %>% 
  terra::vect()

relevant_files <-
  relevant_files %>% 
  dplyr::mutate(out_path_ca = gsub(x = out_path, pattern = "conus", replacement = "ca"))

(start <- Sys.time())
for (i in 1:nrow(relevant_files)) {
  
  reclassification_mat <- 
    rat %>% 
    dplyr::filter(year == relevant_files$year[i]) %>% 
    dplyr::select(value, new_val) %>% 
    as.matrix() %>% 
    rbind(c(-9999, NA))
  
  r <- 
    terra::rast(relevant_files$out_path[i]) %>% 
    terra::crop(y = ca) %>% 
    terra::classify(rcl = reclassification_mat, 
                    filename = relevant_files$out_path_ca[i], 
                    overwrite = TRUE)
  
}
(end <- Sys.time())
(difftime(end, start, units = "mins"))


