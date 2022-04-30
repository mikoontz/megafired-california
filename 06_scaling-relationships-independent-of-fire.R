# Determine scaling relationships with the drivers independent of fire

library(dplyr)
library(terra)
library(sf)
library(data.table)
library(USAboundaries)
library(exactextractr)
library(ggplot2)

dir.create("data/out/fired_daily_neutral-locations_tcf")

if(!file.exists("data/out/fired_daily_random-locations_temperate-conifer-forests_v2.gpkg")) {
  (start_time <- Sys.time())
  resolve <- 
    sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
    sf::st_transform(3310) %>% 
    sf::st_set_agr(value = "constant")
  
  ca <- 
    USAboundaries::us_states(resolution = "high", states = "California") %>% 
    sf::st_transform(3310) %>% 
    sf::st_set_agr(value = "constant")
  
  ca_or_nv_az <- 
    USAboundaries::us_states(resolution = "high", states = c("California", "Oregon", "Nevada", "Arizona")) %>% 
    sf::st_transform(3310) %>% 
    sf::st_set_agr(value = "constant") %>% 
    sf::st_union()
  
  fired <- 
    sf::st_read("data/out/fired_daily_ca_ewe_rank_v5.gpkg") %>% 
    dplyr::filter(tot_hect >= 120) %>% 
    dplyr::filter(ig_year >= 2003 & ig_year <= 2020) %>% 
    sf::st_transform(3310) %>% 
    sf::st_set_agr(value = "constant")
  
  fired_tcf <-
    fired %>% 
    dplyr::filter(biome_name == "Temperate Conifer Forests") %>% 
    dplyr::select(did, id, date) %>% 
    dplyr::mutate(centroid_x = sf::st_coordinates(sf::st_centroid(.))[, "X"],
                  centroid_y = sf::st_coordinates(sf::st_centroid(.))[, "Y"])
  
  tcf <- resolve[resolve$BIOME_NAME == "Temperate Conifer Forests", ] %>% sf::st_intersection(y = ca)
  tcf_buff <- sf::st_intersection(x = tcf, y = sf::st_buffer(x = ca_or_nv_az, dist = -50000))
  
  # tcf_sample_points <- sf::st_sample(x = tcf, size = round(as.numeric(sf::st_area(tcf)) / 1e9), by_polygon = TRUE)
  # tcf_sample_points <- sf::st_sample(x = sf::st_buffer(tcf_u, dist = -25000), size = round(sum(as.numeric(sf::st_area(tcf)) / 1e9)), type = "hexagonal")
  set.seed(1506)
  tcf_sample_points <- 
    sf::st_sample(x = tcf_buff, size = 1000, type = "hexagonal") %>% 
    sf::st_sf() %>% 
    dplyr::mutate(samp_id = 1:nrow(.),
                  x_samp = sf::st_coordinates(.)[, "X"],
                  y_samp = sf::st_coordinates(.)[, "Y"]) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::as_tibble()
  
  (start_time <- Sys.time())
  fired_tcf_with_samps <-
    fired_tcf %>% 
    dplyr::mutate(samps = list(tcf_sample_points)) %>% 
    tidyr::unnest(cols = "samps")
  
  samps_geo <-
    fired_tcf_with_samps %>% 
    sf::st_drop_geometry() %>% 
    sf::st_as_sf(coords = c("x_samp", "y_samp"), crs = 3310) %>%
    sf::st_geometry()
  
  fired_centroids <- 
    fired_tcf_with_samps %>% 
    sf::st_drop_geometry() %>% 
    sf::st_as_sf(coords = c("centroid_x", "centroid_y"), crs = 3310) %>% 
    sf::st_geometry()
  
  fired_raw <- sf::st_read("data/out/fired_daily_ca.gpkg")
  table(fired_raw$pixels)
  
  affine <- samps_geo - fired_centroids
  new_fired_geo <- (sf::st_geometry(fired_tcf_with_samps) + affine) %>% sf::st_set_crs(3310)
  
  fired_tcf_with_new_geo <- sf::st_set_geometry(x = fired_tcf_with_samps, value = new_fired_geo)
  (end_time <- Sys.time())
  (difftime(time1 = end_time, time2 = start_time, units = "mins"))
  
  sf::st_write(obj = fired_tcf_with_new_geo, dsn = "data/out/fired_daily_random-locations_temperate-conifer-forests_v2.gpkg")
  (end_time <- Sys.time())
  (difftime(time1 = end_time, time2 = start_time, units = "mins"))
  
  fired_tcf_with_new_geo <-
    fired_tcf_with_new_geo %>% 
    dplyr::select(did, id, date)
  
  (start_time <- Sys.time())
  sf::st_write(obj = fired_tcf_with_new_geo, dsn = "data/out/fired_daily_neutral-locations_tcf/fired_daily_neutral-locations.shp")
    (end_time <- Sys.time())
  (difftime(time1 = end_time, time2 = start_time, units = "mins"))
}


if (!file.exists("data/out/neutral-proportions_landforms_temperate-conifer-forests.csv")) {
  fired_tcf_with_new_geo <- sf::st_read(dsn = "data/out/fired_daily_random-locations_temperate-conifer-forests.gpkg")
  
  r <- terra::rast("data/out/ee/driver-layers/landforms.tif")
  
  # https://cran.r-project.org/web/packages/exactextractr/vignettes/vig2_categorical.html
  (start_time <- Sys.time())
  landforms_fracs <- exact_extract(r, fired_tcf_with_new_geo, function(df) {
    DT <- as.data.table(df)
    DT[, frac_total := coverage_fraction / sum(coverage_fraction)][, .(freq = sum(frac_total)), by = .(did, samp_id, value)]
  }, summarize_df = TRUE, include_cols = c('did', 'samp_id'), progress = TRUE)
  
  data.table::fwrite(x = landforms_fracs, file = "data/out/neutral-proportions_landforms_temperate-conifer-forests.csv")
  (end_time <- Sys.time())
  (difftime(time1 = end_time, time2 = start_time, units = "mins"))
  # 222 minutes
}

landforms_fracs <- data.table::fread("data/out/neutral-proportions_landforms_temperate-conifer-forests.csv")

# We've read in the compact form, but we need to fill in 0 proportion for all the frequency
# values that didn't have a value associated with them. That is, we need all landform categories
# represented within each samp_id for every did.
# https://stackoverflow.com/questions/54579436/data-table-equivalent-of-tidyrcomplete-with-group-by-with-on-and-by-syntax
landforms_fracs[, `:=`(did = as.factor(did), 
                       value = as.factor(value),
                       samp_id = as.factor(samp_id))]

# modified data table to get all possible combinations of samp_id and value within each
# did group
mDT <- landforms_fracs[, CJ(samp_id = samp_id, value = value, unique = TRUE), by = did]

# next step is to join back to original landforms_fracs data
cvars <- data.table::copy(names(mDT))
ovars <- setdiff(names(landforms_fracs), cvars)

mDT[, (ovars) := landforms_fracs[.SD, on = cvars, mget(sprintf("x.%s", ovars))]]

# Fill in all the NA's with 0's
data.table::setnafill(mDT, type = "const", fill = 0, cols = "freq")

landforms_fracs <- mDT
rm(mDT)


drivers <- data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california_v5.csv")
drivers_tcf <- 
  drivers %>% 
  dplyr::filter(biome_name == "Temperate Conifer Forests")
# %>% 
#   dplyr::select(did, id, date, aoi_ha, (starts_with("csp_ergo_landforms") & ends_with("prop")))

zeros <- 
  landforms_fracs %>% 
  filter(value == 0) %>% 
  group_by(samp_id) %>% 
  arrange(desc(freq))

plot_data <- left_join(tcf_sample_points, zeros)
ggplot(plot_data, aes(x = x_samp, y = y_samp, color = freq)) +
  geom_point(cex = 5)

v <- vect(tcf_sample_points, geom = c("x_samp", "y_samp"), crs = sf::st_crs(3310))

plot(r)
plot(v, add = TRUE)
plot(vect(tcf), add = TRUE)
plot(vect(ca), add = TRUE)
plot(august_30, add = TRUE, fill = "red")
?`plot,SpatVector,character-method`

plot(august_30)
plot(r, add = TRUE, )
plot(august_30, add = TRUE)




did_test <- "9496-2003-01-12"
did_test <- "9496-2003-01-19"
did_test <- "9579-2003-01-22"
did_test <- "9496-2003-01-15"
did_test <- sample(drivers_tcf$did, size = 1)
did_test <- drivers_tcf$did[drivers_tcf$aoi_ha > 500 & drivers_tcf$aoi_ha < 550][1]
did_test <- drivers_tcf$did[drivers_tcf$aoi_ha > 200 & drivers_tcf$aoi_ha < 250][1]
did_test <- drivers_tcf$did[drivers_tcf$aoi_ha > 100 & drivers_tcf$aoi_ha < 150][1]


test_drivers <- 
  drivers_tcf[drivers_tcf$did == did_test, ] %>% 
  mutate(valley_prop = csp_ergo_landforms_41_prop + csp_ergo_landforms_42_prop,
         landform_diversity = vegan::diversity(cbind(csp_ergo_landforms_11_prop, csp_ergo_landforms_12_prop, csp_ergo_landforms_13_prop, csp_ergo_landforms_14_prop, csp_ergo_landforms_15_prop, csp_ergo_landforms_21_prop, csp_ergo_landforms_22_prop, csp_ergo_landforms_23_prop, csp_ergo_landforms_24_prop, csp_ergo_landforms_31_prop, csp_ergo_landforms_32_prop, csp_ergo_landforms_33_prop, csp_ergo_landforms_34_prop, csp_ergo_landforms_41_prop, csp_ergo_landforms_42_prop)))

test_neutral <- landforms_fracs[landforms_fracs$did == did_test, ]
test_neutral_wide <- 
  test_neutral %>% 
  mutate(value = paste0("lf_", value)) %>% 
  tidyr::pivot_wider(id_cols = c(did, samp_id), names_from = "value", values_from = "freq") %>% 
  mutate(valley_prop = lf_41 + lf_42,
         landform_diversity = vegan::diversity(cbind(lf_11, lf_12, lf_14, lf_15, lf_21, lf_22, lf_23, lf_24, lf_31, lf_32, lf_33, lf_34, lf_41, lf_42)))

ggplot(test_neutral_wide, aes(x = landform_diversity)) + 
  geom_histogram() +
  geom_vline(xintercept = test_drivers$landform_diversity, col = "red") +
  ggtitle(label = paste0("Landform diversity across 200 randomly-located fire areas of ", round(test_drivers$aoi_ha), " ha\n(red vertical line is actual landform diversity within the actual fire area)\nArea of increase residual is ", round(test_drivers$aoir_mod, 2), "\nLandform diversity percentile is ", round(sum(test_drivers$landform_diversity > test_neutral_wide$landform_diversity) / nrow(test_neutral_wide), 2)))

# august <- fired[fired$did == "135646-2020-09-26", ]
# august_30 <- fired_tcf_with_new_geo[fired_tcf_with_new_geo$did == "135646-2020-09-26" & fired_tcf_with_new_geo$samp_id == 30, ] %>% vect()
# 
# test_drivers <- drivers[drivers$did == "135646-2020-09-26", ]
# test_neutral <- landforms_fracs[landforms_fracs$did == "135646-2020-09-26", ]

ggplot(test_neutral[test_neutral$value == 41, ], aes(x = freq)) + 
  geom_histogram() +
  geom_vline(xintercept = test_drivers$csp_ergo_landforms_41_prop, col = "red") +
  ggtitle(label = paste0("Proportion of valleys across 200 randomly-located fire areas of ", round(test_drivers$aoi_ha), " ha\n(red vertical line is actual proportion within the actual fire area)\nArea of increase residual is ", round(test_drivers$aoir_mod, 2)))

ggplot(test_neutral[test_neutral$value == 42, ], aes(x = freq)) + 
  geom_histogram() +
  geom_vline(xintercept = test_drivers$csp_ergo_landforms_42_prop, col = "red") +
  ggtitle(label = paste0("Proportion of narrow valleys across 200 randomly-located fire areas of ", round(test_drivers$aoi_ha), " ha\n(red vertical line is actual proportion within the actual fire area)\nArea of increase residual is ", round(test_drivers$aoir_mod, 2)))

ggplot(test_neutral_wide, aes(x = valley_prop)) + 
  geom_histogram() +
  geom_vline(xintercept = test_drivers$valley_prop, col = "red") +
  ggtitle(label = paste0("Proportion of narrow + regular valleys across 200 randomly-located fire areas of ", round(test_drivers$aoi_ha), " ha\n(red vertical line is actual proportion within the actual fire area)\nArea of increase residual is ", round(test_drivers$aoir_mod, 2)))

ggplot(test_neutral_wide, aes(x = landform_diversity)) + 
  geom_histogram() +
  geom_vline(xintercept = test_drivers$landform_diversity, col = "red") +
  ggtitle(label = paste0("Landform diversity across 200 randomly-located fire areas of ", round(test_drivers$aoi_ha), " ha\n(red vertical line is actual landform diversity within the actual fire area)\nArea of increase residual is ", round(test_drivers$aoir_mod, 2), "\nLandform diversity percentile is ", round(sum(test_drivers$landform_diversity > test_neutral_wide$landform_diversity) / nrow(test_neutral_wide)
                                                                                                                                                                                                                                                                                                                        , 2)))


# what percentile?
sum(test_drivers$csp_ergo_landforms_41_prop > test_neutral$freq[test_neutral$value == 41]) / nrow(test_neutral[test_neutral$value == 41, ])

sum(test_drivers$csp_ergo_landforms_42_prop > test_neutral$freq[test_neutral$value == 42]) / nrow(test_neutral[test_neutral$value == 42, ])

sum(test_drivers$landform_diversity > test_neutral_wide$landform_diversity) / nrow(test_neutral_wide)

test_drivers$aoir_mod

plot(test$geom)
plot(sf::st_geometry(fired_tcf_with_new_geo))
plot(sf::st_as_sf(tcf_sample_points, coords = c("x_samp", "y_samp"), crs = 3310), add = TRUE, col = "red", pch = 19)

plot(tcf$geometry)
plot(sf::st_as_sf(tcf_sample_points, coords = c("x_samp", "y_samp"), crs = 3310), add = TRUE, col = "red", pch = 19)


tcf <- terra::vect(tcf)

r_tcf <- 
  terra::crop(x = r, y = tcf) 

r_tcf <- terra::mask(x = r_tcf, mask = tcf)
