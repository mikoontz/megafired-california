# Rasterize the annual tree mortality data from the California aerial detection surveys

library(terra)
library(raster)
library(dplyr)
library(sf)
library(here)
library(fasterize)
library(tidyr)
library(ggplot2)
library(USAboundaries)
library(grainchanger)
library(pbapply)
library(exactextractr)
library(stars)

# Goal is to determine dead trees per acre (TPA) numbers that delineate the 5
# severity categories used for 2017- aerial detection surveys
# Step 1 is to figure out the distribution of area categorized into each of
# those 5 severity classes for a given year
# Step 2 is to determine, *for the same year*, what the distribution of 

ca <- 
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(sf::st_crs(3310)) %>% 
  terra::vect()

# Make implicit missing ADS data explicit with NA's
djny <-
  read.csv("data/raw/doi_10.5061_dryad.7vt36__v1/Young_et_al_Data.csv")

# create the template raster using grid from Young et al., 2017 (Ecology Letters)
template_r <-
  djny %>% 
  dplyr::filter(year == 2009) %>% 
  dplyr::select(alb.x, alb.y, mort.tph) %>% 
  terra::rast(crs = sf::st_crs(3310)$wkt, type = "xyz") %>% 
  setNames("mort.tph") %>% 
  terra::extend(y = ca) %>% 
  # raster::raster() %>% # put back into Raster form to be compatible with grainchanger
  setValues(values = NA)

template_df <-
  as.data.frame(template_r, xy = TRUE)

# get 2017 ADS data
ads2017_surveyed_areas <-
  sf::st_read(here::here("data", "raw", "CONUS_Region5_AllYears.gdb"),
              layer = "SURVEYED_AREAS_FLAT_AllYears_CONUS_Rgn5") %>% 
  dplyr::filter(SURVEY_YEAR == 2017) %>% 
  sf::st_transform(3310) %>% 
  terra::vect()

# use surveyed area as a mask
surveyed_r <-
  terra::rasterize(x = ads2017_surveyed_areas, y = template_r, touches = TRUE)

# data from the 2017 aerial detection surveys (uses the 5 classes of tree mortality severity)
ads2017 <- 
  sf::st_read(here::here("data", "raw", "CONUS_Region5_AllYears.gdb"), 
              layer = "DAMAGE_AREAS_FLAT_AllYears_CONUS_Rgn5") %>% 
  dplyr::filter(SURVEY_YEAR == 2017) %>% 
  st_transform(terra::crs(template_r))

# Severity categories: 
sev_abbrv <- data.frame(code = 0:5, 
                        orig_name = c("", "Very Light (1-3%)", "Light (4-10%)", "Moderate (11-29%)", "Severe (30-50%)", "Very Severe (>50%)"),
                        new_name = c("none", "v_light", "light", "moderate", "severe", "v_severe"))

ads2017 <-
  ads2017 %>% 
  dplyr::mutate(severity = sev_abbrv$new_name[match(x = PERCENT_AFFECTED_CODE, table = sev_abbrv$code)])

# get the coverage fraction of each polygon in the ADS data for 2017 for each
# 3.5km x 3.5km cell in the template raster. Per documentation, loop through
# the different features and combine rasters as we go
s <- 
  ads2017 %>% 
  base::split(f = .$severity) %>% 
  pblapply(FUN = function(x) {
  
  frac <-
    exactextractr::coverage_fraction(x = raster::raster(template_r), y = x, crop = FALSE) %>%
    raster::stack() %>% 
    sum() %>% 
    terra::rast()

  return(frac)
}) %>% 
  terra::rast() %>% 
  terra::mask(mask = surveyed_r) # give NAs to cells where there wasn't any survey

# Some errors inherent in coverage fraction calculation (likely due to some overlapping ads polygons?)
max_frac <- terra::app(x = s, fun = sum)
no_mort <- 1 - max_frac
no_mort[which(no_mort[] < 0)] <- 0
names(no_mort) <- "none"
no_mort <- terra::mask(x = no_mort, mask = surveyed_r)

max_frac <- terra::app(x = c(no_mort, s), fun = sum)
corrected_s <- c(no_mort, s) / max_frac

plot(corrected_s)

s <- c(no_mort, s)


# Now we have the 2017 ADS data in the same raster grid as from Young et al., 2017
plurality_severity_r <- which.max(corrected_s) - 1

plot(plurality_severity_r)

plurality_severity <- 
  c(corrected_s, plurality_severity_r) %>% 
  as.data.frame(xy = TRUE) %>% 
  dplyr::rename(plural_sev_code = which.max) %>% 
  dplyr::mutate(plural_sev = sev_abbrv$new_name[match(x = plural_sev_code, table = sev_abbrv$code)]) %>% 
  dplyr::mutate(plural_sev = factor(plural_sev, levels = sev_abbrv$new_name))

area_coverage_of_severity_classes_2017 <-
  plurality_severity %>% 
  group_by(plural_sev) %>% 
  tally() %>% 
  dplyr::mutate(area = 3500*3500/10000*n)




## What does LEMMA data suggest mortality looked like in 2017?
lemma <- terra::rast("data/raw/2017-lemma-forest-structure-data/rasters/stph_ge_25_2017.tif")

tph_live <- terra::rast("data/raw/2017-lemma-forest-structure-data/rasters/tph_ge_3_2017.tif")
terra::NAflag(tph_live) <- -1

tph_snag <- terra::rast("data/raw/2017-lemma-forest-structure-data/rasters/stph_ge_25_2017.tif")
terra::NAflag(tph_snag) <- -1

pct_dead <- tph_snag / (tph_live + tph_snag)
plot(pct_dead)

rcl_matrix <- data.frame(from = c(0, 0.03, 0.10, 0.29, 0.50),
                         to = c(0.03, 0.10, 0.29, 0.50, 1.0),
                         becomes = 1:5)
dead_categories <- terra::classify(x = pct_dead, rcl = rcl_matrix)
plot(dead_categories)

dead_categories_coarse <- terra::aggregate(dead_categories, fact = 117, fun = "modal")

terra::writeRaster(x = dead_categories_coarse, filename = "data/out/pct-dead-categories_coarse.tif")
plot(dead_categories_coarse, col = viridis::viridis(6))
st_crs(3310)

plot(dead_categories_coarse, col = viridis::viridis(6))
plot(plurality_severity_r, col = viridis::viridis(6))














data <-
  djny %>% 
  dplyr::select(alb.x, alb.y, year, mort.tph, live.tph, mort.tpa) %>% 
  dplyr::mutate(pct_mort = mort.tph / (mort.tph + live.tph),
                area_ha = 3500*3500/10000,
                severity_code = dplyr::case_when(pct_mort == 0.00 ~ 0,
                                                 pct_mort <= 0.03 ~ 1,
                                                 pct_mort <= 0.10 ~ 2,
                                                 pct_mort <= 0.29 ~ 3,
                                                 pct_mort <= 0.50 ~ 4,
                                                 pct_mort > 0.50 ~ 5 )) %>% 
  dplyr::mutate(severity = sev_abbrv$new_name[match(x = severity_code, table = sev_abbrv$code)]) %>% 
  dplyr::mutate(severity = factor(severity, levels = sev_abbrv$new_name))

data

ggplot(data %>% filter(pct_mort > 0), aes(x = pct_mort)) +
  geom_histogram()

ggplot(dplyr::filter(plurality_severity, plural_sev != "none"), aes(x = plural_sev)) +
  geom_bar()

ggplot(dplyr::filter(data, severity != "none"), aes(severity)) + 
  geom_bar() + 
  facet_wrap(facets = vars(year))
  




# National Insect and Disease Detection Surveys
# Info about Digital Mobile Sketch Mapping
# https://www.fs.fed.us/foresthealth/applied-sciences/mapping-reporting/digital-mobile-sketch-mapping.shtml

dir.create(here::here("data", "raw"), showWarnings = FALSE)

usfs_regions <- c(2, 3, 4, 5)
usfs_regions <- c(5)

sapply(usfs_regions, FUN = function(x) {
  if(!file.exists(here::here("data", "raw", paste0("CONUS_Region", x, "_AllYears.gdb")))) {
    download.file(url = paste0("https://www.fs.fed.us/foresthealth/docs/IDS_Data_for_Download/CONUS_Region", x, "_AllYears.gdb.zip"),
                  destfile = here::here("data", "raw", paste0("CONUS_Region", x, "_AllYears.gdb.zip")))
    
    unzip(zipfile = here::here("data", "raw", paste0("CONUS_Region", x, "_AllYears.gdb.zip")), exdir = here::here("data", "raw"))
    unlink(here::here("data", "raw", paste0("CONUS_Region", x, "_AllYears.gdb.zip")))
  }
  
})

# sf::st_layers(here::here("data", "raw", "CONUS_Region5_AllYears.gdb"))
ads_r2 <- sf::st_read(here::here("data", "raw", "CONUS_Region2_AllYears.gdb"), layer = "DAMAGE_AREAS_FLAT_AllYears_CONUS_Rgn2")
ads_r3 <- sf::st_read(here::here("data", "raw", "CONUS_Region3_AllYears.gdb"), layer = "DAMAGE_AREAS_FLAT_AllYears_CONUS_Rgn3")
ads_r4 <- sf::st_read(here::here("data", "raw", "CONUS_Region4_AllYears.gdb"), layer = "DAMAGE_AREAS_FLAT_AllYears_CONUS_Rgn4")
ads_r5 <- sf::st_read(here::here("data", "raw", "CONUS_Region5_AllYears.gdb"), layer = "DAMAGE_AREAS_FLAT_AllYears_CONUS_Rgn5")

ads_r2 %>% 
  filter(!is.na(PERCENT_MIN) & !is.na(LEGACY_NO_TREES))

ads_r5 %>% 
  filter(SURVEY_YEAR < 2017 & !is.na(PERCENT_MIN))
ads_r5 %>% 
  filter(SURVEY_YEAR >= 2017 & !is.na(LEGACY_TPA))

ads_r4 %>% 
  filter(SURVEY_YEAR == 2013 & !is.na(LEGACY_TPA))


ads_r5_4326 <- sf::st_transform(x = ads_r5, crs = 4326)

ads_r5_4326

test <- 
  ads_r5_4326 %>% 
  dplyr::filter(SURVEY_YEAR == 2020)

test2 <- 
  ads_r5 %>% 
  dplyr::filter(SURVEY_YEAR == 2015) %>% 
  dplyr::filter(DAMAGE_TYPE == "Mortality")

test3 <- 
  ads_r5_4326 %>% 
  dplyr::filter(SURVEY_YEAR == 2017)


ads_r5_4326 %>% filter(SURVEY_YEAR >= 2017) %>% st_drop_geometry() %>% group_by(SURVEY_YEAR, PERCENT_MIN) %>% summarize(sum_area = sum(SHAPE_Area)) %>% tidyr::pivot_wider(names_from = PERCENT_MIN, values_from = sum_area)

ads_r5_4326 %>% filter(SURVEY_YEAR >= 2001 & SURVEY_YEAR < 2017) %>% st_drop_geometry() %>% group_by(SURVEY_YEAR, PERCENT_MIN) %>% summarize(sum_area = sum(SHAPE_Area)) %>% tidyr::pivot_wider(names_from = PERCENT_MIN, values_from = sum_area)

ads_terra <- terra::vect(ads4326)

r <- raster::raster(test, res = 0.0315)
r <- terra::rast(ads_terra, res = 0.0315)

f <- fasterize::fasterize(sf = ads4326, raster = r, field = "PERCENT_MID", by = "SURVEY_YEAR", fun = "sum")

years <- (2001 - 5):2015

s <- sapply(years, FUN = function(y) {
  return(terra::rasterize(x = ads_terra[ads_terra$SURVEY_YEAR == y, ], y = r, field = "PERCENT_MIN"))
})

y = years[1]
plot(f)



f <- raster::calc(x = f, fun = mean)

unique(test$PERCENT_MID)
unique(test$PERCENT_MIN)
unique(test$PERCENT_MAX)
4.35 * 0.46
