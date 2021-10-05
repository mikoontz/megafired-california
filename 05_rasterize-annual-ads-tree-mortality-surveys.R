# Rasterize the annual tree mortality data from the California aerial detection surveys

library(terra)
library(raster)
library(dplyr)
library(sf)
library(here)
library(fasterize)

dnjy <- read.csv("data/raw/doi_10.5061_dryad.7vt36__v1/Young_et_al_Data.xlsx")

# National Insect and Disease Detection Surveys
# Info about Digital Mobile Sketch Mapping
# https://www.fs.fed.us/foresthealth/applied-sciences/mapping-reporting/digital-mobile-sketch-mapping.shtml

dir.create(here::here("data", "raw"), showWarnings = FALSE)

usfs_regions <- c(2, 3, 4, 5)

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

tph_live <- terra::rast("data/raw/2017-lemma-forest-structure-data/rasters/tph_ge_3_2017.tif")
terra::NAflag(tph_live) <- -1

tph_snag <- terra::rast("data/raw/2017-lemma-forest-structure-data/rasters/stph_ge_25_2017.tif")
terra::NAflag(tph_snag) <- -1

pct_dead <- tph_snag / (tph_live + tph_snag)
plot(pct_dead)
plot(tph_snag)


f <- raster::calc(x = f, fun = mean)

unique(test$PERCENT_MID)
unique(test$PERCENT_MIN)
unique(test$PERCENT_MAX)
4.35 * 0.46
