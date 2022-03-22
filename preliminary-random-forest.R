library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyr)
library(terra)
library(fasterize)

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  dplyr::select(-id, -BIOME_NUM, -COLOR, -COLOR_BIO, -COLOR_NNH, 
                -ECO_BIOME_, -ECO_ID, -LICENSE, -NNH, -OBJECTID, 
                -REALM, -SHAPE_AREA, -SHAPE_LENG) %>% 
  setNames(tolower(names(.)))

# read in fire data
fires <-
  data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california.csv") %>%
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

events <-
  sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>%
  sf::st_transform(3310) %>%
  sf::st_centroid() %>%
  dplyr::rename(geometry = geom) %>%
  dplyr::mutate(x_3310 = sf::st_coordinates(.)[, "X"],
                y_3310 = sf::st_coordinates(.)[, "Y"]) %>%
  dplyr::filter(tot_hect >= 120) %>%
  dplyr::filter(ig_date >= lubridate::ymd("2003-01-01") & ig_date <= lubridate::ymd("2020-12-31"))

events_resolve <-
  events %>% 
  sf::st_join(y = resolve)

target_fires <-
  fires %>%
  dplyr::filter(tot_hect >= 120)

fires_working <-
  target_fires %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-lcms_landuse_01, -lcms_landuse_02, -lcms_landuse_04, -lcms_landuse_07,
                -lcms_landcover_02, -lcms_landcover_06, -lcms_landcover_02, -lcms_landcover_12, -lcms_landcover_13, -lcms_landcover_14, -lcms_landcover_15, 
                -lcms_change_05,
                -name_frap, -olap_frap, -ig_date, -last_date, -date, -ends_with("rank"), -frp_90, -aoir_max, -aoi_max, -pred_aoi, -act_aoi, -c_area_tm1, -date_frp, -date_aoir, -date_aoi, -e_day_aoir, -e_day_aoi, -event_dur, -model_aoir, -event_modis_lc, -daily_modis_lc,
                -max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -ndvi_proj_area, -ndvi_surf_area, -proj_area, -surf_area, -road_length_m,
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"), -surf_area_ha, -proj_area_ha)

fires_working
