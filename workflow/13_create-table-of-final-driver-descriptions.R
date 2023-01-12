# Final co-variate descriptions

human_drivers <- 
  data.frame(type = "human", 
             variable = c("npl", "concurrent_fires", "caltrans_road_density_mpha"),
             source = c("NIFC", "Derived for this paper using FIRED", "CalTrans [CRS Functional Classification]"),
             calculation = c("National Preparedness Level on the day of the fire/day combination",
                             "Number of FIRED events also active on the day of the fire/day combination",
                             "Percentile of mean road density (meters per hectare) within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)"))

topography_drivers <- 
  data.frame(type = "topography",
             variable = c("elevation", 
                          "rumple_index", 
                          "peak_ridge_cliff", "valleys", "slope_warm", "slope_cool", "slope_neutral", "flat", 
                          "landform_diversity"),
             source = c("Percentiles derived for this paper using USGS 3DEP", 
                        "Calculation and percentiles derived for this paper using USGS 3DEP", 
                        rep("Aggregated from Theobald et al., (2015) and percentiles derived for this paper [Conservation Science Partners 10 m NED Landforms]", times = 6), 
                        "Derived for this paper from Theobald et al., (2015) [Conservation Science Partners 10 m NED Landforms]"),
             calculation = c("Percentile of spatial mean elevation within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of rumple index (calculated using algorithm of Jenness (2004)) of elevation values within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of peak/ridge (neutral), peak/ridge (warm), peak/ridge (cool), cliff, and mountain/divide within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of valleys and narrow valleys within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of upper slope (warm) and lower slope (warm) within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of upper slope (cool) and lower slope (cool) within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of upper slope (neutral) and lower slope (neutral) within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of upper slope (flat) and lower slope (flat) within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of Shannon-Wiener Index of the proportional cover of the 6 aggregated landform types [peak/ridge/cliff, valleys, slope (warm), slope (cool), slope (neutral), flat] within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)"))
#                             "Percentile of Shannon-Wiener Index of the proportional cover of all 15 landform types [peak/ridge (neutral), peak/ridge (warm), peak/ridge (cool), cliff, mountain/divide, valleys, narrow valleys, upper slope (warm), lower slope (warm), upper slope (cool), lower slope (cool), upper slope (neutral), lower slope (neutral), upper slope (flat), lower slope (flat)] within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)"))

weather_drivers <- 
  data.frame(type = "weather",
             variable = c("wind_anisotropy_rtma", 
                          "min_wind_speed_rtma_pct", "max_wind_speed_rtma_pct", 
                          "min_wind_filled_gust_rtma_pct", "max_wind_filled_gust_rtma_pct", 
                          "min_rh_rtma_pct", "max_rh_rtma_pct",
                          "min_temp_rtma_pct", "max_temp_rtma_pct", 
                          "min_vpd_rtma_pct", "max_vpd_rtma_pct", 
                          "bi_pct", "erc_pct", "fm100_pct", "fm1000_pct",
                          "spei14d", "spei30d", "spei90d", "spei180d", "spei270d", "spei1y", "spei2y", "spei5y", "pdsi_z"),
             source = c("Daily summary derived for this paper from NOAA Real Time Mesoscale Analysis", 
                        rep("Daily summary and percentiles derived for this paper from NOAA Real Time Mesoscale Analysis", times = 2),
                        rep("Wind speed filled for all missing gust data, daily summary, and percentiles derived for this paper using NOAA Real Time Mesoscale Analysis", times = 2),
                        rep("Daily summary and percentiles derived for this paper from NOAA Real Time Mesoscale Analysis", times = 4),
                        rep("Calculations, daily summary, and percentiles derived for this paper from NOAA Real Time Mesoscale Analysis", times = 2),
                        rep("Percentiles derived for this paper using data from Abatzoglou (2012) [GRIDMET using NFDRS fire danger index scale]", times = 2),
                        rep("Percentiles derived for this paper using data from Abatzoglou (2012) [GRIDMET]", times = 2),
                        rep("Abatzoglou (2012) [GRIDMET]", times = 9)),
             calculation = c("standard deviation of cosine of hourly wind directions each day",
                             "minimum percentile of hourly sustained wind speed in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "maximum percentile of hourly sustained wind speed in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "minimum percentile of hourly wind gust speed in a day (missing data filled with sustained wind speed) compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "maximum percentile of hourly wind gust speed in a day (missing data filled with sustained wind speed) compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "minimum percentile of hourly relative humidity in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "maximum percentile of hourly relative humidity in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "minimum percentile of hourly temperature in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "maximum percentile of hourly temperature in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "minimum percentile of hourly vapor pressure deficit in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "maximum percentile of hourly vapor pressure deficit (VPD) in a day compared to hourly VPD between 2011-01-01 and 2020-12-31",
                             "percentile of daily burning index (BI) compared to daily BI between 1981-01-01 and 2020-12-31",
                             "percentile of daily energy release component (ERC) compared to daily ERC between 1981-01-01 and 2020-12-31",
                             "percentile of 100-hour fuel moisture (FM100) compared to daily FM100 between 1981-01-01 and 2020-12-31",
                             "percentile of 1000-hour fuel moisture (FM1000) compared to daily FM1000 between 1981-01-01 and 2020-12-31",
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 14 days",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 30 days",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 90 days",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 180 days",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 270 days",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 1 year",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 2 years",                             
                             "standardized precipitation evapotranspiration index (SPEI) aggregated across prior 5 years",
                             "Palmer Drought Severity Index (PDSI) z-score"))


fuel_drivers <- 
  data.frame(type = "fuel",
             variable = c("ndvi", "veg_structure_rumple", 
                          "trees_tm01", "shrubs_tm01", "grass_forb_herb_tm01", "barren_tm01", "landcover_diversity_tm01",
                          "fire_high_tm01_tm05", "fire_high_tm06_tm10",
                          "fire_not_high_tm01_tm05", "fire_not_high_tm06_tm10",
                          "clearcut_harvest_othermech_tm01_tm05", "clearcut_harvest_othermech_tm06_tm10",
                          "fuel_trt_tm01_tm05", "fuel_trt_tm06_tm10",
                          "insect_disease_high_tm01_tm05", "insect_disease_high_tm06_tm10",
                          "insect_disease_not_high_tm01_tm05", "insect_disease_not_high_tm06_tm10"),
             source = c("Spatial summary and percentiles derived for this paper using Landsat surface reflectance", 
                        "Calculation and percentiles derived for this paper using Landsat surface reflectance",
                        rep("Aggregated from US Forest Service Landscape Change Monitoring System (LCMS) Landcover and percentiles derived for this paper", times = 4),
                        "Calculation and percentiles derived for this paper using US Forest Service LCMS Landcover",
                        rep("Aggregated from DOI/USGS LANDFIRE Annual Disturbance and percentiles derived for this paper", times = 12)),
             calculation = c("First, a per-pixel normalized difference vegetation index (NDVI; using near infrared and red surface reflectance) is calculated as a temporal mean across previous growing season (between day of year 152 to day of year 258 in the year before the fire) after cloud masking and applying Landsat Collection 2 scale/offset values. Then, the percentile of spatial mean NDVI within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire).",
                             "Percentile of rumple index (calculated using algorithm of Jenness (2004)) of NDVI values (multiplied by 100 for convenience) within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LCMS 'trees' landcover type one year prior to fire event within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LCMS 'shrubs' and 'shrubs/trees mix' landcover types one year prior to fire event within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LCMS 'grass/forb/herb', 'grass/forb/herb/trees mix', and 'grass/forb/herb/shrubs mix' landcover types one year prior to fire event within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LCMS 'barren', 'barren/trees mix', 'barren/shrubs mix', 'barren/grass/forb/herb mix', and 'snow or ice' landcover types one year prior to fire event within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of Shannon-Wiener Index of the proportional cover of the 4 aggregated LCMS landcover types [trees, shrubs, grass/forb/herb, barren] one year prior to fire event within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             # "Percentile of Shannon-Wiener Index of the proportional cover of 11 LCMS landcover types [trees, shrubs/trees mix, shrubs, grass/forb/herb, grass/forb/herb/trees mix, grass/forb/herb/shrub mix, barren, snow or ice, barren/trees mix, barren/shrub mix, barren/grass/forb/herb mix] one year prior to fire event within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE high-severity fire occurring 1 to 5 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE high-severity fire occurring 6 to 10 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE fire disturbance that wasn't high-severity occurring 1 to 5 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE fire disturbance that wasn't high-severity occurring 6 to 10 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE clearcut, harvest, or other mechanical disturbance occurring 1 to 5 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE clearcut, harvest, or other mechanical disturbance occurring 6 to 10 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE thinning or mastication disturbance occurring 1 to 5 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE thinning or mastication disturbance occurring 6 to 10 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE high-severity insect/disease disturbance occurring 1 to 5 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE high-severity insect/disease disturbance occurring 6 to 10 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE insect/disease disturbance that wasn't high-severity occurring 1 to 5 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)",
                             "Percentile of proportional cover of LANDFIRE insect/disease disturbance that wasn't high-severity occurring 6 to 10 years prior to fire/day combination within polygon representing daily burn perimeter compared to same value calculated when that polygon is located in ~500 other locations within the fire's biome, but not where the fire burned (i.e., independent of fire)"))

interacting_drivers <- 
  data.frame(type = "interacting",
             variable = c("wind_terrain_anisotropy_rtma", "min_wind_terrain_alignment_rtma_pct", "max_wind_terrain_alignment_rtma_pct"),
             source = c("Calculations and daily summary derived for this paper from NOAA Real Time Mesoscale Analysis (RTMA) and USGS 3DEP",
                        rep("Calculations, percentiles, and daily summary derived for this paper from NOAA RTMA and 3DEP", times = 2)),
             calculation = c("2 times standard deviation of hourly wind/terrain alignment [abs(cos(wind direction minus terrain aspect))] in a day",
                             "minimum percentile of hourly wind/terrain alignment [abs(cos(wind direction minus terrain aspect))] in a day compared to hourly wind/terrain alignment between 2011-01-01 and 2020-12-31",
                             "maximum percentile of hourly wind/terrain alignment [abs(cos(wind direction minus terrain aspect))] in a day compared to hourly wind/terrain alignment between 2011-01-01 and 2020-12-31"))

fire_drivers <- 
  data.frame(type = "fire",
             variable = "sqrt_aoi_tm1",
             source = "Derived for this paper using daily FIRED events",
             calculation = "Square root of daily area of increase at fire's previous time step")

out <- rbind(human_drivers,
      topography_drivers,
      weather_drivers,
      fuel_drivers,
      interacting_drivers,
      fire_drivers)

write.csv(x = out, file = "data/out/drivers/driver-descriptions.csv", row.names = FALSE)




#### Interacting aggregations
#           wind_terrain_anisotropy_rtma = 2*sd(abs(cos(wind_aspect_alignment_rad))), # greater standard deviation means MORE asymmetry in wind/terrain alignment in a day
#           wind_terrain_alignment_rtma = mean(abs(cos(wind_aspect_alignment_rad))), # cos() such that exact alignment (wind blowing into uphill slope) gets a 1, 180 degrees off gets a -1 (wind blowing into downhill slope); take the absolute value such that either blowing into uphill or downhill slope gets maximum alignment value 
#           min_wind_terrain_alignment_rtma = min(abs(cos(wind_aspect_alignment_rad))),
#           max_wind_terrain_alignment_rtma = max(abs(cos(wind_aspect_alignment_rad))),
#           min_wind_terrain_alignment_rtma_pct = min(wind_aspect_alignment_rad_pct),
#           max_wind_terrain_alignment_rtma_pct = max(wind_aspect_alignment_rad_pct),


#### weather aggregations
# summarize(wind_anisotropy_rtma = sd(cos(WDIR_rad)), # greater standard deviation means MORE asymmetry in wind direction in a day
#           # multiply wind terrain anisotropy by 2 to put it on the same [0,1] scale as wind anisotropy and wind terrain alignment
#           # instead of [0,0.5]
#           max_wind_speed_rtma = max(WIND),
#           min_wind_speed_rtma = min(WIND),
#           max_wind_gust_rtma = max(GUST),
#           min_wind_gust_rtma = min(GUST),
#           max_wind_filled_gust_rtma = max(wind_filled_gust),
#           min_wind_filled_gust_rtma = min(wind_filled_gust),
#           max_wind_speed_rtma_pct = max(WIND_pct),
#           min_wind_speed_rtma_pct = min(WIND_pct),
#           max_wind_gust_rtma_pct = max(GUST_pct),
#           min_wind_gust_rtma_pct = min(GUST_pct),
#           max_wind_filled_gust_rtma_pct = max(wind_filled_gust_pct),
#           min_wind_filled_gust_rtma_pct = min(wind_filled_gust_pct),
#           max_rh_rtma = max(rh),
#           min_rh_rtma = min(rh),
#           max_rh_rtma_pct = max(rh_pct),
#           min_rh_rtma_pct = min(rh_pct),
#           max_temp_rtma = max(TMP),
#           min_temp_rtma = min(TMP),
#           max_temp_rtma_pct = max(TMP_pct),
#           min_temp_rtma_pct = min(TMP_pct),
#           max_vpd_rtma = max(vpd_hPa),
#           min_vpd_rtma = min(vpd_hPa),
#           max_vpd_rtma_pct = max(vpd_hPa_pct),
#           min_vpd_rtma_pct = min(vpd_hPa_pct))

#### Topography aggregations
# peak_ridge_cliff = peak_ridge_warm + peak_ridge + peak_ridge_cool + mountain_divide + cliff,
# valleys = valley_original + valley_narrow,
# slope_warm = upper_slope_warm + lower_slope_warm,
# slope_cool = upper_slope_cool + lower_slope_cool,
# slope_neutral = upper_slope + lower_slope,
# flat = upper_slope_flat + lower_slope_flat

#### Fuel aggregations
# LCMS Landcover calculations
# trees_tm01 =
#   trees_original_tm01,
# shrubs_tm01 = 
#   shrubs_trees_mix_tm01 + 
#   shrubs_original_tm01,
# grass_forb_herb_tm01 = 
#   grass_forb_herb_original_tm01 + 
#   grass_forb_herb_trees_mix_tm01 + 
#   grass_forb_herb_shrub_mix_tm01,
# barren_tm01 = 
#   barren_trees_mix_tm01 + 
#   barren_shrub_mix_tm01 + 
#   barren_grass_forb_herb_mix_tm01 + 
#   barren_original_tm01 + 
#   snow_or_ice_tm01)]

# # Landfire disturbance aggregations
# new_dist_type <-
#   tibble::tribble(
#     ~dist_type,          ~new_dist_type,                  ~new_dist,
#     "Wildfire",          "fire",                          1,
#     "Wildland Fire Use", "fire",                          1,
#     "Prescribed Fire",   "fire",                          1,
#     "Wildland Fire",     "fire",                          1,
#     "Fire",              "fire",                          1,
#     "Insects",           "insect_disease",                2,
#     "Disease",           "insect_disease",                2,
#     "Insects/Disease",   "insect_disease",                2,
#     "Biological",        "insect_disease",                2,
#     "Clearcut",          "clearcut_harvest_othermech",    3,
#     "Harvest",           "clearcut_harvest_othermech",    3,
#     "Other Mechanical",  "clearcut_harvest_othermech",    3,
#     "Thinning",          "fuel_trt",                      4,
#     "Mastication",       "fuel_trt",                      4,
#     "Weather",           "weather",                       5,
#     "Development",       "development",                   6,
#     "Chemical",          "chemical",                      7,
#     "Herbicide" ,        "chemical",                      7,
#     "Insecticide",       "insecticide",                   8,
#     "Unknown",           "unknown",                       9)
# 
# Severity groupings:
# new_sev_type <-
#   tibble::tribble(
#     ~severity,         ~new_sev_type,           ~new_sev,
#     "Unburned/Low",    "low",                   1,
#     "Low",             "low",                   1,
#     "No Severity",     "low",                   1,
#     "Medium",          "medium",                2,
#     "Low-Medium",      "medium",                2,
#     "Medium-Low",      "medium",                2,
#     "Medium-High",     "medium",                2,
#     "High-Medium",     "medium",                2,
#     "High",            "high",                  3,
#     "Increased Green", "increased_green",       4)
# 
# lf_fired_drivers[, `:=`(date = as.Date(date),
#                         samp_id = NULL,
#                         fire_not_high_tm01_tm05 = fire_tm01_tm05 - fire_high_tm01_tm05,
#                         fire_not_high_tm06_tm10 = fire_tm06_tm10 - fire_high_tm06_tm10,
#                         insect_disease_not_high_tm01_tm05 = insect_disease_tm01_tm05 - insect_disease_high_tm01_tm05,
#                         insect_disease_not_high_tm06_tm10 = insect_disease_tm06_tm10 - insect_disease_high_tm06_tm10)]
# 
