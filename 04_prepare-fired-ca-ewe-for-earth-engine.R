library(sf)
library(dplyr)

dir.create("data/out/fired_events_ca_ewe_rank", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out/fired_daily_ca_ewe_rank", recursive = TRUE, showWarnings = FALSE)

fired_events <- 
  sf::st_read("data/out/fired_events_ca.gpkg") %>% 
  sf::st_transform(4326) %>% 
  rename(ig_date = ignition_date,
         ig_day = ignition_day,
         ig_month = ignition_month,
         ig_year = ignition_year,
         event_dur = event_duration,
         tot_px = total_pixels,
         tot_ar_km2 = total_area_km2,
         fsr_px_dy = fsr_pixels_per_day,
         fsr_km2_dy = fsr_km2_per_day,
         mx_grw_px = max_growth_pixels,
         mn_grw_px = min_growth_pixels,
         mu_grw_px = mean_growth_pixels,
         mx_grw_km2 = max_growth_km2,
         mn_grw_km2 = min_growth_km2,
         mu_grw_km2 = mean_growth_km2,
         mx_grw_dte = max_growth_date,
         lc_code = landcover_code,
         lc_mode = landcover_mode,
         lc_name = lc_name,
         lc_desc = lc_description,
         lc_type = landcover_type,
         ig_utm_x = ignition_utm_x,
         ig_utm_y = ignition_utm_y,
         tot_perim = final_perimeter,
         geometry = geom) %>% 
  dplyr::select(-ig_utm_x, -ig_utm_y)

fired_daily <-
  sf::st_read("data/out/fired_daily_ca.gpkg") %>% 
  sf::st_transform(4326) %>% 
  rename(ig_date = ignition_date,
         ig_day = ignition_day,
         ig_month = ignition_month,
         ig_year = ignition_year,
         event_dur = event_duration,
         tot_px = total_pixels,
         tot_ar_km2 = total_area_km2,
         fsr_px_dy = fsr_pixels_per_day,
         fsr_km2_dy = fsr_km2_per_day,
         mx_grw_px = max_growth_pixels,
         mn_grw_px = min_growth_pixels,
         mu_grw_px = mean_growth_pixels,
         mx_grw_km2 = max_growth_km2,
         mn_grw_km2 = min_growth_km2,
         mu_grw_km2 = mean_growth_km2,
         mx_grw_dte = max_growth_date,
         lc_code = landcover_code,
         lc_mode = landcover_mode,
         lc_name = lc_name,
         lc_desc = lc_description,
         lc_type = landcover_type,
         ig_utm_x = ignition_utm_x,
         ig_utm_y = ignition_utm_y,
         day_ar_km2 = daily_area_km2,
         day_ar_ha = daily_area_ha,
         cum_ar_t = cum_area_ha,
         cum_ar_tm1 = cum_area_ha_tminus1,
         geometry = geom) %>% 
  dplyr::select(-ig_utm_x, -ig_utm_y)

ewe <- 
  read.csv("data/out/extreme-wildfire-events-ranking.csv") %>% 
  dplyr::filter(ignition_year <= 2020) %>% 
  dplyr::rename(olap_frap = overlapping_pct_frap, 
                ig_date = ignition_date, 
                ig_year = ignition_year, 
                ig_month = ignition_month, 
                date_frp = acq_date_frp, 
                date_aoir = acq_date_aoir,
                tot_hect = total_area_ha,
                pred_aoi = predicted_aoi,
                act_aoi = actual_aoi_during_max_aoir,
                c_area_tm1 = cum_area_ha_tminus1,
                date_aoi = acq_date_aoi,
                e_day_aoir = event_day_aoir,
                e_day_aoi = event_day_aoi,
                event_dur = event_duration,
                model_aoir = modeled_max_aoir) %>% 
  dplyr::mutate(megafire = ifelse(mecdf >= 0.9, yes = "megafire", "non-megafire")) %>% 
  dplyr::select(id, megafire, everything())

fired_events_ee <-
  fired_events %>% 
  dplyr::filter(ig_year <= 2020) %>% 
  dplyr::select(id) %>% 
  dplyr::left_join(ewe)

fired_daily_ee <-
  fired_daily %>% 
  dplyr::filter(ig_year <= 2020) %>% 
  dplyr::select(id, did, date) %>% 
  dplyr::left_join(ewe)

sf::st_write(obj = fired_events_ee, dsn = "data/out/fired_events_ca_ewe_rank.gpkg", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_ee, dsn = "data/out/fired_daily_ca_ewe_rank.gpkg", delete_dsn = TRUE)

sf::st_write(obj = fired_events_ee, dsn = "data/out/fired_events_ca_ewe_rank/fired_events_ca_ewe_rank.shp", delete_dsn = TRUE)
sf::st_write(obj = fired_daily_ee, dsn = "data/out/fired_daily_ca_ewe_rank/fired_daily_ca_ewe_rank.shp", delete_dsn = TRUE)

ggplot(fired_events_ee, aes(x = ig_month)) +
  geom_histogram() +
  facet_wrap(facets = "megafire")

ggplot(fired_daily_ee, aes(x = ig_year)) +
  geom_histogram() +
  facet_wrap(facets = "megafire", scales = "free_y")

fired_events_ee %>% st_drop_geometry() %>% count(megafire, ig_year)
