# build relationship between area and driver values, independent of fire

library(dplyr)
library(sf)
library(data.table)
library(ggplot2)

fired_daily_drivers <- 
  sf::st_read("data/out/analysis-ready/FIRED-daily-scale-drivers_california_v5.gpkg") %>% 
  dplyr::mutate(area_log10 = log10(proj_area)) %>% 
  dplyr::rename(veg_structure_rumple = forest_structure_rumple) %>% 
  dplyr::select(!(dplyr::starts_with("csp_ergo_landforms") & !dplyr::ends_with("prop"))) %>% 
  dplyr::select(!(dplyr::starts_with("lcms") & !dplyr::ends_with("prop")))
  

random_polys <- 
  sf::st_read("data/out/fire-independent-polygons_tcf_ca.gpkg") %>% 
  dplyr::mutate(area = as.numeric(sf::st_area(.))) %>% 
  sf::st_drop_geometry()

static_drivers <- data.table::fread("data/out/ee/fire-independent-drivers/fire-independent-tcf-static-drivers_california.csv")
static_drivers <- static_drivers[-1, ]
static_drivers[, `:=`(.geo = NULL, `system:index` = NULL)]

fluc_drivers <- data.table::fread("data/out/ee/fire-independent-drivers/fire-independent-tcf-fluctuating-drivers_california.csv")
fluc_drivers <- fluc_drivers[-1, ]
fluc_drivers[, `:=`(.geo = NULL, `system:index` = NULL)]

random_polys <- as.data.table(random_polys)
random_polys[, date := NULL]

static_drivers_with_polys <- random_polys[static_drivers, on = c("did", "id", "samp_id")]
fluc_drivers_with_polys <- random_polys[fluc_drivers, on = c("did", "id", "samp_id")]

out_static <-
  static_drivers_with_polys %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x*10*10)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms"), .fns = ~ .x / area)) %>% 
  dplyr::mutate(rumple_index = surf_area / proj_area) %>% 
  dplyr::mutate(area_log10 = log10(area)) %>%
  dplyr::mutate(area_log10_round = round(area_log10, 1)) %>% 
  dplyr::mutate(date = lubridate::seconds_to_period(date / 1000) + lubridate::ymd("1970-01-01") - lubridate::hours(8)) %>% 
  dplyr::select(did, id, date, everything())

out_static

out_fluc <-
  fluc_drivers_with_polys %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x*30*30)) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("lcms"), .fns = ~ .x / area)) %>%
  dplyr::mutate(veg_structure_rumple = ndvi_surf_area / ndvi_proj_area) %>% 
  dplyr::mutate(area_log10 = log10(area)) %>%
  dplyr::mutate(area_log10_round = round(area_log10, 1)) %>% 
  dplyr::mutate(date = lubridate::seconds_to_period(date / 1000) + lubridate::ymd("1970-01-01") - lubridate::hours(8)) %>% 
  dplyr::mutate(ig_year = lubridate::year(date)) %>% 
  dplyr::select(did, id, date, ig_year, everything())

# https://en.wikipedia.org/wiki/Median_absolute_deviation
# MAD might be mostly useless here, as it will often be 0 for small polygons
fire_independent_static_driver_medians <- 
  out_static %>%
  tidyr::pivot_longer(cols = -c(did, id, date, samp_id, area, area_log10, area_log10_round), names_to = "driver", values_to = "value") %>% 
  group_by(area_log10_round, driver) %>%
  summarize(median = median(value), mad = median(abs(value - median(value)))) %>% 
  dplyr::mutate(ig_year = list(2003:2020)) %>% 
  tidyr::unnest(cols = "ig_year") %>% 
  dplyr::select(ig_year, area_log10_round, dplyr::everything())

fire_independent_fluc_driver_medians <- 
  out_fluc %>%
  tidyr::pivot_longer(cols = -c(did, id, date, ig_year, samp_id, area, area_log10, area_log10_round), names_to = "driver", values_to = "value") %>% 
  group_by(ig_year, area_log10_round, driver) %>% 
  summarize(median = median(value), mad = median(abs(value - median(value))))

fire_indep_meds <- 
  rbind(fire_independent_static_driver_medians, fire_independent_fluc_driver_medians) %>% 
  dplyr::filter(!(driver %in% c("proj_area", "surf_area", "ndvi_proj_area", "ndvi_surf_area"))) %>% 
  dplyr::mutate(driver = case_when(substr(driver, start = 1, stop = 8) == "csp_ergo" ~ paste0(driver, "_prop"),
                                   substr(driver, start = 1, stop = 4) == "lcms" ~ paste0(driver, "_prop"),
                                   TRUE ~ driver))


fired_drivers_long <-
  fired_daily_drivers %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(did, ig_year, area_log10, unique(fire_indep_meds$driver)) %>% 
  tidyr::pivot_longer(cols = -c(did, ig_year, area_log10), names_to = "driver", values_to = "value")

fired_drivers_long
fire_indep_meds

get_expected_median <- function(ig_year, area_log10, driver) {
  working <- 
    fire_indep_meds %>% 
    filter(ig_year == ig_year, driver == driver)
}



###########################################################
get_quantiles <- function(x, probs) {
  quantiles <- quantile(x, probs = probs)
  data.frame(quantile = probs, value = quantiles)
}

area_quants <-
  out_static %>%
  group_by(area_log10_round) %>% 
  summarize(summary = get_quantiles(csp_ergo_landforms_41_prop, probs = c(seq(0, 1, by = 0.025)))) %>% 
  tidyr::unpack(cols = "summary") %>% 
  dplyr::mutate(quantile = as.factor(quantile))


# Narrow valleys
ggplot() +
  geom_point(data = out_static, mapping = aes(x = area_log10, y = csp_ergo_landforms_42_prop)) +
  geom_point(data = area_quants, mapping = aes(x = area_log10_round, y = value, color = quantile)) +
  geom_smooth(data = area_quants, mapping = aes(x = area_log10_round, y = value, color = quantile)) +
  scale_color_viridis_d()

# Valleys
ggplot() +
  geom_point(data = out_static, mapping = aes(x = area_log10, y = csp_ergo_landforms_41_prop)) +
  geom_point(data = area_quants, mapping = aes(x = area_log10_round, y = value, color = quantile)) +
  geom_smooth(data = area_quants, mapping = aes(x = area_log10_round, y = value, color = quantile)) +
  scale_color_viridis_d()

model_data <- 
  out_static %>% 
  dplyr::group_by(area_log10_round) %>% 
  dplyr::mutate(quantile = dplyr::percent_rank(csp_ergo_landforms_42_prop)) %>% 
  dplyr::select(area_log10_round, quantile, csp_ergo_landforms_42_prop) %>% 
  dplyr::rename(area_log10 = area_log10_round)

model_data

library(mgcv)
fm1 <- gam(quantile ~ s(area_log10, csp_ergo_landforms_42_prop), data = model_data)
summary(fm1)
plot(fm1)

newdata <- 
  fired_daily %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(proj_area_log10, csp_ergo_landforms_42_prop) %>%
  dplyr::arrange(proj_area_log10) %>% 
  as_tibble()

# dplyr::mutate(preds = predict(object = fm1, newdata = .)) %>% 

idx <- sample(x = 1:nrow(newdata), size = 1)
area_quants

newdata[idx, ]

newdata
ggplot(newdata, aes(x = proj_area_log10, y = preds)) +
  geom_point() +
  geom_smooth()

out <-
  out %>% 
  dplyr::group_by(log10_area_round) %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("csp_ergo_landforms") & dplyr::ends_with("prop"), .fns = dplyr::percent_rank, .names = "{.col}_pct")) %>% 
  as.data.table()

ggplot(out, aes(x = proj_area_log10, y = csp_ergo_landforms_42_prop, color = csp_ergo_landforms_42_prop_pct)) + 
  geom_point() +
  scale_color_viridis_c()

library(terra)

model_data <-
  out %>% 
  dplyr::select(proj_area_log10, csp_ergo_landforms_42_prop, csp_ergo_landforms_42_prop_pct) %>% 
  as.data.frame()

library(fields) 
tps <- fields::Tps(x = model_data[, c("proj_area_log10", "csp_ergo_landforms_42_prop")],
                   Y = model_data[, "csp_ergo_landforms_42_prop_pct"])

# use model to predict values at all locations
r <- terra::rast(e, nrow = 100, ncol = 100)
surface <- terra::interpolate(r, tps)

surface <- terra::classify(x = surface, rcl = matrix(c(-Inf, 0, 0,
                                                       1, Inf, 1), 
                                                     nrow = 2, ncol = 3,
                                                     byrow = TRUE))
plot(surface, col = viridis::viridis(100))

plot(test)
p <- mask(p, r)
plot(p)


plot(surface, xlim = c(min(out$proj_area_log10), max(out$proj_area_log10)), ylim = c(0, 1))

plot(ra)
r <- rast(system.file("ex/elev.tif", package="terra"))
ra <- aggregate(r, 10)
xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
v <- values(ra)
i <- !is.na(v)
xy <- xy[i,]
v <- v[i]

## Not run: 
library(fields) 
tps <- Tps(xy, v)
p <- rast(r)

p <- interpolate(p, tps)
p <- mask(p, r)
plot(p)
