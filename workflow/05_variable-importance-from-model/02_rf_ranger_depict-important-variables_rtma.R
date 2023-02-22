# Plot the conditional predictive impact of variables

library(data.table)
library(dplyr)
library(ggplot2)
library(ggdist)
library(sf)
library(USAboundaries)
library(tmap)
library(patchwork)
library(here)

latest_rf_cpi_date <- sort(list.files(path = here::here("data", "out", "rf", "conditional-predictive-impact")), 
                              decreasing = TRUE)[1]

latest_rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", latest_rf_cpi_date)
latest_rf_cpi_figs_dir <- here::here("figs", "rf", "conditional-predictive-impact", latest_rf_cpi_date)

dir.create(latest_rf_cpi_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(latest_rf_cpi_figs_dir, showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

# Full names of the biomes for the plot titles
# biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

driver_description <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

col_pal_df <- 
  read.csv(here::here("data", "out", "driver-color-palette.csv"))

col_pal <- setNames(object = col_pal_df$hexcode, nm = col_pal_df$type)

cpi <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <-
      data.table::fread(input = here::here(latest_rf_cpi_dir, paste0("rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))) %>% 
      dplyr::rename(variable = Variable)
  }) %>% 
  data.table::rbindlist()

cpi_summary_across_iter <- 
  cpi %>% 
  dplyr::group_by(variable, spatial_fold = id, biome) %>% 
  dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
                   sd = sd(CPI, na.rm = TRUE),
                   count = sum(!is.na(CPI)),
                   min = min(CPI, na.rm = TRUE),
                   max = max(CPI, na.rm = TRUE),
                   lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
                   upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
  dplyr::arrange(biome, variable, spatial_fold) %>% 
  dplyr::mutate(biome_fullname = biome_lookup[match(biome, names(biome_lookup))]) %>% 
  dplyr::left_join(driver_description)

cpi_summary_across_folds <-
  cpi_summary_across_iter %>% 
  dplyr::group_by(variable, biome) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::arrange(biome, desc(cpi_median)) %>% 
  dplyr::left_join(driver_description)

cpi_summary_across_folds %>% 
  dplyr::filter(biome == "tcf", cpi_median > 0) %>% 
  dplyr::arrange(desc(cpi_median)) %>% 
  print(n = 25)

cpi_summary_across_folds %>% 
  dplyr::filter(biome == "tcf", cpi_mean > 0) %>% 
  dplyr::arrange(desc(cpi_mean)) %>% 
  print(n = 50)

cpi_summary_across_folds %>% 
  dplyr::filter(biome == "mfws", cpi_median > 0) %>% 
  dplyr::arrange(desc(cpi_median)) %>% 
  print(n = 25)

cpi_summary_across_folds %>% 
  dplyr::filter(biome == "mfws", cpi_mean > 0) %>% 
  dplyr::arrange(desc(cpi_mean)) %>% 
  print(n = 25)

cpi_summary_across_folds %>% 
  dplyr::filter(biome == "dxs", cpi_median > 0) %>% 
  dplyr::arrange(desc(cpi_median)) %>% 
  print(n = 25)

cpi_summary_across_folds %>% 
  dplyr::filter(biome == "dxs", cpi_mean > 0) %>% 
  dplyr::arrange(desc(cpi_mean)) %>% 
  print(n = 25)

cpi_tcf <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "tcf", ]
cpi_mfws <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "mfws", ]
# cpi_tgss <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "tgss", ]
cpi_dxs <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "dxs", ]

cpi_summary_across_folds_tcf <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "tcf", ]
cpi_summary_across_folds_mfws <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "mfws", ]
# cpi_summary_across_folds_tgss <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "tgss", ]
cpi_summary_across_folds_dxs <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "dxs", ]


# ca <-
#   USAboundaries::us_states(resolution = "high", states = "California") %>% 
#   sf::st_transform(3310) %>% 
#   sf::st_geometry()
# 
# resolve <- 
#   sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
#   sf::st_transform(3310) %>% 
#   sf::st_intersection(y = ca)

# Interval plots + maps
# Temperate Conifer Forests
tcf_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_tcf, 
                             mapping = aes(y = factor(variable, levels = rev(cpi_summary_across_folds_tcf$variable)), 
                                           x = cpi, xmin = lwr, xmax = upr, color = type), size = 2) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  theme(legend.position = "none") + 
  facet_wrap(facets = "biome_fullname") +
  labs(y = "Driver variable",
       x = "Conditional predictive impact") +
  geom_point(data = cpi_summary_across_folds_tcf, mapping = aes(x = cpi_median, y = factor(variable, levels = rev(cpi_summary_across_folds_tcf$variable))), pch = "X", size = 2.5) +
  geom_vline(xintercept = 0)

tcf_cpi_gg
# resolve_tcf <- 
#   resolve %>% 
#   dplyr::filter(ECO_NAME %in% cpi_tcf$eco_name_daily)
# 
# tcf_tmap <-
#   tm_shape(ca) +
#   tm_polygons() +
#   tm_shape(resolve_tcf) +
#   # tm_fill("ECO_NAME", title = "Ecoregion name", palette = color_palette) +
#   tm_layout(legend.position = c("left", "bottom"), 
#             legend.width = 0.7, frame = FALSE)
# 
# tcf_tmap_grob <- tmap::tmap_grob(tcf_tmap)
# 
# p <- tcf_cpi_gg + patchwork::inset_element(p = tcf_tmap_grob, 
#                                            left = 0.5,
#                                            bottom = 0.025,
#                                            right = 0.995,
#                                            top = 0.675,
#                                            align_to = "panel",
#                                            on_top = TRUE)
# p
# 
ggsave(filename = here::here(latest_rf_cpi_figs_dir, "cpi_tcf.png"), plot = tcf_cpi_gg, height = 9, width = 14, units = "in")


#### Mediterranean Forest Woodland and Scrub

mfws_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_mfws, 
                             mapping = aes(y = factor(variable, levels = rev(cpi_summary_across_folds_mfws$variable)), 
                                           x = cpi, xmin = lwr, xmax = upr, color = type), size = 2) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  theme(legend.position = "none") + 
  facet_wrap(facets = "biome_fullname") +
  labs(y = "Driver variable",
       x = "Conditional predictive impact") +
  geom_point(data = cpi_summary_across_folds_mfws, mapping = aes(x = cpi_median, y = factor(variable, levels = rev(cpi_summary_across_folds_mfws$variable))), pch = "X", size = 2.5) +
  geom_vline(xintercept = 0)

# resolve_mfws <- 
#   resolve %>% 
#   dplyr::filter(ECO_NAME %in% cpi_mfws$eco_name_daily) 
# 
# newgeoms <- 
#   lapply(seq_along(1:nrow(resolve_mfws)), 
#          FUN = function(x) {
#            subgeoms <- 
#              resolve_mfws %>% 
#              dplyr::slice(x) %>% 
#              sf::st_cast() %>% 
#              dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
#              sf::st_union()
#          }) %>% 
#   do.call("rbind", .) %>% 
#   sf::st_sfc(crs = sf::st_crs(resolve_mfws))
# 
# resolve_mfws <- 
#   resolve_mfws %>% 
#   sf::st_set_geometry(value = newgeoms)
# 
# mfws_tmap <-
#   tm_shape(ca) +
#   tm_polygons() +
#   tm_shape(resolve_mfws) +
#   tm_fill("ECO_NAME", title = "Ecoregion name", palette = color_palette) +
#   tm_layout(legend.position = c("left", "bottom"), 
#             legend.width = 0.7, frame = FALSE)
# 
# mfws_tmap_grob <- tmap::tmap_grob(mfws_tmap)
# 
# p <- mfws_cpi_gg + patchwork::inset_element(p = mfws_tmap_grob, 
#                                             left = 0.5,
#                                             bottom = 0.025,
#                                             right = 0.995,
#                                             top = 0.675,
#                                             align_to = "panel",
#                                             on_top = TRUE)
# p
# 
ggsave(filename = here::here(latest_rf_cpi_figs_dir, "cpi_mfws.png"), plot = mfws_cpi_gg, height = 9, width = 14, units = "in")

# Temperate Grasslands, Savanna, and Shrublands
# 
# tgss_cpi_gg <-
#   ggplot() +
#   ggdist::geom_pointinterval(data = cpi_tgss, 
#                              mapping = aes(y = factor(variable, levels = rev(cpi_summary_across_folds_tgss$variable)), 
#                                            x = cpi, xmin = lwr, xmax = upr, color = spatial_fold), size = 2) +
#   scale_color_manual(values = color_palette) + 
#   theme_bw() +
#   theme(legend.position = "none") + 
#   facet_wrap(facets = "biome_fullname") +
#   labs(y = "Driver variable",
#        x = "Conditional predictive impact") +
#   geom_point(data = cpi_summary_across_folds_tgss, mapping = aes(x = cpi_median, y = factor(variable, levels = rev(cpi_summary_across_folds_tgss$variable))), pch = "X", size = 2.5)
# 
# 
# fold_assignments <- fold_lookup_tables[fold_lookup_tables$biome_shortname == "tgss", ]
# 
# tgss_spatial_folds <-
#   sf::st_read("data/out/fired_daily_ca_epsg3310_2003-2020.gpkg") %>% 
#   dplyr::filter(did %in% fold_assignments$did) %>% 
#   dplyr::left_join(fold_assignments, by = "did") %>% 
#   dplyr::group_by(spatial_fold) %>% 
#   dplyr::summarize() %>% 
#   sf::st_convex_hull()
#   
# tgss_tmap <-
#   tm_shape(ca) +
#   tm_polygons() +
#   tm_shape(tgss_spatial_folds) +
#   tm_fill("spatial_fold", title = "Spatial fold", palette = color_palette) +
#   tm_layout(legend.position = c(0.5, 0.8),  
#             legend.width = 0.7, frame = FALSE)
# 
# tgss_tmap_grob <- tmap::tmap_grob(tgss_tmap)
# 
# p <- tgss_cpi_gg + patchwork::inset_element(p = tgss_tmap_grob, 
#                                             left = 0.6,
#                                             bottom = 0.25,
#                                             right = 0.995,
#                                             top = 0.725,
#                                             align_to = "panel",
#                                             on_top = TRUE)
# p
# 
# ggsave(filename = "figs/rf/cpi/cpi_tgss.png", plot = p, height = 9, width = 14, units = "in")


# Deserts and xeric shrublands

dxs_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_dxs, 
                             mapping = aes(y = factor(variable, levels = rev(cpi_summary_across_folds_dxs$variable)), 
                                           x = cpi, xmin = lwr, xmax = upr, color = type), size = 2) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  theme(legend.position = "none") + 
  facet_wrap(facets = "biome_fullname") +
  labs(y = "Driver variable",
       x = "Conditional predictive impact") +
  geom_point(data = cpi_summary_across_folds_dxs, mapping = aes(x = cpi_median, y = factor(variable, levels = rev(cpi_summary_across_folds_dxs$variable))), pch = "X", size = 2.5) +
  geom_vline(xintercept = 0)

# resolve_dxs <- 
#   resolve %>% 
#   dplyr::filter(ECO_NAME %in% cpi_dxs$eco_name_daily) 
# 
# dxs_tmap <-
#   tm_shape(ca) +
#   tm_polygons() +
#   tm_shape(resolve_dxs) +
#   tm_fill("ECO_NAME", title = "Ecoregion name", palette = color_palette) +
#   tm_layout(legend.position = c("left", "bottom"), 
#             legend.width = 0.7, frame = FALSE)
# 
# dxs_tmap_grob <- tmap::tmap_grob(dxs_tmap)
# 
# p <- dxs_cpi_gg + patchwork::inset_element(p = dxs_tmap_grob, 
#                                            left = 0.5,
#                                            bottom = 0.025,
#                                            right = 0.995,
#                                            top = 0.675,
#                                            align_to = "panel",
#                                            on_top = TRUE)
# p
# 
ggsave(filename = here::here(latest_rf_cpi_figs_dir, "cpi_dxs.png"), plot = dxs_cpi_gg, height = 9, width = 14, units = "in")
