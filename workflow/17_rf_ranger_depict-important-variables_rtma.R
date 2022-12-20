# Plot the conditional predictive impact of variables

library(data.table)
library(dplyr)
library(ggplot2)
library(ggdist)
library(sf)
library(USAboundaries)
library(tmap)
library(patchwork)

dir.create("figs/rf/cpi", recursive = TRUE, showWarnings = FALSE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

# Full names of the biomes for the plot titles
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Temperate Grasslands, Savannas & Shrublands", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_lookup_tables <- 
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <- 
      read.csv(paste0("data/out/rf/tuning/spatial-fold-lookup-table_rtma_", biome_shortname, ".csv")) %>% 
      dplyr::arrange(spatial_fold)
  }) %>% 
  do.call("rbind", .)

# Spatial fold assignments for each biome based on the folds assigned during tuning
fold_n <- 
  fold_lookup_tables %>% 
  dplyr::group_by(biome_name_daily, biome_shortname, eco_name_daily, spatial_fold) %>% 
  dplyr::tally()

cpi <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    out <-
      data.table::fread(input = paste0("data/out/rf/conditional-predictive-impact/rf_ranger_variable-importance_rtma_cpi_classif-mcc_spatial-cv_", biome_shortname, ".csv"))
  }) %>% 
  data.table::rbindlist()

cpi_summary_across_iter <- 
  cpi %>% 
  dplyr::group_by(Variable, spatial_fold = id, biome) %>% 
  dplyr::summarize(cpi = mean(CPI, na.rm = TRUE),
                   sd = sd(CPI, na.rm = TRUE),
                   count = sum(!is.na(CPI)),
                   min = min(CPI, na.rm = TRUE),
                   max = max(CPI, na.rm = TRUE),
                   lwr = mean(CPI, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI))),
                   upr = mean(CPI, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(CPI)) - 1) * sd(CPI, na.rm = TRUE) / sqrt(sum(!is.na(CPI)))) %>%
  dplyr::left_join(fold_n, by = c(biome = "biome_shortname", "spatial_fold")) %>%
  dplyr::arrange(biome, Variable, spatial_fold) %>% 
  dplyr::mutate(biome_fullname = biome_lookup[match(biome, names(biome_lookup))])

cpi_summary_across_folds <-
  cpi_summary_across_iter %>% 
  dplyr::group_by(Variable, biome) %>% 
  dplyr::summarize(n_pos = length(which(cpi > 0)),
                   n_neg = length(which(cpi < 0)),
                   n_0 = length(which(cpi == 0)),
                   cpi_mean = mean(x = cpi),
                   cpi_lwr = mean(x = lwr),
                   cpi_upr = mean(x = upr),
                   cpi_wgt_mean = weighted.mean(x = cpi, w = n),
                   cpi_wgt_lwr = weighted.mean(x = lwr, w = n),
                   cpi_wgt_upr = weighted.mean(x = upr, w = n),
                   cpi_median = median(x = cpi),
                   n = n()) %>% 
  dplyr::arrange(biome, desc(cpi_median), desc(cpi_wgt_mean))

cpi_tcf <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "tcf", ]
cpi_mfws <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "mfws", ]
# cpi_tgss <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "tgss", ]
cpi_dxs <- cpi_summary_across_iter[cpi_summary_across_iter$biome == "dxs", ]

cpi_summary_across_folds_tcf <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "tcf", ]
cpi_summary_across_folds_mfws <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "mfws", ]
# cpi_summary_across_folds_tgss <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "tgss", ]
cpi_summary_across_folds_dxs <- cpi_summary_across_folds[cpi_summary_across_folds$biome == "dxs", ]

color_palette <- c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')

ca <-
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_geometry()

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  sf::st_intersection(y = ca)

# Interval plots + maps
# Temperate Conifer Forests
tcf_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_tcf, 
                             mapping = aes(y = factor(Variable, levels = rev(cpi_summary_across_folds_tcf$Variable)), 
                                           x = cpi, xmin = lwr, xmax = upr, color = eco_name_daily), size = 2) +
  scale_color_manual(values = color_palette) + 
  theme_bw() +
  theme(legend.position = "none") + 
  facet_wrap(facets = "biome_fullname") +
  labs(y = "Driver variable",
       x = "Conditional predictive impact") +
  geom_point(data = cpi_summary_across_folds_tcf, mapping = aes(x = cpi_median, y = factor(Variable, levels = rev(cpi_summary_across_folds_tcf$Variable))), pch = "X", size = 2.5)

resolve_tcf <- 
  resolve %>% 
  dplyr::filter(ECO_NAME %in% cpi_tcf$eco_name_daily)

tcf_tmap <-
  tm_shape(ca) +
  tm_polygons() +
  tm_shape(resolve_tcf) +
  tm_fill("ECO_NAME", title = "Ecoregion name", palette = color_palette) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.width = 0.7, frame = FALSE)

tcf_tmap_grob <- tmap::tmap_grob(tcf_tmap)

p <- tcf_cpi_gg + patchwork::inset_element(p = tcf_tmap_grob, 
                                           left = 0.5,
                                           bottom = 0.025,
                                           right = 0.995,
                                           top = 0.675,
                                           align_to = "panel",
                                           on_top = TRUE)
p

ggsave(filename = "figs/rf/cpi/cpi_tcf.png", plot = p, height = 9, width = 14, units = "in")


#### Mediterranean Forest Woodland and Scrub

mfws_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_mfws, 
                             mapping = aes(y = factor(Variable, levels = rev(cpi_summary_across_folds_mfws$Variable)), 
                                           x = cpi, xmin = lwr, xmax = upr, color = eco_name_daily), size = 2) +
  scale_color_manual(values = color_palette) + 
  theme_bw() +
  theme(legend.position = "none") + 
  facet_wrap(facets = "biome_fullname") +
  labs(y = "Driver variable",
       x = "Conditional predictive impact") +
  geom_point(data = cpi_summary_across_folds_mfws, mapping = aes(x = cpi_median, y = factor(Variable, levels = rev(cpi_summary_across_folds_mfws$Variable))), pch = "X", size = 2.5)

resolve_mfws <- 
  resolve %>% 
  dplyr::filter(ECO_NAME %in% cpi_mfws$eco_name_daily) 

newgeoms <- 
  lapply(seq_along(1:nrow(resolve_mfws)), 
         FUN = function(x) {
           subgeoms <- 
             resolve_mfws %>% 
             dplyr::slice(x) %>% 
             sf::st_cast() %>% 
             dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>% 
             sf::st_union()
         }) %>% 
  do.call("rbind", .) %>% 
  sf::st_sfc(crs = sf::st_crs(resolve_mfws))

resolve_mfws <- 
  resolve_mfws %>% 
  sf::st_set_geometry(value = newgeoms)

mfws_tmap <-
  tm_shape(ca) +
  tm_polygons() +
  tm_shape(resolve_mfws) +
  tm_fill("ECO_NAME", title = "Ecoregion name", palette = color_palette) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.width = 0.7, frame = FALSE)

mfws_tmap_grob <- tmap::tmap_grob(mfws_tmap)

p <- mfws_cpi_gg + patchwork::inset_element(p = mfws_tmap_grob, 
                                            left = 0.5,
                                            bottom = 0.025,
                                            right = 0.995,
                                            top = 0.675,
                                            align_to = "panel",
                                            on_top = TRUE)
p

ggsave(filename = "figs/rf/cpi/cpi_mfws.png", plot = p, height = 9, width = 14, units = "in")

# Temperate Grasslands, Savanna, and Shrublands
# 
# tgss_cpi_gg <-
#   ggplot() +
#   ggdist::geom_pointinterval(data = cpi_tgss, 
#                              mapping = aes(y = factor(Variable, levels = rev(cpi_summary_across_folds_tgss$Variable)), 
#                                            x = cpi, xmin = lwr, xmax = upr, color = spatial_fold), size = 2) +
#   scale_color_manual(values = color_palette) + 
#   theme_bw() +
#   theme(legend.position = "none") + 
#   facet_wrap(facets = "biome_fullname") +
#   labs(y = "Driver variable",
#        x = "Conditional predictive impact") +
#   geom_point(data = cpi_summary_across_folds_tgss, mapping = aes(x = cpi_median, y = factor(Variable, levels = rev(cpi_summary_across_folds_tgss$Variable))), pch = "X", size = 2.5)
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
                             mapping = aes(y = factor(Variable, levels = rev(cpi_summary_across_folds_dxs$Variable)), 
                                           x = cpi, xmin = lwr, xmax = upr, color = eco_name_daily), size = 2) +
  scale_color_manual(values = color_palette) + 
  theme_bw() +
  theme(legend.position = "none") + 
  facet_wrap(facets = "biome_fullname") +
  labs(y = "Driver variable",
       x = "Conditional predictive impact") +
  geom_point(data = cpi_summary_across_folds_dxs, mapping = aes(x = cpi_median, y = factor(Variable, levels = rev(cpi_summary_across_folds_dxs$Variable))), pch = "X", size = 2.5)

resolve_dxs <- 
  resolve %>% 
  dplyr::filter(ECO_NAME %in% cpi_dxs$eco_name_daily) 

dxs_tmap <-
  tm_shape(ca) +
  tm_polygons() +
  tm_shape(resolve_dxs) +
  tm_fill("ECO_NAME", title = "Ecoregion name", palette = color_palette) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.width = 0.7, frame = FALSE)

dxs_tmap_grob <- tmap::tmap_grob(dxs_tmap)

p <- dxs_cpi_gg + patchwork::inset_element(p = dxs_tmap_grob, 
                                           left = 0.5,
                                           bottom = 0.025,
                                           right = 0.995,
                                           top = 0.675,
                                           align_to = "panel",
                                           on_top = TRUE)
p

ggsave(filename = "figs/rf/cpi/cpi_dxs.png", plot = p, height = 9, width = 14, units = "in")
