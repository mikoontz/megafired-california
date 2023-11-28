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

latest_ard_date <- sort(list.files(path = here::here("data", "ard", "late")), 
                        decreasing = TRUE)[1]

rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", "late", latest_ard_date)
rf_cpi_figs_dir <- here::here("figs", "rf", "conditional-predictive-impact", "late", latest_ard_date)
rf_tables_dir <- here::here("tables", "rf", "late", latest_ard_date)

dir.create(rf_cpi_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(rf_cpi_figs_dir, showWarnings = FALSE, recursive = TRUE)

# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

# Full names of the biomes for the plot titles
biome_lookup <- c("Temperate Conifer Forests", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands")
names(biome_lookup) <- biome_shortnames

driver_description <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

col_pal_df <- 
  read.csv(here::here("data", "out", "driver-color-palette.csv"))

col_pal <- setNames(object = col_pal_df$hexcode, nm = col_pal_df$type)

cpi_results <- 
  data.table::fread(here::here(rf_cpi_dir, "cpi-important-variables_late.csv")) %>% 
  dplyr::rename(variable = Variable) %>% 
  dplyr::left_join(driver_description) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::group_split() %>% 
  lapply(FUN = function(x) {
    out <- 
      x %>% 
      dplyr::mutate(display_name = gsub(x = display_name, 
                                        pattern = "Standardized Precipitation-\nEvapotranspiration Index\n",
                                        replacement = "SPEI ")) %>% 
      dplyr::arrange(dplyr::desc(display_name))
    
    new_order <- 
      match(x = paste0("SPEI ", 
                       c("(14 days)", 
                         "(30 days)", 
                         "(90 days)", 
                         "(180 days)", 
                         "(270 days)", 
                         "(1 year)",
                         "(2 years)", 
                         "(5 years)")), 
            table = out$display_name)
    
    old_order <- sort(new_order, decreasing = TRUE)
    out[old_order, ] <- out[new_order, ]
    
    out <-
      out %>% 
      dplyr::mutate(variable = forcats::fct_inorder(variable),
                    display_name = forcats::fct_inorder(display_name))
    
    return(out)
  })

names(cpi_results) <- sapply(cpi_results, FUN = \(x) return(unique(x$biome)))
model_skill_results <- data.table::fread(here::here(rf_tables_dir, "model-skill-results_late.csv"))



# Interval plots
# Temperate Conifer Forests
tcf_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_results[["tcf"]],
                             mapping = aes(y = display_name, 
                                           x = cpi_mean, 
                                           xmin = cpi_lwr, 
                                           xmax = cpi_upr, 
                                           color = type), 
                             size = 2) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  labs(x = "Conditional predictive impact (units of MCC score)",
       y = NULL,
       color = "Driver type") +
  ggtitle(label = biome_lookup[match(x = unique((cpi_results[["tcf"]]$biome)), table = names(biome_lookup))])

tcf_cpi_gg

ggsave(filename = here::here(rf_cpi_figs_dir, "cpi_tcf.png"), plot = tcf_cpi_gg, height = 18, width = 10, units = "in")

#### Mediterranean Forest Woodland and Scrub

mfws_cpi_gg <-
  ggplot() +
  ggdist::geom_pointinterval(data = cpi_results[["mfws"]],
                             mapping = aes(y = display_name, 
                                           x = cpi_mean, 
                                           xmin = cpi_lwr, 
                                           xmax = cpi_upr, 
                                           color = type), 
                             size = 2) +
  scale_color_manual(values = col_pal) +
  theme_bw() +
  labs(x = "Conditional predictive impact (units of MCC score)",
       y = NULL,
       color = "Driver type") +
  ggtitle(label = biome_lookup[match(x = unique((cpi_results[["mfws"]]$biome)), table = names(biome_lookup))])

mfws_cpi_gg

ggsave(filename = here::here(rf_cpi_figs_dir, "cpi_mfws_late.png"), plot = mfws_cpi_gg, height = 18, width = 10, units = "in")
