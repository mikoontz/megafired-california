library(sf)
library(data.table)
library(dplyr)
library(USAboundaries)
library(tidyr)
library(pbapply)

# Expand the daily fire perimeters such that they are placed on top of 1000 random points (within each Resolve Biome)
ca <-   
  USAboundaries::us_states(resolution = "high", states = "California") %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_geometry()

ca_or_nv_az <- 
  USAboundaries::us_states(resolution = "high", states = c("California", "Oregon", "Nevada", "Arizona")) %>% 
  sf::st_transform(3310) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_union() %>% 
  sf::st_geometry()

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310) %>% 
  dplyr::select(BIOME_NAME, ECO_NAME) %>% 
  dplyr::rename_all(.funs = tolower) %>% 
  sf::st_set_agr(value = "constant") %>% 
  sf::st_intersection(y = ca)

lookup_table <- data.frame(biome_shortname = c("dxs", "mfws", "tcf", "tgss"), 
                           biome_name = c("Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands & Scrub", "Temperate Conifer Forests", "Temperate Grasslands, Savannas & Shrublands"))

random_fired_polys <- 
  list.files("data/out/fired_daily_random-locations/", full.names = TRUE, pattern = ".csv") %>% 
  pbsapply(FUN = function(x) {
    biome_shortname <- strsplit(x = basename(x), split = "_", fixed = TRUE)[[1]][4]
    biome_name <- lookup_table$biome_name[match(x = biome_shortname, table = lookup_table$biome_shortname)]
    
    this_df <- 
      data.table::fread(x) %>% 
      dplyr::mutate(biome_shortname = biome_shortname,
                    biome_name = biome_name)
    
    return(this_df)
    }, USE.NAMES = TRUE, simplify = FALSE) %>% 
  data.table::rbindlist() %>% 
  sf::st_as_sf(coords = c("x_samp", "y_samp"), remove = FALSE, crs = 3310)

ggplot() + 
  geom_sf(data = resolve) +
  geom_sf(data = random_fired_polys, aes(color = biome_name), pch = 19, size = 0.5)


