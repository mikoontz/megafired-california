library(terra)
library(dplyr)
library(data.table)
library(sf)

r <- terra::rast("data/out/ee/driver-layers/landforms.tif")
resolve <- sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% sf::st_transform(3310)

tcf <- resolve[resolve$BIOME_NAME == "Temperate Conifer Forests", ]
tcf <- terra::vect(tcf)

r_tcf <- 
  terra::crop(x = r, y = tcf) 

r_tcf <- terra::mask(x = r_tcf, mask = tcf)

plot(r_tcf)

DT <- as.data.frame(r_tcf, xy = TRUE, na.rm = TRUE)
DT <- 
  as.data.table(DT) %>% 
  setNames(c("x", "y", "landform"))

cell_count <- DT[, .(n = .N), by = landform]
cell_count[, tot := nrow(DT)]
cell_count[, prop := n / tot]


csp_ergo_landforms_desc <-
  tribble(~value, ~color,	~description,
          "11", "#141414", "Peak/ridge (warm)",
          "12", "#383838", "Peak/ridge",
          "13", "#808080"," Peak/ridge (cool)",
          "14", "#EBEB8F", "Mountain/divide",
          "15", "#F7D311", "Cliff",
          "21", "#AA0000", "Upper slope (warm)",
          "22", "#D89382", "Upper slope",
          "23", "#DDC9C9", "Upper slope (cool)",
          "24", "#DCCDCE", "Upper slope (flat)",
          "31", "#1C6330", "Lower slope (warm)",
          "32", "#68AA63", "Lower slope",
          "33", "#B5C98E", "Lower slope (cool)",
          "34", "#E1F0E5", "Lower slope (flat)",
          "41", "#a975ba", "Valley",
          "42", "#6f198c", "Valley (narrow)") %>% 
  dplyr::mutate(value = as.numeric(value))

table1 <- 
  cell_count %>% 
  left_join(csp_ergo_landforms_desc, by = c(landform = "value")) %>% 
  dplyr::slice(-1)

data.table::fwrite(x = table1, file = "data/out/landform-proportions_temperate-conifer-forests.csv")
