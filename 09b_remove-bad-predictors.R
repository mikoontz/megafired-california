library(readr)
library(dplyr)
library(spatialRF)
library(randomForestExplainer)
library(ggplot2)
library(patchwork)
library(ranger)
library(sf)

dir.create("figs/rf", showWarnings = FALSE)

# system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/rf  data/out/rf", stdout = TRUE)  

# biome_shortname <- "tcf"
# biome_shortname <- "mfws"
# biome_shortname <- "tgss"
# biome_shortname <- "dxs"

nonspatial_version <- "v13"
nonspatial_simple_version <- "v3"

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

lapply(biome_shortnames, FUN = function(biome_shortname) {
  biome_nonspatial <-
    list.files("data/out/rf", full.names = TRUE) %>% 
    data.frame(fname = .) %>% 
    dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
                  grepl(pattern = "95th", x = fname),
                  grepl(pattern = "_nonspatial_", x = fname),
                  grepl(pattern = biome_shortname, x = fname)) %>%
    dplyr::pull(fname) %>% 
    readr::read_rds()
  
  biome_nonspatial_import <-
    list.files("data/out/rf", full.names = TRUE) %>% 
    data.frame(fname = .) %>% 
    dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
                  grepl(pattern = "95th", x = fname),
                  grepl(pattern = "_nonspatial-model-transferability_", x = fname),
                  grepl(pattern = biome_shortname, x = fname)) %>%
    dplyr::pull(fname) %>% 
    readr::read_rds()
  
  data <- biome_nonspatial$ranger.arguments$data
  indep_vars <- biome_nonspatial$forest$independent.variable.names
  # n_pred <- 100
  # n_rep <- 1000
  # idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
  
  bad_vars <- 
    biome_nonspatial_import$importance$per.variable %>% 
    dplyr::filter((importance.cv + importance.cv.mad) <= 0) %>% 
    dplyr::arrange(importance.cv) %>% 
    dplyr::pull(variable)
  
  new_indep_vars <- setdiff(indep_vars, bad_vars) 
  distance_thresholds = c(0, 1000, 2000, 4000, 8000, 16000, 32000, 64000)
  
  random_seed <- 2203
  xy <- biome_nonspatial$ranger.arguments$xy
  
  data_sf <-
    xy %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 3310, remove = FALSE)
  
  dist_mat <-
    sf::st_distance(x = data_sf,
                    y = data_sf) %>%
    units::drop_units()
  
  subversion_counter <- 1
  
  while(length(bad_vars) > 0) {
    new_version <- paste(nonspatial_version, subversion_counter, sep = ".")
    
    (start_time <- Sys.time())
    new_biome_nonspatial <- spatialRF::rf(
      data = data,
      dependent.variable.name = "ewe",
      predictor.variable.names = new_indep_vars,
      distance.matrix = dist_mat,
      distance.thresholds = distance_thresholds,
      xy = xy, #not needed by rf, but other functions read it from the model
      seed = random_seed,
      verbose = TRUE
    ) 
    
    new_biome_nonspatial <-
      spatialRF::rf_evaluate(
        model = new_biome_nonspatial,
        xy = xy,                  #data coordinates
        repetitions = 30,         #number of spatial folds
        training.fraction = 0.75, #training data fraction on each fold
        metrics = c("r.squared", "rmse", "auc"),
        seed = random_seed,
        verbose = TRUE
      )
    (end_time <- Sys.time())
    (difftime(time1 = end_time, time2 = start_time, units = "mins"))
    
    new_biome_nonspatial_import <- spatialRF::rf_importance(model = new_biome_nonspatial, xy = xy, metric = "rmse")
    
    readr::write_rds(x = new_biome_nonspatial, file = file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial_", new_version, ".rds")))
    
    readr::write_rds(x = new_biome_nonspatial_import, file = file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial-model-transferability_", new_version, ".rds")))
    
    bad_vars <- 
      new_biome_nonspatial_import$importance$per.variable %>% 
      dplyr::filter((importance.cv + importance.cv.mad) <= 0) %>% 
      dplyr::arrange(importance.cv) %>% 
      dplyr::pull(variable)
    
    new_indep_vars <- setdiff(new_indep_vars, bad_vars) 
    
    subversion_counter <- subversion_counter + 1
  }
  return(file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial_", new_version, ".rds")))
})