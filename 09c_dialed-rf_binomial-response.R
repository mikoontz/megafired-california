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

nonspatial_version <- "v10"
nonspatial_simple_version <- "v3"

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

lapply(biome_shortnames[-1], FUN = function(biome_shortname) {
  new_version <- paste0(nonspatial_version, "_dialed")
  
  biome_nonspatial <-
    list.files("data/out/rf", full.names = TRUE) %>% 
    data.frame(fname = .) %>% 
    dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
                  grepl(pattern = "95th", x = fname),
                  grepl(pattern = "_nonspatial_", x = fname),
                  grepl(pattern = biome_shortname, x = fname)) %>% 
    tidyr::separate(col = "fname", into = c("name", "subversion", "ext"), sep = "\\.", remove = FALSE, fill = "right") %>% 
    dplyr::mutate(subversion = ifelse(subversion == "rds", yes = "0", no = subversion)) %>% 
    dplyr::mutate(subversion = as.numeric(subversion)) %>% 
    dplyr::filter(subversion == max(subversion, na.rm = TRUE)) %>% 
    dplyr::pull(fname) %>%
    readr::read_rds()
  
  biome_nonspatial_import <-
    list.files("data/out/rf", full.names = TRUE) %>% 
    data.frame(fname = .) %>% 
    dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
                  grepl(pattern = "95th", x = fname),
                  grepl(pattern = "_nonspatial-model-transferability_", x = fname),
                  grepl(pattern = biome_shortname, x = fname)) %>% 
    tidyr::separate(col = "fname", into = c("name", "subversion", "ext"), sep = "\\.", remove = FALSE, fill = "right") %>% 
    dplyr::mutate(subversion = ifelse(subversion == "rds", yes = "0", no = subversion)) %>% 
    dplyr::mutate(subversion = as.numeric(subversion)) %>% 
    dplyr::filter(subversion == max(subversion, na.rm = TRUE)) %>% 
    dplyr::pull(fname) %>%
    readr::read_rds()
  
  data <- biome_nonspatial$ranger.arguments$data
  predictor.variable.names <- biome_nonspatial$forest$independent.variable.names
  
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
  
  (start_time <- Sys.time())
  new_biome_nonspatial <- spatialRF::rf(
    data = data,
    dependent.variable.name = "ewe",
    predictor.variable.names = predictor.variable.names,
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
  
  
  indep_vars <- new_biome_nonspatial$forest$independent.variable.names
  n_pred <- 100
  n_rep <- 1000
  idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
  
  key_vars <-
    new_biome_nonspatial_import$importance$per.variable %>%
    dplyr::filter((importance.cv - importance.cv.mad) >= 0) %>%
    dplyr::arrange(dplyr::desc(importance.cv)) %>%
    dplyr::pull(variable)
  
  newdata <- lapply(seq_along(key_vars), FUN = function(i) {
    other_vars <- setdiff(indep_vars, key_vars[i])
    newdata_x <- seq(min(data[[key_vars[i]]]), max(data[[key_vars[i]]]), length.out = n_pred)
    
    this_var_newdata <-
      lapply(seq_along(idx), FUN = function(j) {
        this_idx_newdata <-
          data.frame(target_var = key_vars[i], group = j, iter = 1:n_pred, x = newdata_x) %>%
          cbind(data[idx[j], other_vars], row.names = "iter") %>%
          dplyr::mutate(iter = 1:n_pred)
        
        names(this_idx_newdata)[names(this_idx_newdata) == "x"] <- key_vars[i]
        
        this_idx_newdata <- this_idx_newdata[, c("target_var", "group", "iter", indep_vars)]
        
      }) %>%
      do.call(what = "rbind", args = .)
  }) |>
    do.call("rbind", args = _)
  
  newdata$ewe <- predict(new_biome_nonspatial, data = newdata)$predictions
  
  newdata_agg <-
    split(x = newdata, f = list(newdata$target_var, newdata$iter)) |>
    lapply(FUN = function(x) {
      data.frame(target_var = unique(x$target_var),
                 iter = unique(x$iter),
                 x = unique(x[[unique(x$target_var)]]),
                 ewe = mean(x$ewe),
                 lwr = t.test(x = x$ewe)$conf.int[1],
                 upr = t.test(x = x$ewe)$conf.int[2])
    }) |>
    do.call("rbind", args = _) |>
    dplyr::mutate(target_var = factor(target_var, levels = key_vars))
  
  rownames(newdata_agg) <- 1:nrow(newdata_agg)
  
  newdata_plotting <-
    newdata[, c("target_var", "group", "iter", "ewe", key_vars)] %>%
    split(f = .$target_var) %>%
    lapply(FUN = function(x) {
      data.frame(target_var = x$target_var,
                 group = x$group,
                 iter = x$iter,
                 x = x[, unique(x$target_var)],
                 ewe = x$ewe)
    }) %>%
    do.call(what = "rbind", args = .)
  
  rownames(newdata_plotting) <- 1:nrow(newdata_plotting)
  newdata_plotting$target_var <- factor(newdata_plotting$target_var, levels = key_vars)
  
  response_curves_spaghetti_gg <-
    ggplot() +
    geom_line(data = newdata_plotting, aes(x = x, y = ewe, group = group), alpha = 0.1) +
    geom_line(data = newdata_agg, aes(x = x, y = ewe), color = "red") +
    # geom_ribbon(alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw()
  
  response_curves_highlight_gg <-
    ggplot(data = newdata_agg, aes(x = x, y = ewe, ymin = lwr, ymax = upr)) +
    geom_line(lwd = 1.25) +
    geom_ribbon(alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw()
  
  
  ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables-spaghetti_", new_version, ".png"))
  ggplot2::ggsave(plot = response_curves_highlight_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables-highlight_", new_version, ".png"))
  
  return(file.path("data", "out", "rf", paste0("rf_", biome_shortname, "_binomial-response-95th-pct-ewe_nonspatial_", new_version, ".rds")))
})
