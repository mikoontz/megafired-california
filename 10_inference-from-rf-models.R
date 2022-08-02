library(readr)
library(dplyr)
library(spatialRF)
library(randomForestExplainer)
library(ggplot2)
library(patchwork)
library(ranger)

dir.create("figs/rf", showWarnings = FALSE)

# system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/rf  data/out/rf", stdout = TRUE)  

# biome_shortname <- "tcf"
# biome_shortname <- "mfws"
# biome_shortname <- "tgss"
# biome_shortname <- "dxs"

nonspatial_version <- "v8"
nonspatial_simple_version <- "v3"
nonspatial_repeat_version <- "v1"

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

for (counter in 1:4) {
  biome_shortname <- biome_shortnames[counter]
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
  
  rfspatial_model_transferability_gg <- biome_nonspatial_import$importance$cv.per.variable.plot
  
  # biome_nonspatial_simple <-
  #   list.files("data/out/rf", full.names = TRUE) %>% 
  #   data.frame(fname = .) %>% 
  #   dplyr::filter(grepl(pattern = nonspatial_simple_version, x = fname), 
  #                 grepl(pattern = "95th", x = fname),
  #                 grepl(pattern = "_nonspatial-simple", x = fname),
  #                 grepl(pattern = biome_shortname, x = fname)) %>%
  #   dplyr::pull(fname) %>% 
  #   readr::read_rds()
  # 
  # plot_response_curves(biome_nonspatial, quantiles = 0.5)
  
  # AUC of simple model versus one that includes fuel, topography, weather, and human factors
  # eval_out <- 
  #   rbind(
  #     biome_nonspatial$evaluation$aggregated %>% 
  #       dplyr::filter(model == "Testing",
  #                     metric %in% c("auc", "rmse")) %>% 
  #       dplyr::mutate(model = "complex"),
  #     biome_nonspatial_simple$evaluation$aggregated %>% 
  #       dplyr::filter(model == "Testing",
  #                     metric %in% c("auc", "rmse")) %>% 
  #       dplyr::mutate(model = "simple")
  #   )
  
  spatialrf_import_gg <- biome_nonspatial$importance$cv.per.variable.plot
  # biome_nonspatial <- spatialRF::rf_importance(model = biome_nonspatial, xy = biome_nonspatial$ranger.arguments$xy, metric = "auc")
  
  spatialrf_eval_gg <- spatialRF::plot_evaluation(biome_nonspatial, notch = FALSE)
  
  # https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html#variable-interactions
  min_depth_frame <- randomForestExplainer::min_depth_distribution(biome_nonspatial)
  
  min_depth_dist_gg <- randomForestExplainer::plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)
  
  importance_frame <- randomForestExplainer::measure_importance(biome_nonspatial)
  
  multiway_import_gg <- randomForestExplainer::plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
  
  (key_vars <- randomForestExplainer::important_variables(importance_frame, k = 10, measures = c("mean_min_depth", "no_of_trees")))
  
  interactions_frame <- randomForestExplainer::min_depth_interactions(forest = biome_nonspatial, vars = key_vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
  
  min_depth_interactions_gg <- randomForestExplainer::plot_min_depth_interactions(interactions_frame)
  
  interactions_frame_working <- 
    interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ] %>% 
    dplyr::slice(1:30) %>% 
    dplyr::filter(variable != as.character(root_variable)) %>% 
    dplyr::mutate(sorted_interaction = mapply(FUN = function(x, y) {
      paste(sort(c(x, y)), collapse = ":")
    }, 
    variable, as.character(root_variable))) %>% 
    dplyr::filter(!duplicated(sorted_interaction))
  
  biome_nonspatial$call <- str2lang(paste0("ranger::ranger(ewe ~ ", paste(biome_nonspatial$forest$independent.variable.names, collapse = " + "), ")"))
  
  n_pages <- 2
  n_per_page <- ceiling(nrow(interactions_frame_working) / n_pages)
  
  l <- lapply(1:n_pages, function(j) {
    sub_l <- lapply(1:n_per_page, function(i) {
      idx <- floor((j-1)*n_per_page + i) 
      print(idx)
      if(nrow(interactions_frame_working) >= idx) {
        interaction_plot <- randomForestExplainer::plot_predict_interaction(
          forest = biome_nonspatial,
          data = biome_nonspatial$ranger.arguments$data,
          variable1 = as.character(interactions_frame_working[idx, "root_variable"]),
          variable2 = as.character(interactions_frame_working[idx, "variable"]))
      } else interaction_plot <- patchwork::plot_spacer()
    })
  })
  
  p1 <- patchwork::wrap_plots(l[[1]], ncol = 3, nrow = 5)
  p2 <- patchwork::wrap_plots(l[[2]], ncol = 3, nrow = 5)
  
  
  ggplot2::ggsave(plot = rfspatial_model_transferability_gg, filename = paste0("figs/rf/", biome_shortname, "_model-transferability_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = spatialrf_eval_gg, filename = paste0("figs/rf/", biome_shortname, "_auc-r2-rmse_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = min_depth_dist_gg, filename = paste0("figs/rf/", biome_shortname, "_min-depth-distribution_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = multiway_import_gg, filename = paste0("figs/rf/", biome_shortname, "_multiway-importance_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = min_depth_interactions_gg, filename = paste0("figs/rf/", biome_shortname, "_min-depth-interactions_", nonspatial_version, ".png"))
  ggplot2::ggsave(filename = paste0("figs/rf/", biome_shortname, "_interactions_01_", nonspatial_version, ".png"), plot = p1, height = 15, width = 15, units = "in")
  ggplot2::ggsave(filename = paste0("figs/rf/", biome_shortname, "_interactions_02_", nonspatial_version, ".png"), plot = p2, height = 15, width = 15, units = "in")
}


nonspatial_model_out <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    biome_nonspatial <- list.files("data/out/rf", full.names = TRUE) %>% 
      data.frame(fname = .) %>% 
      dplyr::filter(grepl(pattern = "v9", x = fname), 
                    grepl(pattern = "95th", x = fname),
                    grepl(pattern = "_nonspatial_", x = fname),
                    grepl(pattern = biome_shortname, x = fname)) %>%
      dplyr::pull(fname) %>% 
      readr::read_rds()
    
    biome_nonspatial_simple <-
      list.files("data/out/rf", full.names = TRUE) %>% 
      data.frame(fname = .) %>% 
      dplyr::filter(grepl(pattern = "v3", x = fname), 
                    grepl(pattern = "95th", x = fname),
                    grepl(pattern = "_nonspatial-simple", x = fname),
                    grepl(pattern = biome_shortname, x = fname)) %>%
      dplyr::pull(fname) %>% 
      readr::read_rds()
    
    eval_out <- 
      rbind(
        biome_nonspatial$evaluation$aggregated %>% 
          dplyr::filter(model == "Testing",
                        metric %in% c("auc", "rmse")) %>% 
          dplyr::mutate(model = "complex"),
        biome_nonspatial_simple$evaluation$aggregated %>% 
          dplyr::filter(model == "Testing",
                        metric %in% c("auc", "rmse")) %>% 
          dplyr::mutate(model = "simple")
      )
    
    # model_compare <- spatialRF::rf_compare(models = list(complex = biome_nonspatial, simple = biome_nonspatial_simple),
    #                                        xy = biome_nonspatial$ranger.arguments$xy)
    # 
    # return(list(eval = eval_out, model_compare = model_compare))
  })

names(nonspatial_model_out) <- biome_shortnames




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
  n_pred <- 100
  n_rep <- 1000
  idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
  
  key_vars <- 
    biome_nonspatial_import$importance$per.variable %>% 
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
  
  newdata$ewe <- predict(biome_nonspatial, data = newdata)$predictions
  
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
  
  ggplot(newdata_agg, aes(x = x, y = ewe, ymin = lwr, ymax = upr)) +
    geom_line() +
    geom_ribbon(alpha = 0.25) +
    facet_wrap(facets = "target_var", scales = "free") +
    theme_bw()
  
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
  
  ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables-spaghetti_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = response_curves_highlight_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables-highlight_", nonspatial_version, ".png"))
  
  })


indep_vars <- 
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
  
  data.frame(biome = biome_shortname, driver = biome_nonspatial$forest$independent.variable.names)
})

predictor_table <- read.csv("tables/driver-descriptions.csv")
names(indep_vars) <- biome_shortnames

predictor_table[[biome_shortname]] <- predictor_table$driver %in% indep_vars[[biome_shortname]]$driver

sum(predictor_table$driver %in% indep_vars[[biome_shortname]]$driver)
nrow(indep_vars[[biome_shortname]])

indep_vars[[biome_shortname]]$driver[!(indep_vars[[biome_shortname]]$driver %in% predictor_table$driver)]
