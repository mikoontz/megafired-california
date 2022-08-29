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

nonspatial_version <- "v15_dialed"
nonspatial_simple_version <- "v3"

biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")

# ### Importance values
# party_imp <- permimp::permimp(object = party_model, conditional = TRUE, AUC = TRUE, threshold = 0.8)
# party_imp <- permimp::permimp(object = party_model, nperm = 5, conditional = TRUE, AUC = TRUE, threshold = 0.8)
# 
# # https://link.springer.com/article/10.1007/s11634-016-0276-4
# imp_p <- vita::NTA(as.matrix(party_imp$values))
# summary(imp_p)$cmat %>% 
#   as.data.frame() %>% 
#   dplyr::filter(`p-value` <= 0.05) %>% 
#   dplyr::arrange(desc(`CV-PerVarImp`))
# imp_p$PerVarImp
# imp_p$pvalue
# imp_p_sum 
# CV-PerVarImp    p-value
# barren_grass_forb_herb_mix 0.0155671932 0.00000000
# mountain_divide            0.0077333469 0.00000000
# barren_shrub_mix           0.0044383284 0.00000000
# cliff                      0.0034996408 0.00000000
# road_density_mpha          0.0030029846 0.00000000
# sqrt_aoi_tm1               0.0026617807 0.00000000
# peak_ridge_cool            0.0010744355 0.00000000
# barren_trees_mix           0.0010246360 0.00000000
# ndvi                       0.0004494767 0.00000000
# wind_terrain_anisotropy    0.0004198664 0.00000000
# max_soil_water_pct         0.0004122641 0.00000000
# fuel_slow_loss             0.0003827295 0.00000000
# spei30d                    0.0002887751 0.00000000
# fm100_pct                  0.0002763040 0.00000000
# min_vpd_pct                0.0002421228 0.00000000
# upper_slope_cool           0.0002288020 0.02631579
# random_nums <-
#   matrix(data = runif(n = nrow(data) * 100, min = -10, max = 10), nrow = nrow(data), ncol = 100) %>%
#   as.data.frame()
# 
# indep_vars <- c(predictor.variable.names_reduced, names(random_nums))
# rf_formula <- as.formula(paste0("ewe ~ ", paste(indep_vars, collapse = " + ")))
# 
# data_aug <- cbind(data, random_nums)
# # Conditional permutation importance for dealing with multicollinear predictors
# # https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-9-307
# 
# (start_time <- Sys.time())
# party_model <- party::cforest(formula = rf_formula, data = data_aug, control = cforest_unbiased())
# (end_time <- Sys.time())
# (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 
# # AUC-based permutation importance because it is more robust to class imbalance
# # https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-119
# (start_time <- Sys.time())
# # party_imp <- party::varimpAUC(party_model, conditional = TRUE)
# # party_imp <- permimp::permimp(object = party_model, conditional = TRUE, AUC = TRUE, threshold = 0.95)
# party_imp <- permimp::permimp(object = party_model, conditional = TRUE, AUC = TRUE, threshold = 0.8)
# (end_time <- Sys.time())
# (difftime(time1 = end_time, time2 = start_time, units = "mins"))
# 
# # https://link.springer.com/article/10.1007/s11634-016-0276-4
# imp_p <- vita::NTA(as.matrix(party_imp$values))
# summary(imp_p)$cmat %>% 
#   as.data.frame() %>% 
#   dplyr::filter(`p-value` <= 0.05) %>% 
#   dplyr::arrange(desc(`CV-PerVarImp`))
# imp_p$PerVarImp
# imp_p$pvalue
# imp_p_sum 
# 

## Some importance-like plots

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
  
  spatialrf_import_gg <- biome_nonspatial$importance$cv.per.variable.plot
  # biome_nonspatial <- spatialRF::rf_importance(model = biome_nonspatial, xy = biome_nonspatial$ranger.arguments$xy, metric = "auc")
  
  spatialrf_eval_gg <- spatialRF::plot_evaluation(biome_nonspatial, notch = FALSE)
  
  # https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html#variable-interactions
  min_depth_frame <- randomForestExplainer::min_depth_distribution(biome_nonspatial)
  
  min_depth_dist_gg <- randomForestExplainer::plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)
  
  importance_frame <- randomForestExplainer::measure_importance(biome_nonspatial)
  
  multiway_import_gg <- randomForestExplainer::plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")
  
  ggplot2::ggsave(plot = rfspatial_model_transferability_gg, filename = paste0("figs/rf/", biome_shortname, "_model-transferability_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = spatialrf_eval_gg, filename = paste0("figs/rf/", biome_shortname, "_auc-r2-rmse_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = min_depth_dist_gg, filename = paste0("figs/rf/", biome_shortname, "_min-depth-distribution_", nonspatial_version, ".png"))
  ggplot2::ggsave(plot = multiway_import_gg, filename = paste0("figs/rf/", biome_shortname, "_multiway-importance_", nonspatial_version, ".png"))
}

# Some interaction-like plots
for (counter in c(1:4)) {
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
  
  # (key_vars <- randomForestExplainer::important_variables(importance_frame, k = 10, measures = c("mean_min_depth", "no_of_trees")))
  key_vars <-
    biome_nonspatial_import$importance$per.variable %>%
    dplyr::filter((importance.cv - importance.cv.mad) >= 0) %>%
    dplyr::arrange(dplyr::desc(importance.cv)) %>%
    dplyr::pull(variable)
  
  interactions_frame <- randomForestExplainer::min_depth_interactions(forest = biome_nonspatial, 
                                                                      vars = key_vars, 
                                                                      mean_sample = "relevant_trees", 
                                                                      uncond_mean_sample = "relevant_trees")
  
  interactions_frame_working <- 
    interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ] %>% 
    dplyr::slice(1:30) %>% 
    dplyr::filter(variable != as.character(root_variable)) %>% 
    dplyr::mutate(sorted_interaction = mapply(FUN = function(x, y) {
      paste(sort(c(x, y)), collapse = ":")
    }, 
    variable, as.character(root_variable))) %>% 
    dplyr::filter(!duplicated(sorted_interaction)) %>% 
    dplyr::mutate(interaction_eff_size = uncond_mean_min_depth - mean_min_depth) %>% 
    dplyr::arrange(desc(abs(interaction_eff_size))) %>% 
    dplyr::filter(interaction_eff_size > 0.5)
  
  # Customized code from plot_min_depth_interactions() function
  # min_depth_interactions_gg <- randomForestExplainer::plot_min_depth_interactions(interactions_frame_working)
  new_levels <- interactions_frame_working[order(abs(interactions_frame_working$interaction_eff_size), decreasing = TRUE), "interaction"]
  interactions_frame_working$interaction <- factor(interactions_frame_working$interaction, levels = new_levels)
  # minimum <- min(interactions_frame$mean_min_depth, na.rm = TRUE)
  
  # k <- sum(abs(interactions_frame_working$interaction_eff_size > 0.5))
  min_depth_interactions_gg <- 
    ggplot(interactions_frame_working, aes(x = interaction, y = mean_min_depth, fill = occurrences)) + 
    geom_bar(stat = "identity") + 
    geom_pointrange(aes(ymin = pmin(mean_min_depth, uncond_mean_min_depth), y = uncond_mean_min_depth, 
                        ymax = pmax(mean_min_depth, uncond_mean_min_depth), 
                        shape = "unconditional"), fatten = 2, size = 1) + 
    # geom_hline(aes(yintercept = minimum, linetype = "minimum"), color = "red", size = 1.5) + 
    scale_linetype_manual(name = NULL, values = 1) + 
    theme_bw() + scale_shape_manual(name = NULL, values = 19) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  biome_nonspatial$call <- str2lang(paste0("ranger::ranger(ewe ~ ", paste(biome_nonspatial$forest$independent.variable.names, collapse = " + "), ")"))
  
  n_pages <- ceiling(nrow(interactions_frame_working) / 9)
  n_per_page <- ceiling(nrow(interactions_frame_working) / n_pages)
  
  l <- lapply(1:n_pages, function(j) {
    sub_l <- lapply(1:n_per_page, function(i) {
      idx <- floor((j-1)*n_per_page + i) 
      print(idx)
      if(nrow(interactions_frame_working) >= idx) {
        interaction_plot <- 
          randomForestExplainer::plot_predict_interaction(
            forest = biome_nonspatial,
            data = biome_nonspatial$ranger.arguments$data,
            variable1 = as.character(interactions_frame_working[idx, "root_variable"]),
            variable2 = as.character(interactions_frame_working[idx, "variable"])) +
          ggtitle(label = NULL) +
          labs(fill = "P(ewe)")
      } else interaction_plot <- patchwork::plot_spacer()
    })
  })
  
  for (p in seq_along(l)) {
    p1 <- patchwork::wrap_plots(l[[p]], ncol = 3, nrow = 3) + patchwork::plot_annotation(title = "Model predictions as a function of additively interacting covariates")
    ggplot2::ggsave(filename = paste0("figs/rf/", biome_shortname, "_interactions_", stringr::str_pad(string = p, width = 2, side = "left", pad = "0"), "_", nonspatial_version, ".png"), plot = p1, width = 17, height = 15, units = "in", )
    
  }
  
}


nonspatial_model_out <-
  lapply(biome_shortnames, FUN = function(biome_shortname) {
    biome_nonspatial <- list.files("data/out/rf", full.names = TRUE) %>% 
      data.frame(fname = .) %>% 
      dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
                    grepl(pattern = "95th", x = fname),
                    grepl(pattern = "_nonspatial_", x = fname),
                    grepl(pattern = biome_shortname, x = fname)) %>%
      dplyr::pull(fname) %>% 
      readr::read_rds()
    
    eval_out <- 
      rbind(
        biome_nonspatial$evaluation$aggregated %>% 
          dplyr::filter(model == "Testing",
                        metric %in% c("auc", "rmse")) %>% 
          dplyr::mutate(model = "complex")
      )
    
    # biome_nonspatial_simple <-
    #   list.files("data/out/rf", full.names = TRUE) %>% 
    #   data.frame(fname = .) %>% 
    #   dplyr::filter(grepl(pattern = nonspatial_simple_version, x = fname), 
    #                 grepl(pattern = "95th", x = fname),
    #                 grepl(pattern = "_nonspatial-simple", x = fname),
    #                 grepl(pattern = biome_shortname, x = fname)) %>%
    #   dplyr::pull(fname) %>% 
    #   readr::read_rds()
    
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
    # 
    # model_compare <- spatialRF::rf_compare(models = list(complex = biome_nonspatial, simple = biome_nonspatial_simple),
    #                                        xy = biome_nonspatial$ranger.arguments$xy)
    # 
    # return(list(eval = eval_out, model_compare = model_compare))
  })

names(nonspatial_model_out) <- biome_shortnames

# lapply(biome_shortnames, FUN = function(biome_shortname) {
#   biome_nonspatial <-
#     list.files("data/out/rf", full.names = TRUE) %>% 
#     data.frame(fname = .) %>% 
#     dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
#                   grepl(pattern = "95th", x = fname),
#                   grepl(pattern = "_nonspatial_", x = fname),
#                   grepl(pattern = biome_shortname, x = fname)) %>%
#     dplyr::pull(fname) %>% 
#     readr::read_rds()
#   
#   biome_nonspatial_import <-
#     list.files("data/out/rf", full.names = TRUE) %>% 
#     data.frame(fname = .) %>% 
#     dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
#                   grepl(pattern = "95th", x = fname),
#                   grepl(pattern = "_nonspatial-model-transferability_", x = fname),
#                   grepl(pattern = biome_shortname, x = fname)) %>%
#     dplyr::pull(fname) %>% 
#     readr::read_rds()
#   
#   data <- biome_nonspatial$ranger.arguments$data
#   indep_vars <- biome_nonspatial$forest$independent.variable.names
#   n_pred <- 100
#   n_rep <- 1000
#   idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
#   
#   key_vars <- 
#     biome_nonspatial_import$importance$per.variable %>% 
#     dplyr::filter((importance.cv - importance.cv.mad) >= 0) %>% 
#     dplyr::arrange(dplyr::desc(importance.cv)) %>% 
#     dplyr::pull(variable)
#   
#   newdata <- lapply(seq_along(key_vars), FUN = function(i) {
#     other_vars <- setdiff(indep_vars, key_vars[i])
#     newdata_x <- seq(min(data[[key_vars[i]]]), max(data[[key_vars[i]]]), length.out = n_pred)
#     
#     this_var_newdata <- 
#       lapply(seq_along(idx), FUN = function(j) {
#         this_idx_newdata <- 
#           data.frame(target_var = key_vars[i], group = j, iter = 1:n_pred, x = newdata_x) %>% 
#           cbind(data[idx[j], other_vars], row.names = "iter") %>% 
#           dplyr::mutate(iter = 1:n_pred)
#         
#         names(this_idx_newdata)[names(this_idx_newdata) == "x"] <- key_vars[i]
#         
#         this_idx_newdata <- this_idx_newdata[, c("target_var", "group", "iter", indep_vars)]
#         
#       }) %>% 
#       do.call(what = "rbind", args = .)
#   }) |> 
#     do.call("rbind", args = _)
#   
#   newdata$ewe <- predict(biome_nonspatial, data = newdata)$predictions
#   
#   newdata_agg <-
#     split(x = newdata, f = list(newdata$target_var, newdata$iter)) |> 
#     lapply(FUN = function(x) {
#       data.frame(target_var = unique(x$target_var),
#                  iter = unique(x$iter),
#                  x = unique(x[[unique(x$target_var)]]),
#                  ewe = mean(x$ewe),
#                  lwr = t.test(x = x$ewe)$conf.int[1],
#                  upr = t.test(x = x$ewe)$conf.int[2])
#     }) |>
#     do.call("rbind", args = _) |>
#     dplyr::mutate(target_var = factor(target_var, levels = key_vars))
#   
#   rownames(newdata_agg) <- 1:nrow(newdata_agg) 
#   
#   ggplot(newdata_agg, aes(x = x, y = ewe, ymin = lwr, ymax = upr)) +
#     geom_line() +
#     geom_ribbon(alpha = 0.25) +
#     facet_wrap(facets = "target_var", scales = "free") +
#     theme_bw()
#   
#   newdata_plotting <- 
#     newdata[, c("target_var", "group", "iter", "ewe", key_vars)] %>% 
#     split(f = .$target_var) %>% 
#     lapply(FUN = function(x) {
#       data.frame(target_var = x$target_var,
#                  group = x$group,
#                  iter = x$iter,
#                  x = x[, unique(x$target_var)],
#                  ewe = x$ewe)
#     }) %>% 
#     do.call(what = "rbind", args = .)
#   
#   rownames(newdata_plotting) <- 1:nrow(newdata_plotting)
#   newdata_plotting$target_var <- factor(newdata_plotting$target_var, levels = key_vars)
#   
#   response_curves_spaghetti_gg <- 
#     ggplot() +
#     geom_line(data = newdata_plotting, aes(x = x, y = ewe, group = group), alpha = 0.1) +
#     geom_line(data = newdata_agg, aes(x = x, y = ewe), color = "red") +
#     # geom_ribbon(alpha = 0.25) +
#     facet_wrap(facets = "target_var", scales = "free") +
#     theme_bw()
#   
#   response_curves_highlight_gg <- 
#     ggplot(data = newdata_agg, aes(x = x, y = ewe, ymin = lwr, ymax = upr)) +
#     geom_line(lwd = 1.25) +
#     geom_ribbon(alpha = 0.25) +
#     facet_wrap(facets = "target_var", scales = "free") +
#     theme_bw()
#   
#   ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables-spaghetti_", nonspatial_version, ".png"))
#   ggplot2::ggsave(plot = response_curves_highlight_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-important-variables-highlight_", nonspatial_version, ".png"))
#   
# })


# # any variables we should drop from the model?
# model_checking <- 
#   lapply(biome_shortnames, FUN = function(biome_shortname) {
#     biome_nonspatial <-
#       list.files("data/out/rf", full.names = TRUE) %>% 
#       data.frame(fname = .) %>% 
#       dplyr::filter(grepl(pattern = nonspatial_version, x = fname), 
#                     grepl(pattern = "95th", x = fname),
#                     grepl(pattern = "_nonspatial_", x = fname),
#                     grepl(pattern = biome_shortname, x = fname)) %>%
#       dplyr::pull(fname) %>% 
#       readr::read_rds()
#     
#     data <- biome_nonspatial$ranger.arguments$data 
#     case.wgts <- spatialRF::case_weights(data = data, dependent.variable.name = "ewe")
#     data <-
#       data %>% 
#       dplyr::mutate(ewe = factor(ewe, levels = c(0, 1)))
#     
#     indep_vars <- biome_nonspatial$forest$independent.variable.names
#     n_pred <- 100
#     n_rep <- 1000
#     idx <- sample(x = nrow(data), size = n_rep, replace = TRUE)
#     
#     ranger_formula <- as.formula(paste0("ewe ~ ", paste(indep_vars, collapse = " + ")))
#     ranger_equivalent <- ranger::ranger(formula = ranger_formula,
#                                         data = data,
#                                         classification = TRUE,
#                                         probability = TRUE,
#                                         class.weights = unique(case.wgts),
#                                         importance = "permutation",
#                                         seed = 1)
#     
#     ranger_import_pval <- ranger::importance_pvalues(x = ranger_equivalent, formula = ranger_formula, data = data, method = "altmann")
#     key_vars <- 
#       ranger_import_pval %>% 
#       as.data.frame() %>% 
#       filter(pvalue < 0.05) %>% 
#       rownames()
# 
#     newdata <- lapply(seq_along(key_vars), FUN = function(i) {
#       other_vars <- setdiff(indep_vars, key_vars[i])
#       newdata_x <- seq(min(data[[key_vars[i]]]), max(data[[key_vars[i]]]), length.out = n_pred)
#       
#       this_var_newdata <- 
#         lapply(seq_along(idx), FUN = function(j) {
#           this_idx_newdata <- 
#             data.frame(target_var = key_vars[i], group = j, iter = 1:n_pred, x = newdata_x) %>% 
#             cbind(data[idx[j], other_vars], row.names = "iter") %>% 
#             dplyr::mutate(iter = 1:n_pred)
#           
#           names(this_idx_newdata)[names(this_idx_newdata) == "x"] <- key_vars[i]
#           
#           this_idx_newdata <- this_idx_newdata[, c("target_var", "group", "iter", indep_vars)]
#           
#         }) %>% 
#         do.call(what = "rbind", args = .)
#     }) |> 
#       do.call("rbind", args = _)
#     
#     newdata$ewe <- predict(ranger_equivalent, data = newdata)$predictions[, 2]
#     # newdata$ewe <- predict(biome_nonspatial, data = newdata)$predictions
#     
#     newdata_agg <-
#       split(x = newdata, f = list(newdata$target_var, newdata$iter)) |> 
#       lapply(FUN = function(x) {
#         data.frame(target_var = unique(x$target_var),
#                    iter = unique(x$iter),
#                    x = unique(x[[unique(x$target_var)]]),
#                    ewe = mean(x$ewe),
#                    lwr = t.test(x = x$ewe)$conf.int[1],
#                    upr = t.test(x = x$ewe)$conf.int[2])
#       }) |>
#       do.call("rbind", args = _) |>
#       dplyr::mutate(target_var = factor(target_var, levels = key_vars))
#     
#     rownames(newdata_agg) <- 1:nrow(newdata_agg) 
#     
#     ggplot(newdata_agg, aes(x = x, y = ewe, ymin = lwr, ymax = upr)) +
#       geom_line() +
#       geom_ribbon(alpha = 0.25) +
#       facet_wrap(facets = "target_var", scales = "free") +
#       theme_bw()
#     
#     newdata_plotting <- 
#       newdata[, c("target_var", "group", "iter", "ewe", key_vars)] %>% 
#       split(f = .$target_var) %>% 
#       lapply(FUN = function(x) {
#         data.frame(target_var = x$target_var,
#                    group = x$group,
#                    iter = x$iter,
#                    x = x[, unique(x$target_var)],
#                    ewe = x$ewe)
#       }) %>% 
#       do.call(what = "rbind", args = .)
#     
#     rownames(newdata_plotting) <- 1:nrow(newdata_plotting)
#     newdata_plotting$target_var <- factor(newdata_plotting$target_var, levels = key_vars)
#     
#     response_curves_spaghetti_gg <- 
#       ggplot() +
#       geom_line(data = newdata_plotting, aes(x = x, y = ewe, group = group), alpha = 0.1) +
#       geom_line(data = newdata_agg, aes(x = x, y = ewe), color = "red") +
#       # geom_ribbon(alpha = 0.25) +
#       facet_wrap(facets = "target_var", scales = "free") +
#       theme_bw()
#     
#     response_curves_highlight_gg <- 
#       ggplot(data = newdata_agg, aes(x = x, y = ewe, ymin = lwr, ymax = upr)) +
#       geom_line(lwd = 1.25) +
#       geom_ribbon(alpha = 0.25) +
#       facet_wrap(facets = "target_var", scales = "free") +
#       theme_bw()
#     
#     ggplot2::ggsave(plot = response_curves_spaghetti_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-ranger-important-variables-spaghetti_", nonspatial_version, ".png"))
#     ggplot2::ggsave(plot = response_curves_highlight_gg, filename = paste0("figs/rf/", biome_shortname, "_response-curves-ranger-important-variables-highlight_", nonspatial_version, ".png"))
#     
#     data.frame(biome = biome_shortname, driver = biome_nonspatial$forest$independent.variable.names)
#   })
# 




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



# Includes Sobol-MDA approach
# https://github.com/clementbenard/ranger/commit/75c0f90c1c85581ace5dc3f39ea32c62a94be2c5
# remotes::install_github(repo = "clementbenard/ranger", ref = "75c0f90c1c85581ace5dc3f39ea32c62a94be2c5")
# remotes::install_github(repo = "clementbenard/ranger")

party_imp <- permimp::permimp(object = final_party_model, conditional = TRUE, AUC = TRUE, threshold = 0.8)
# 
# # # https://link.springer.com/article/10.1007/s11634-016-0276-4
# imp_p <- vita::NTA(as.matrix(party_imp$values))
# summary(imp_p)$cmat %>%
#   as.data.frame() %>%
#   dplyr::filter(`p-value` <= 0.05) %>%
#   dplyr::arrange(desc(`CV-PerVarImp`))

# CV-PerVarImp    p-value
# barren_grass_forb_herb_mix_tm01 6.388551e-04 0.00000000
# sqrt_aoi_tm1                    1.009734e-04 0.00000000
# barren_shrub_mix_tm01           1.000166e-04 0.00000000
# mountain_divide                 8.034414e-05 0.00000000
# road_density_mpha               5.592944e-05 0.00000000
# spei5y                          5.199631e-05 0.00000000
# cliff                           1.960194e-05 0.00000000
# peak_ridge_cool                 1.912447e-05 0.00000000
# barren_trees_mix_tm01           1.877983e-05 0.00000000
# min_wind_speed_pct              1.755426e-05 0.00000000
# change_diversity_tm01           1.508939e-05 0.00000000
# wind_terrain_anisotropy         1.456523e-05 0.00000000
# fuel_slow_loss_tm01             1.151352e-05 0.00000000
# fuel_stable_tm01                1.130654e-05 0.00000000
# concurrent_fires                9.302766e-06 0.00000000
# spei180d                        8.113308e-06 0.00000000
# lower_slope                     8.097090e-06 0.00000000
# wind_anisotropy                 7.797262e-06 0.00000000
# wind_terrain_alignment          5.877176e-06 0.04761905
# elevation                       5.847678e-06 0.04761905
# lower_slope_warm                5.467980e-06 0.04761905
# veg_structure_rumple            5.228789e-06 0.04761905