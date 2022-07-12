library(randomForestExplainer)
# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html#variable-interactions
min_depth_frame <- randomForestExplainer::min_depth_distribution(biome_nonspatial)
randomForestExplainer::plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

importance_frame <- randomForestExplainer::measure_importance(biome_nonspatial)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))

interactions_frame <- min_depth_interactions(biome_nonspatial, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")

plot_min_depth_interactions(interactions_frame)

biome_nonspatial$call <- str2lang(paste0("ranger::ranger(ewe ~ ", paste(predictor.variable.names_reduced$selected.variables, collapse = " + "), ")"))

plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "fm100_pct")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "sqrt_aoi_tm1")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "friction_walking_only", variable2 = "road_density_mpha")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "landform_diversity")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "min_wind_speed_pct")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "fm100_pct", variable2 = "road_density_mpha")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "pdsi_z")
plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "spei2y")

plot_predict_interaction(forest = biome_nonspatial, data = data, variable1 = "road_density_mpha", variable2 = "road_density_mpha")

spatialRF::auc(o = data$ewe, p = ifelse(biome_spatial$predictions$values >= 0.5, 1, 0))
rf_response_curves_gg <-
  spatialRF::plot_response_curves(
    biome_spatial,
    quantiles = c(0.5),
    ncol = 5
  )

rf_response_curves_gg