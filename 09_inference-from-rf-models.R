library(readr)
library(dplyr)
library(spatialRF)
library(randomForestExplainer)
library(ggplot2)
library(patchwork)

dir.create("figs/rf", showWarnings = FALSE)

# system2(command = "aws", args = "s3 sync s3://california-megafires/data/out/rf  data/out/rf", stdout = TRUE)  

# biome_shortname <- "tcf"
biome_shortname <- "mfws"
# biome_shortname <- "tgss"
# biome_shortname <- "dxs"


biome_nonspatial <-
  list.files("data/out/rf", full.names = TRUE) %>% 
  data.frame(fname = .) %>% 
  dplyr::filter(grepl(pattern = "v5", x = fname), 
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

plot_response_curves(biome_nonspatial, quantiles = 0.5)

# AUC of simple model versus one that includes fuel, topography, weather, and human factors
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

# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html#variable-interactions
min_depth_frame <- randomForestExplainer::min_depth_distribution(biome_nonspatial)
randomForestExplainer::plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

importance_frame <- randomForestExplainer::measure_importance(biome_nonspatial)
randomForestExplainer::plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

(vars <- randomForestExplainer::important_variables(importance_frame, k = 10, measures = c("mean_min_depth", "no_of_trees")))

interactions_frame <- randomForestExplainer::min_depth_interactions(biome_nonspatial, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")

randomForestExplainer::plot_min_depth_interactions(interactions_frame)
biome_nonspatial$call <- str2lang(paste0("ranger::ranger(ewe ~ ", paste(predictor.variable.names_reduced$selected.variables, collapse = " + "), ")"))

interactions_frame_working <- 
  interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ] %>% 
  dplyr::slice(1:30) %>% 
  dplyr::filter(variable != as.character(root_variable)) %>% 
  dplyr::mutate(sorted_interaction = mapply(FUN = function(x, y) {
    paste(sort(c(x, y)), collapse = ":")
  }, 
  variable, as.character(root_variable))) %>% 
  dplyr::filter(!duplicated(sorted_interaction))

n_pages <- 2

l <- lapply(1:n_pages, function(j) {
  sub_l <- lapply(1:(nrow(interactions_frame_working)/n_pages), function(i) {
    randomForestExplainer::plot_predict_interaction(
      forest = biome_nonspatial, 
      data = data, 
      variable1 = as.character(interactions_frame_working[(j-1) + i, "root_variable"]), 
      variable2 = as.character(interactions_frame_working[(j-1) + i, "variable"])
    )
  })
})

p1 <- patchwork::wrap_plots(l[[1]], ncol = 3, nrow = 5)
p2 <- patchwork::wrap_plots(l[[2]], ncol = 3, nrow = 5)

ggplot2::ggsave(filename = paste0("figs/rf/", biome_shortname, "_interactions_01.png"), plot = p1, height = 15, width = 15, units = "in")
ggplot2::ggsave(filename = paste0("figs/rf/", biome_shortname, "_interactions_02.png"), plot = p2, height = 15, width = 15, units = "in")

