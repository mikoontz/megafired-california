# We had been implementing partial dependence plots
# https://christophm.github.io/interpretable-ml-book/pdp.html

# With correlated data, it may be better to implement accumulated local effects plots
# https://christophm.github.io/interpretable-ml-book/ale.html#ale
# https://search.r-project.org/CRAN/refmans/ALEPlot/html/ALEPlot.html
# Apley and Zhu, 2020. https://doi.org/10.1111/rssb.12377

library(dplyr)
library(ALEPlot)
library(readr)
library(here)
library(ranger)
library(ggplot2)

latest_ard_date <- sort(list.files(path = here::here("data", "ard", "early")), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", "early", latest_ard_date)
rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", "early", latest_ard_date)

rf_fitted_dir <- here::here("data", "out", "rf", "fitted", "early", latest_ard_date)

rf_ale_curves_dir <- here::here("figs", "rf", "ale-curves", "early", latest_ard_date)
dir.create(rf_ale_curves_dir, showWarnings = FALSE, recursive = TRUE)

biome_shortnames <- c("tcf", "mfws")
biome_lookup <- 
  tibble::tibble(biome_name_daily = c("Temperate Conifer Forests", 
                                      "Mediterranean Forests, Woodlands & Scrub", 
                                      "Temperate Grasslands, Savannas & Shrublands", 
                                      "Deserts & Xeric Shrublands"),
                 biome_shortname = c("tcf", "mfws", "tgss", "dxs"))

driver_descriptions <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

# What are the important variables (as determined by CPI)?
key_vars <- 
  data.table::fread(input = here::here(rf_cpi_dir, "cpi-important-variables_early.csv")) %>%
  dplyr::rename(variable = Variable) %>% 
  dplyr::filter(key_var == 1) %>% 
  dplyr::left_join(driver_descriptions) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::group_split()

key_vars_l <-
  lapply(key_vars, FUN = function(x) {
    out <- 
      x %>%
      dplyr::mutate(variable = forcats::fct_inorder(variable),
                    display_name = forcats::fct_inorder(display_name))
  }) %>% 
  setNames(sapply(key_vars, FUN = function(x) return(unique(x$biome))))

fitted_models <-
  lapply(X = biome_shortnames, FUN = function(biome_shortname) {
    fname <- here::here(rf_fitted_dir, paste0("rf_ranger_fitted-model_rtma_", biome_shortname, "_early.rds"))
    return(readr::read_rds(fname))
  }) %>% 
  setNames(biome_shortnames)

yhat <- function(X.model, newdata) as.numeric(predict(X.model, data = newdata)$predictions[, 1])

ale_gg <- function(biome_shortname) {
  fm <- fitted_models[[biome_shortname]]
  key_vars_df <- key_vars_l[[biome_shortname]]
  key_vars <- key_vars_l[[biome_shortname]]$variable
  # Don't include first column, because that is the response
  data <- fm$forest$data
  X <- data[, -1]
  key_var_rank <- 
    dplyr::tibble(J = match(x = key_vars, table = names(X)),
                  rank = 1:length(key_vars))
  J <- 
    dplyr::tibble(J = 1:ncol(X), 
                  variable = names(X),
                  key_var = as.numeric(variable %in% key_vars)) %>% 
    dplyr::left_join(y = key_var_rank) %>% 
    dplyr::filter(key_var == 1) %>% 
    dplyr::arrange(rank)
  
  out_all_key_vars <- 
    lapply(seq_along(J$rank), FUN = function(i) {
      this_row <- J[J$rank == i, ]
      # variable_name <- J$variable[J$rank == i]
      l <- ALEPlot::ALEPlot(X = X,
                            X.model = fm, 
                            J = this_row$J,
                            pred.fun = yhat)
      
      out_this_key_var <-
        dplyr::as_tibble(l[2:3]) %>% 
        setNames(c("x", "f")) %>% 
        dplyr::mutate(variable = this_row$variable)
      
      return(out_this_key_var)
    }) %>% 
    data.table::rbindlist() %>% 
    dplyr::left_join(driver_descriptions) %>% 
    dplyr::mutate(variable = factor(variable, levels = key_vars_df$variable),
                  display_name = factor(display_name, levels = key_vars_df$display_name))
  
  return(list(out_all_key_vars = out_all_key_vars, X = X))
}

pdp_gg <- function(biome_shortname) {
  fm <- fitted_models[[biome_shortname]]
  key_vars_df <- key_vars_l[[biome_shortname]]
  key_vars <- key_vars_l[[biome_shortname]]$variable
  # Don't include first column, because that is the response
  data <- fm$forest$data
  X <- data[, -1]
  key_var_rank <- 
    dplyr::tibble(J = match(x = key_vars, table = names(X)),
                  rank = 1:length(key_vars))
  J <- 
    dplyr::tibble(J = 1:ncol(X), 
                  variable = names(X),
                  key_var = as.numeric(variable %in% key_vars)) %>% 
    dplyr::left_join(y = key_var_rank) %>% 
    dplyr::filter(key_var == 1) %>% 
    dplyr::arrange(rank)
  
  out_all_key_vars <- 
    lapply(seq_along(J$rank), FUN = function(i) {
      print(i)
      this_row <- J[J$rank == i, ]
      # variable_name <- J$variable[J$rank == i]
      l <- ALEPlot::PDPlot(X = X,
                           X.model = fm, 
                           J = this_row$J,
                           pred.fun = yhat,
                           K = 50)
      
      out_this_key_var <-
        dplyr::as_tibble(l[1:2]) %>% 
        setNames(c("x", "f")) %>% 
        dplyr::mutate(variable = this_row$variable)
      
      return(out_this_key_var)
    }) %>% 
    data.table::rbindlist() %>% 
    dplyr::left_join(driver_descriptions) %>% 
    dplyr::mutate(variable = factor(variable, levels = key_vars_df$variable),
                  display_name = factor(display_name, levels = key_vars_df$display_name))
  
  return(list(out_all_key_vars = out_all_key_vars, X = X))
}

tcf <- ale_gg(biome_shortname = "tcf")
tcf_ale <- tcf$out_all_key_vars
tcf_x <- 
  tcf$X %>% 
  tidyr::pivot_longer(cols = tidyselect::everything(), 
                      names_to = "variable", 
                      values_to = "x") %>% 
  dplyr::left_join(y = driver_descriptions, by = "variable") %>% 
  dplyr::filter(variable %in% tcf_ale$variable) %>% 
  dplyr::mutate(display_name = factor(display_name, levels = levels(tcf_ale$display_name)))

tcf_ale_gg <- 
  ggplot(data = tcf_ale, mapping = aes(x = x, y = f)) +
  geom_line() +
  # geom_rug() +
  geom_rug(data = tcf_x, mapping = aes(x = x), inherit.aes = FALSE) +
  facet_wrap(facets = "display_name", scales = "free") +
  theme_bw() +
  labs(x = "Variable value (local area)",
       y = "Accumulated local effect") +
  ggtitle("Temperate Conifer Forests")

ggsave(plot = tcf_ale_gg, 
       filename = file.path(rf_ale_curves_dir, "tcf_ale-curves_early.png"),
       width = 12, height = 12)

# tcf_pdp <- pdp_gg(biome_shortname = "tcf")
# ggplot(tcf_pdp, aes(x = x, y = f)) +
#   geom_line() +
#   geom_rug() +
#   facet_wrap(facets = "display_name", scales = "free") +
#   theme_bw()

mfws <- ale_gg(biome_shortname = "mfws")
mfws_ale <- mfws$out_all_key_vars
mfws_x <- 
  mfws$X %>% 
  tidyr::pivot_longer(cols = tidyselect::everything(), 
                      names_to = "variable", 
                      values_to = "x") %>% 
  dplyr::left_join(y = driver_descriptions, by = "variable") %>% 
  dplyr::filter(variable %in% mfws_ale$variable) %>% 
  dplyr::mutate(display_name = factor(display_name, levels = levels(mfws_ale$display_name)))

mfws_ale_gg <- 
  ggplot(mfws_ale, aes(x = x, y = f)) +
  geom_line() +
  # geom_rug() +
  geom_rug(data = mfws_x, mapping = aes(x = x), inherit.aes = FALSE) +
  facet_wrap(facets = "display_name", scales = "free") +
  theme_bw() +
  labs(x = "Variable value (local area)",
       y = "Accumulated local effect") +
  ggtitle("Mediterranean Forests, Woodlands & Scrub")

ggsave(plot = mfws_ale_gg, 
       filename = file.path(rf_ale_curves_dir, "mfws_ale-curves_early.png"),
       width = 12, height = 12)


# mfws_pdp <- pdp_gg(biome_shortname = "mfws")
# ggplot(mfws_pdp, aes(x = x, y = f)) +
#   geom_line() +
#   geom_rug() +
#   facet_wrap(facets = "display_name", scales = "free") +
#   theme_bw()





# fm_mfws <- fitted_models[["mfws"]]
# key_vars_mfws <- key_vars[["mfws"]]$variable
# # Don't include first column, because that is the response
# X_mfws <- fm_mfws$forest$data[, -1]
# J_mfws <- match(x = key_vars_mfws, table = names(X_mfws))
# # dplyr::as_tibble(X_mfws[, J_mfws])
# dplyr::tibble(J = J_mfws, variable = names(X_mfws)[J_mfws])
# 
# sqrt_aoi_tm1 <- ALEPlot::ALEPlot(X = X_tcf, 
#                                  X.model = fm_tcf, 
#                                  J = J_tcf$J[which(J_tcf$rank == 1)],
#                                  pred.fun = yhat)
# 
# insect_disease_tm01_tm10_l <- ALEPlot::ALEPlot(X = X_tcf, 
#                                                X.model = fm_tcf, 
#                                                J = J_tcf$J[which(J_tcf$variable == "insect_disease_tm01_tm10")],
#                                                pred.fun = yhat, K = 5)
# 
# insect_disease_tm01_tm10_l <- ALEPlot::ALEPlot(X = X_tcf, 
#                                                X.model = fm_tcf, 
#                                                J = J_tcf$J[which(J_tcf$variable %in% c("insect_disease_tm01_tm10", "sqrt_aoi_tm1"))],
#                                                pred.fun = yhat, K = 75)
# 
# insect_disease_tm01_tm10 <- 
#   dplyr::as_tibble(insect_disease_tm01_tm10_l[2:3]) %>% 
#   dplyr::mutate(variable = "insect_disease_tm01_tm10")
# 
# ggplot(insect_disease_tm01_tm10, aes(x = x.values, y = f.values)) +
#   geom_line() +
#   geom_rug(inherit.aes = FALSE, data = data_tcf[, c("insect_disease_tm01_tm10", "ewe")], 
#            mapping = aes(x = insect_disease_tm01_tm10)) +
#   theme_bw() +
#   facet_wrap(facets = "variable") +
#   labs(x = "Percentile rank",
#        y = "Accumulated local effect on EWE probability")
# 
# caltrans_road_density_mpha_l <- ALEPlot::ALEPlot(X = X_tcf, 
#                                                  X.model = fm_tcf, 
#                                                  J = J_tcf$J[which(J_tcf$variable == "caltrans_road_density_mpha")],
#                                                  pred.fun = yhat)
# 
# caltrans_road_density_mpha <- 
#   dplyr::as_tibble(caltrans_road_density_mpha_l[2:3]) %>% 
#   dplyr::mutate(variable = "caltrans_road_density_mpha")
# 
# ggplot(caltrans_road_density_mpha, aes(x = x.values, y = f.values)) +
#   geom_line() +
#   geom_rug(inherit.aes = FALSE, data = data_tcf[, c("caltrans_road_density_mpha", "ewe")], 
#            mapping = aes(x = caltrans_road_density_mpha)) +
#   theme_bw() +
#   facet_wrap(facets = "variable") +
#   labs(x = "Percentile rank",
#        y = "Accumulated local effect on EWE probability")
