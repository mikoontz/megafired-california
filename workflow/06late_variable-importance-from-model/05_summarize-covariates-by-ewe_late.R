library(dplyr)
library(data.table)
library(ggplot2)

latest_ard_date <- sort(list.files(path = here::here("data", "ard", "late")), 
                        decreasing = TRUE)[1]

latest_ard_dir <- here::here("data", "ard", "late", latest_ard_date)

latest_rf_cpi_date <- sort(list.files(path = here::here("data", "out", "rf", "conditional-predictive-impact", "late")), 
                           decreasing = TRUE)[1]

rf_cpi_dir <- here::here("data", "out", "rf", "conditional-predictive-impact", latest_ard_date)

latest_rf_ttest_dir <- here::here("figs", "rf", "ewe-vs-nonewe-driver-comparison", "late", latest_ard_date)
dir.create(latest_rf_ttest_dir, showWarnings = FALSE, recursive = TRUE)

driver_descriptions <- 
  read.csv("data/out/drivers/driver-descriptions.csv") %>% 
  dplyr::as_tibble()

# What are the important variables (as determined by CPI)?
key_vars <- 
  data.table::fread(input = here::here(rf_cpi_dir, "cpi-important-variables_late.csv")) %>%
  dplyr::rename(variable = Variable) %>% 
  dplyr::filter(key_var == 1) %>% 
  dplyr::left_join(driver_descriptions) %>% 
  dplyr::group_by(biome) %>% 
  dplyr::group_split()

key_vars <-
  lapply(key_vars, FUN = function(x) {
    out <- 
      x %>%
      dplyr::mutate(variable = forcats::fct_inorder(variable),
                    display_name = forcats::fct_inorder(display_name))
  }) %>% 
  setNames(sapply(key_vars, FUN = function(x) return(unique(x$biome))))

# analysis-ready data
ard <- 
  lapply(X = list.files(here::here(latest_ard_dir), 
                        full.names = TRUE),
         FUN = data.table::fread) |> 
  data.table::rbindlist(fill = TRUE)

# Which events were EWEs? 
ewes <-
  ard[, .SD, .SDcols = c("did", "ewe", "biome_shortname")]

# Summaries of static, fluc, and landfire disturbance data
static_fluc_summary <- 
  data.table::fread("data/out/drivers/fluc-static-driver-proportion-percentiles_long.csv") %>% 
  dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)

lf_summary <- 
  data.table::fread("data/out/drivers/landfire-disturbance-driver-proportion-percentiles_long.csv") %>% 
  dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)

# Summary of non-normalized data (e.g., weather, NPL, sqrt_aoi_tm01)
weather <-
  ard %>% 
  dplyr::select(tidyselect::all_of(c("did", "ewe", "biome_shortname", driver_descriptions$variable[driver_descriptions$type %in% c("weather", "interacting")]))) %>% 
  data.table::melt(id.vars = c("did", "ewe", "biome_shortname"), value.name = "measured_value") %>% 
  dplyr::mutate(expected_value = 0.5,
                expected_value_median = 0,
                adj = measured_value,
                diff = measured_value - expected_value,
                diff_median = measured_value - expected_value_median) %>% 
  dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)

nonnormalized <-
  ard %>% 
  dplyr::select(tidyselect::all_of(c("did", "ewe", "biome_shortname", driver_descriptions$variable[driver_descriptions$type %in% c("fire")], "npl", "short_concurrent_fires"))) %>% 
  data.table::melt(id.vars = c("did", "ewe", "biome_shortname"), value.name = "measured_value") %>% 
  dplyr::mutate(expected_value = 0,
                expected_value_median = 0,
                adj = measured_value,
                diff = measured_value - expected_value,
                diff_median = measured_value - expected_value_median) %>% 
  dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj)


nonnormalized[variable == "sqrt_aoi_tm1", `:=`(measured_value = sqrt((measured_value^2*1e4)/pi)/1e3*pi,
                                               diff = sqrt((diff^2*1e4)/pi)/1e3*pi,
                                               diff_median = sqrt((diff_median^2*1e4)/pi)/1e3*pi)] 

# ard_summary_by_did <-
#   ard %>% 
#   dplyr::select(tidyselect::all_of(c("did", "ewe", "biome_shortname", driver_descriptions$variable))) %>% 
#   data.table::melt(id.vars = c("did", "ewe", "biome_shortname"), value.name = "measured_value") %>% 
#   dplyr::mutate(expected_value = 0.5,
#                 expected_value_median = 0,
#                 adj = measured_value,
#                 diff = measured_value - expected_value,
#                 diff_median = measured_value - expected_value_median) %>% 
#   dplyr::select(did, variable, expected_value, measured_value, diff, expected_value_median, diff_median, adj) %>% 
#   dplyr::left_join(y = ewes) %>% 
#   dplyr::left_join(y = driver_descriptions)
# 
# ard_summary <-
#   ard_summary_by_did %>% 
#   group_by(display_name, variable, ewe) %>% 
#   summarize(measured_value = mean(adj, na.rm = TRUE))
# 
# ard_tcf <-
#   ard %>% 
#   dplyr::filter(did %in% unique(tcf$did)) %>% 
#   dplyr::select(tidyselect::all_of(c("did", "ewe", "biome_shortname", driver_descriptions$variable))) %>% 
#   data.table::melt(id.vars = c("did", "ewe", "biome_shortname"), value.name = "measured_value") %>% 
#   dplyr::filter(variable %in% key_vars[["tcf"]]$variable) %>% 
#   dplyr::mutate(variable = factor(variable, levels = levels(key_vars[["tcf"]]$variable))) %>% 
#   dplyr::arrange(variable, ewe)
# 
# ggplot(ard_tcf, aes(x = measured_value, y = ewe)) +
#   geom_point() +
#   geom_smooth(method = "glm", method.args = list(family = binomial())) +
#   facet_wrap(facets = "variable", scales = "free_x") +
#   theme_bw()
# 
# ggplot(ard_tcf[ard_tcf$variable == "insect_disease_tm01_tm10", ], aes(x = measured_value, y = ewe)) +
#   geom_point() +
#   geom_smooth(method = "glm", method.args = list(family = binomial())) +
#   facet_wrap(facets = "variable", scales = "free_x") +
#   theme_bw()
# 
# ggplot(ard_tcf[ard_tcf$variable == "insect_disease_tm01_tm10", ], aes(x = measured_value, y = ewe)) +
#   geom_point() +
#   geom_smooth() +
#   facet_wrap(facets = "variable", scales = "free_x") +
#   theme_bw()
# 
# ggplot(ard_tcf[ard_tcf$variable == "insect_disease_tm01_tm10", ], aes(x = measured_value, y = ewe)) +
#   geom_point() +
#   geom_smooth(method = "glm", method.args = list(family = binomial())) +
#   # geom_smooth(method = "gam", formula = as.formula('y ~ s(x, bs = "cs", k = 4)')) +
#   facet_wrap(facets = "variable", scales = "free_x") +
#   theme_bw() +
#   labs(x = "Actual measured proportion")
# 
# ggplot(test, aes(x = diff_median, y = ewe)) +
#   geom_point() +
#   geom_smooth(method = "glm", method.args = list(family = binomial())) +
#   # geom_smooth(method = "gam", formula = as.formula('y ~ s(x, bs = "cs", k = 4)')) +
#   facet_wrap(facets = "variable", scales = "free_x") +
#   theme_bw()
# 
# ggplot(test, aes(x = diff_median, y = as.factor(ewe))) +
#   geom_violin() +
#   # geom_smooth(method = "gam", formula = as.formula('y ~ s(x, bs = "cs", k = 4)')) +
#   facet_wrap(facets = "variable", scales = "free_x") +
#   theme_bw()
# 
# test <- 
#   tcf %>% 
#   filter(variable == "insect_disease_tm01_tm10" | variable == "sqrt_aoi_tm1") %>% 
#   mutate(ewe = as.numeric(ewe) - 1) %>% 
#   dplyr::select(-source, -calculation) %>% 
#   tidyr::pivot_wider(id_cols = c("did", "ewe", "biome_shortname"), names_from = "variable", values_from = "diff")
# 
# ggplot(test, aes(x = insect_disease_tm01_tm10, y = as.factor(ewe), color = sqrt_aoi_tm1)) +
#   geom_point() +
#   viridis::scale_color_viridis()

# all driver summaries together
driver_summaries <-
  rbind(static_fluc_summary, lf_summary, weather, nonnormalized) %>% 
  dplyr::filter(did %in% ewes$did) %>% 
  dplyr::left_join(y = ewes) %>% 
  dplyr::left_join(y = driver_descriptions)

tcf <- 
  driver_summaries %>% 
  dplyr::filter(biome_shortname == "tcf") %>% 
  dplyr::filter(variable %in% levels(key_vars[["tcf"]]$variable)) %>% 
  dplyr::mutate(variable = factor(variable, levels = levels(key_vars[["tcf"]]$variable)),
                display_name = factor(display_name, levels = levels(key_vars[["tcf"]]$display_name)),
                ewe = ifelse(ewe == 1, yes = "EWE", no = "non-EWE"),
                ewe = factor(ewe, levels = c("non-EWE", "EWE")))

tcf_expected_vals <-
  tcf %>% 
  group_by(display_name, ewe) %>% 
  summarize(expected_value = mean(expected_value),
            measured_value = mean(measured_value),
            diff = mean(diff),
            expected_value_median = mean(expected_value_median),
            diff_median = mean(diff_median)) %>% 
  dplyr::arrange(display_name, ewe) %>% 
  dplyr::mutate(x = ifelse(ewe == "non-EWE", yes = 0.8, no = 1.8),
                xend = ifelse(ewe == "non-EWE", yes = 1.2, no = 2.2))

tcf_plot_data <-
  tcf %>% 
  dplyr::group_by(biome_shortname, ewe, variable, display_name) %>% 
  dplyr::summarize(mean = mean(measured_value),
                   lwr = mean(measured_value, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(measured_value)) - 1) * sd(measured_value, na.rm = TRUE) / sqrt(sum(!is.na(measured_value))),
                   upr = mean(measured_value, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(measured_value)) - 1) * sd(measured_value, na.rm = TRUE) / sqrt(sum(!is.na(measured_value))),
                   mean_diff = mean(diff),
                   lwr_diff = mean(diff, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(diff)) - 1) * sd(diff, na.rm = TRUE) / sqrt(sum(!is.na(diff))),
                   upr_diff = mean(diff, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(diff)) - 1) * sd(diff, na.rm = TRUE) / sqrt(sum(!is.na(diff))))

tcf_real_vals_gg <-
  ggplot(tcf_plot_data, aes(x = ewe, y = mean, ymin = lwr, ymax = upr, color = factor(ewe))) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_wrap(facets = "display_name", scales = "free_y") +
  scale_color_manual(values = c("black", "red")) +
  geom_segment(data = tcf_expected_vals, mapping = aes(x = x, 
                                                       xend = xend, 
                                                       y = expected_value, 
                                                       yend = expected_value, 
                                                       color = as.factor(ewe)),
                inherit.aes = FALSE) +
  theme_bw() +
  ggtitle(label = "Important drivers of EWEs in California's Temperate Conifer Forests") +
  labs(x = NULL,
       y = "Measured value",
       color = "EWE or not?")

ggsave(plot = tcf_real_vals_gg, filename = here::here(latest_rf_ttest_dir, "ewe-vs-nonewe_real-vals_tcf.png"))

tcf_diff_vals_gg <-
  ggplot(tcf_plot_data, aes(x = ewe, y = mean_diff, ymin = lwr_diff, ymax = upr_diff, color = factor(ewe))) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_wrap(facets = "display_name", scales = "free_y") +
  scale_color_manual(values = c("black", "red")) +
  theme_bw() +
  ggtitle(label = "Important drivers of EWEs in California's Temperate Conifer Forests") +
  labs(x = NULL,
       y = "Measured value",
       color = "EWE or not?")

ggsave(plot = tcf_diff_vals_gg, filename = here::here(latest_rf_ttest_dir, "ewe-vs-nonewe_differenced-vals_tcf.png"))

## MFWS
mfws <- 
  driver_summaries %>% 
  dplyr::filter(biome_shortname == "mfws") %>% 
  dplyr::filter(variable %in% levels(key_vars[["mfws"]]$variable)) %>% 
  dplyr::mutate(variable = factor(variable, levels = levels(key_vars[["mfws"]]$variable)),
                display_name = factor(display_name, levels = levels(key_vars[["mfws"]]$display_name)),
                ewe = ifelse(ewe == 1, yes = "EWE", no = "non-EWE"),
                ewe = factor(ewe, levels = c("non-EWE", "EWE")))

mfws_expected_vals <-
  mfws %>% 
  group_by(display_name, ewe) %>% 
  summarize(expected_value = mean(expected_value),
            measured_value = mean(measured_value),
            diff = mean(diff)) %>% 
  dplyr::arrange(display_name, ewe) %>% 
  dplyr::mutate(x = ifelse(ewe == "non-EWE", yes = 0.8, no = 1.8),
                xend = ifelse(ewe == "non-EWE", yes = 1.2, no = 2.2))

mfws_plot_data <-
  mfws %>% 
  dplyr::group_by(biome_shortname, ewe, variable, display_name) %>% 
  dplyr::summarize(mean = mean(measured_value),
                   lwr = mean(measured_value, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(measured_value)) - 1) * sd(measured_value, na.rm = TRUE) / sqrt(sum(!is.na(measured_value))),
                   upr = mean(measured_value, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(measured_value)) - 1) * sd(measured_value, na.rm = TRUE) / sqrt(sum(!is.na(measured_value))),
                   mean_diff = mean(diff),
                   lwr_diff = mean(diff, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(diff)) - 1) * sd(diff, na.rm = TRUE) / sqrt(sum(!is.na(diff))),
                   upr_diff = mean(diff, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(diff)) - 1) * sd(diff, na.rm = TRUE) / sqrt(sum(!is.na(diff))))

mfws_real_vals_gg <- 
  ggplot(mfws_plot_data, aes(x = ewe, y = mean, ymin = lwr, ymax = upr, color = factor(ewe))) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_wrap(facets = "display_name", scales = "free_y") +
  scale_color_manual(values = c("black", "red")) +
  geom_segment(data = mfws_expected_vals, mapping = aes(x = x, 
                                                       xend = xend, 
                                                       y = expected_value, 
                                                       yend = expected_value, 
                                                       color = as.factor(ewe)),
               inherit.aes = FALSE) +
  theme_bw() +
  ggtitle(label = "Important drivers of EWEs in California's Mediterranean Forests, Woodlands & Scrub") +
  labs(x = NULL,
       y = "Measured value",
       color = "EWE or not?")

ggsave(plot = mfws_real_vals_gg, filename = here::here(latest_rf_ttest_dir, "ewe-vs-nonewe_real-vals_mfws.png"))

mfws_diff_vals_gg <-
  ggplot(mfws_plot_data, aes(x = ewe, y = mean_diff, ymin = lwr_diff, ymax = upr_diff, color = factor(ewe))) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_wrap(facets = "display_name", scales = "free_y") +
  scale_color_manual(values = c("black", "red")) +
  theme_bw() +
  ggtitle(label = "Important drivers of EWEs in California's Mediterranean Forests, Woodlands & Scrub") +
  labs(x = NULL,
       y = "Measured value",
       color = "EWE or not?")

ggsave(plot = mfws_diff_vals_gg, filename = here::here(latest_rf_ttest_dir, "ewe-vs-nonewe_differenced-vals_mfws.png"))


# This figure plots all the data instead of just a 95% confidence interval-- a little harder
# to see clearly IMo
# ggplot(tcf, aes(x = ewe, y = measured_value, color = factor(ewe))) +
#   geom_point() +
#   facet_wrap(facets = "display_name", scales = "free_y") +
#   scale_color_manual(values = c("black", "red")) +
#   geom_segment(data = tcf_expected_vals, mapping = aes(x = x, 
#                                                        xend = xend, 
#                                                        y = expected_value, 
#                                                        yend = expected_value, 
#                                                        color = as.factor(ewe))) +
#   geom_jitter(width = 0.05, height = 0) +
#   theme_bw() +
#   ggtitle(label = "Important drivers of EWEs in California's Temperate Conifer Forests") +
#   labs(x = NULL,
#        color = "EWE or not?")
