# Tune the classification threshold by calculating 
library(dplyr)
library(tidyr)
library(pbapply)
library(data.table)
# library(PRROC)
library(ggplot2)

latest_ard_date <- sort(list.files(path = here::here("data", "ard", "late")), 
                        decreasing = TRUE)[1]

# latest_rf_tuning_date <- sort(list.files(path = here::here("data", "out", "rf", "tuning")), 
#                               decreasing = TRUE)[1]

latest_rf_tuning_dir <- here::here("data", "out", "rf", "tuning", "late", latest_ard_date)
latest_rf_figs_dir <- here::here("figs", "rf", "tuning", "late", latest_ard_date)

# We can tune the "classification threshold" for saying something is an EWE or not
# No need to fit more models for this step, we merely adjust our classification threshold, convert the
# pseudo-probability predictions from the models already fit to crisp 0 or 1 values, then calculate the 
# model skill metrics.
# biome_shortnames <- c("tcf", "mfws", "tgss", "dxs")
biome_shortnames <- c("tcf", "mfws", "dxs")

(start_time <- Sys.time())
# Take the spatial CV results (observation for held out fold and predictions) and calculate various model skill metrics
pbapply::pblapply(X = biome_shortnames, FUN = function(biome_shortname) {
  biome_tune <- data.table::fread(here::here(latest_rf_tuning_dir, paste0("rf_ranger_spatial-cv-tuning_rtma_", biome_shortname, "_late.csv")))
  biome_tune[, o_fac := factor(o, levels = c(1, 0))]
  
  # We can tune the classification threshold for calling a prediction on {0, 1} a 0 versus a 1
  # Iterate through the grid of classification thresholds, assign the 0/1 prediction based on 
  # whether the {0, 1} model prediction is below or above that threshold, calculate a bunch of
  # model skill metrics, take the mean of each of those model skill metrics for each combo of
  # hyperparameters (including classification threshold) for each spatial fold (i.e., across 
  # the 10 iterations)
  classification_thresh_vec <- seq(from = 0.01, to = 0.40, by = 0.01)
  
  biome_tune[, classification_thresh := list(classification_thresh_vec)]
  
  # For TCF, this turns into a 355 million row data table, which appears to be handled just fine. {data.table is amazing.}
  biome_tune <- 
    tidyr::unnest(biome_tune, cols = "classification_thresh") %>% 
    as.data.table()
  
  biome_tune[, p_fac := factor(ifelse(p >= classification_thresh, yes = 1, no = 0), levels = c(1, 0))]
  
  # For ROC-AUC: 
  # Youden WJ (1950). “Index for rating diagnostic tests.” Cancer, 3(1), 32–35. 
  # doi:10.1002/1097-0142(1950)3:1<32::aid-cncr2820030106>3.0.co;2-3.
  
  # For PR-AUC: 
  # Davis J, Goadrich M (2006). “The relationship between precision-recall and ROC curves.” 
  # In Proceedings of the 23rd International Conference on Machine Learning. ISBN 9781595933836.
  #
  # J. Grau, I. Grosse, and J. Keilwagen. PRROC: computing and visualizing precision-recall and 
  # receiver operating characteristic curves in R. Bioinformatics, 31(15):2595-2597, 2015.
  
  # For MCC: 
  # Chicco, D., & Jurman, G. (2020). The advantages of the Matthews correlation coefficient (MCC) 
  # over F1 score and accuracy in binary classification evaluation. BMC Genomics, 21(1), 6. 
  # https://doi. org/10.1186/s12864-019-6413-7
  #
  # Chicco, D., Tötsch, N., & Jurman, G. (2021). The Matthews correlation coefficient (MCC) is more 
  # reliable than balanced accuracy, bookmaker informedness, and markedness in two-class confusion 
  # matrix evaluation. BioData Mining, 14, 13. https://doi.org/10.1186/s13040-021-00244-z
  
  metrics <- c("tp", "fp", "fn", "tn", "accuracy", "rmse", "logloss", "roc_auc", "pr_auc", "precision", "recall", "specificity", "f_meas", "informedness", "mcc")
  
  # First, figure out the confusion matrix and other metrics that can be calculated from observed/predicted directly
  results <-
    biome_tune[, .(tp = as.numeric(length(which(o == 1 & p_fac == "1"))),
                   fp = as.numeric(length(which(o == 0 & p_fac == "1"))),
                   fn = as.numeric(length(which(o == 1 & p_fac == "0"))),
                   tn = as.numeric(length(which(o == 0 & p_fac == "0"))),
                   accuracy = mean(o_fac == p_fac),
                   rmse = sqrt(mean((o - p)^2)),
                   # logloss code from {MLmetrics} package
                   logloss = -mean(o * log(pmax(pmin(p, 1 - 1e-15), 1e-15)) + (1 - o) * log(1 - pmax(pmin(p, 1 - 1e-15), 1e-15))),
                   # roc_auc code from {mlr3measures} package
                   roc_auc = (mean(rank(p, ties.method = "average")[which(o == 1)]) - (as.numeric(length(which(o == 1))) + 1)/2) / as.numeric(length(which(o == 0))),
                   pr_auc = PRROC::pr.curve(scores.class0 = p, weights.class0 = o)[[2]]),
               by = .(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, iter)]
  
  # Then, we can derive model skill metrics based on the confusion matrix
  results[, `:=`(precision = tp / (tp + fp),
                 recall = tp / (tp + fn),
                 specificity = tn / (tn + fp))]
  
  results[, `:=`(f_meas = 2 * (precision * recall / (precision + recall)),
                 informedness = (tp / (tp + fn)) + (tn / (tn + fp)) - 1,
                 mcc = ((tp * tn) - (fn * fp)) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)))]
  
  # Here, we calculate the mean of all of the model skill metrics across the 10 iterations (i.e., for each
  # combo of hyperparameters, for each spatial fold)
  # https://stackoverflow.com/questions/35618545/applying-a-function-to-all-columns-of-a-data-table-together-with-a-group-by
  # lwr and upr confidence intervals; https://stackoverflow.com/questions/48612153/how-to-calculate-confidence-intervals-for-a-vector
  
  # Melt data to get to long form for next pieces
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
  results_long <-
    data.table::melt(results, 
                     id.vars = c("id", "assessment_ewe_n", "assessment_ewe_0", "assessment_ewe_1", "mtry", "num.trees", "sample.fraction", "classification_thresh", "min.node.size", "class.wgts", "iter"),
                     measure.vars = metrics,
                     variable.name = ".metric",
                     value.name = ".estimate")
  
  # Apply summary functions to the .estimate column across iterations
  results_across_iter <-
    results_long[, .(mean = mean(.estimate, na.rm = TRUE),
                     sd = sd(.estimate, na.rm = TRUE),
                     n = sum(!is.na(.estimate)),
                     min = min(.estimate, na.rm = TRUE),
                     max = max(.estimate, na.rm = TRUE),
                     lwr = mean(.estimate, na.rm = TRUE) - qt(p = 0.975, df = sum(!is.na(.estimate)) - 1) * sd(.estimate, na.rm = TRUE) / sqrt(sum(!is.na(.estimate))),
                     upr = mean(.estimate, na.rm = TRUE) + qt(p = 0.975, df = sum(!is.na(.estimate)) - 1) * sd(.estimate, na.rm = TRUE) / sqrt(sum(!is.na(.estimate))),
                     biome = biome_shortname),
                 by = .(id, assessment_ewe_n, assessment_ewe_0, assessment_ewe_1, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric)]
  
  
  data.table::fwrite(x = results_across_iter, file = here::here(latest_rf_tuning_dir, paste0("rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, "_late.csv")))
  
})

# Look at the tuning results and see how we can fine tune the hyperparameters
tuning_metrics_l <- lapply(biome_shortnames, FUN = function(biome_shortname) {
  
  tuning_metrics <- 
    data.table::fread(input = here::here(latest_rf_tuning_dir, paste0("rf_ranger_spatial-cv-tuning-metrics_rtma_", biome_shortname, "_late.csv")))
  
  tune_gg_data <- 
    tuning_metrics %>%
    filter(.metric %in% c("informedness", "mcc")) %>% 
    group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>% 
    summarize(n = n(),
              n_not_missing = sum(!is.na(mean)),
              mean = mean(x = mean, na.rm = TRUE),
              lwr = mean(x = lwr, na.rm = TRUE)) %>% 
    dplyr::select(mtry, num.trees, sample.fraction, classification_thresh, min.node.size, .metric, mean, lwr, n, n_not_missing) %>% 
    dplyr::filter((n_not_missing / n) >= 0.5) %>% 
    tidyr::pivot_wider(id_cols = c(mtry, num.trees, sample.fraction, classification_thresh, min.node.size), names_from = ".metric", values_from = "mean") %>% 
    arrange(desc(informedness))
  
  informedness_mcc_gg <-
    ggplot(tune_gg_data, aes(x = informedness, y = mcc)) + 
    geom_point() +
    theme_bw()
  
  ggsave(filename = here::here(latest_rf_figs_dir, "informedness-vs-mcc_rtma_", biome_shortname, "_late.png"), plot = informedness_mcc_gg)
  
  class_thresh_effect_data <-
    tuning_metrics %>% 
    dplyr::filter(!(.metric %in% c("tp", "fp", "fn", "tn", "rmse"))) %>% 
    group_by(biome, mtry, num.trees, sample.fraction, classification_thresh, min.node.size, class.wgts, .metric) %>% 
    summarize(n = n(),
              n_not_missing = sum(!is.na(mean)),
              mean = mean(x = mean, na.rm = TRUE),
              lwr = mean(x = lwr, na.rm = TRUE)) %>% 
    dplyr::select(mtry, num.trees, sample.fraction, classification_thresh, min.node.size, .metric, mean, lwr, n, n_not_missing) %>% 
    dplyr::filter((n_not_missing / n) >= 0.5)
  
  class_thresh_effect_gg <-
    ggplot(data = class_thresh_effect_data, mapping = aes(x = classification_thresh, y = mean)) +
    geom_point() +
    geom_smooth() + 
    facet_wrap(facets = ".metric") +
    theme_bw()
  
  ggsave(filename = here::here(latest_rf_figs_dir, paste0("classification-threshold-effect_rtma_", biome_shortname, "_late.png")), plot = class_thresh_effect_gg)
  
})
(end_time <- Sys.time())
(difftime(end_time, start_time, units = "hours"))

# Took about 70 minutes total on a 12-core machine with 64GB of RAM
