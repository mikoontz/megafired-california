fold_avg_fname = here::here("data/out/rf/tuning/early/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_rtma_tcf_early.csv")
across_fold_fname = here::here("data/out/rf/tuning/early/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_across-folds_tcf_early.csv")

compare_mean_vs_median_across_folds = function(fold_avg_fname, across_fold_fname, biome, early_or_late) {
  
  fold_avg = data.table::fread(fold_avg_fname) |> 
    dplyr::filter(.metric == "mcc") |> 
    dplyr::group_by(mtry, sample.fraction, classification_thresh, min.node.size) |> 
    dplyr::summarize(mcc_mean_across_folds = mean(x = mean, na.rm = TRUE),
                     mcc_median_across_folds = median(x = mean, na.rm = TRUE))
  
  integrate_across_folds = data.table::fread(across_fold_fname) |> 
    dplyr::filter(.metric == "mcc") |> 
    dplyr::select(mtry, sample.fraction, classification_thresh, min.node.size, mean) |> 
    dplyr::rename(mcc_across_all_folds = mean)
  
  both = merge(
    x = fold_avg,
    y = integrate_across_folds
  ) |> 
    dplyr::mutate(
      biome = {{biome}}, early_or_late = {{early_or_late}},
      mean_diff = mcc_mean_across_folds - mcc_across_all_folds,
      median_diff = mcc_median_across_folds - mcc_across_all_folds
    ) |> 
    tidyr::pivot_longer(cols = c("mean_diff", "median_diff"), names_to = "diff_type", values_to = "diff")
  
  return(both)
}

recipe = tibble::tibble(
  fold_avg_fname = c(
    here::here("data/out/rf/tuning/early/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_rtma_tcf_early.csv"),
    here::here("data/out/rf/tuning/early/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_rtma_mfws_early.csv"),
    here::here("data/out/rf/tuning/late/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_rtma_tcf_late.csv"),
    here::here("data/out/rf/tuning/late/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_rtma_mfws_late.csv")
  ),
  across_fold_fname = c(
    here::here("data/out/rf/tuning/early/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_across-folds_tcf_early.csv"),
    here::here("data/out/rf/tuning/early/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_across-folds_mfws_early.csv"),
    here::here("data/out/rf/tuning/late/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_across-folds_tcf_late.csv"),
    here::here("data/out/rf/tuning/late/2023-06-21/rf_ranger_spatial-cv-tuning-metrics_across-folds_mfws_late.csv")
  ),
  biome = c(
    "tcf",
    "mfws",
    "tcf",
    "mfws"
  ),
  early_or_late = c(
    "early",
    "early",
    "late",
    "late"
  )
)

out = purrr::pmap(
  .l = recipe,
  .f = compare_mean_vs_median_across_folds
) |> 
  data.table::rbindlist()

out

library(ggplot2)

ggplot(out, aes(x = mcc_across_all_folds, y = diff, color = diff_type)) +
  # geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_grid(early_or_late ~ biome) +
  geom_hline(yintercept = 0) + 
  theme_bw()

