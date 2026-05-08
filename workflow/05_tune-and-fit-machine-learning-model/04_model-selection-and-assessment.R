library(ggplot2)

source("./R/utils.R")

tune_varselect_version <- glue::glue("v0.1.a")

model_skill_by_fold <- list.files(
  path = glue::glue(
    "data/out/rf/tune_varselect/{tune_varselect_version}/",
    "model-skill_by-spatial-fold"
  ),
  full.names = TRUE
) |> 
  purrr::map(.f = data.table::fread) |> 
  data.table::rbindlist() |>
  dplyr::group_by(tune_varselect_version, biome_shortname, mtry, sample.fraction, min.node.size, n_important_variables, important_variable_rf_formula) |>
  dplyr::summarize(mcc_full_by_fold = mean(mcc_full, na.rm = TRUE),
                   mcc_reduced_by_fold = mean(mcc_reduced, na.rm = TRUE),
                   logloss_full_by_fold = mean(logloss_full, na.rm = TRUE),
                   logloss_reduced_by_fold = mean(logloss_reduced, na.rm = TRUE)) |>
  dplyr::ungroup()

model_skill_overall <- list.files(
  path = glue::glue(
    "data/out/rf/tune_varselect/{tune_varselect_version}/",
    "model-skill_overall"
  ),
  full.names = TRUE
) |>
  purrr::map(.f = data.table::fread) |> 
  data.table::rbindlist()

cpi_results <- list.files(
  path = glue::glue(
    "data/out/rf/tune_varselect/{tune_varselect_version}/",
    "cpi-results"
  ),
  full.names = TRUE
) |> 
  purrr::map(.f = data.table::fread) |> 
  data.table::rbindlist()

model_skill_overall_compact <- model_skill_overall |> 
  dplyr::select(
    tune_varselect_version, biome_shortname, 
    mtry, sample.fraction, min.node.size, 
    n, n_important_variables, 
    important_variable_rf_formula, 
    mcc_reduced, logloss_reduced
  )

model_skill_by_fold_compact <- model_skill_by_fold |> 
  dplyr::select(
    tune_varselect_version, biome_shortname, 
    mtry, sample.fraction, min.node.size, 
    n_important_variables, 
    important_variable_rf_formula, 
    mcc_reduced_by_fold, logloss_reduced_by_fold
  )

model_skill <- dplyr::left_join(
  model_skill_overall_compact,
  model_skill_by_fold_compact
)

pareto_front = rPref::psel(
  df = model_skill[model_skill$biome_shortname == "tcf", ],
  pref = rPref::high(mcc_reduced) * rPref::high(mcc_reduced_by_fold) * rPref::low(n_important_variables)
)

top_results = r2_pareto_front |> na.omit()

best_fit = top_results[2,]

best_hyperparameter_set_overall_mcc <- model_skill_overall |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::filter(mcc_reduced == max(mcc_reduced, na.rm = TRUE)) |> 
  dplyr::select(biome_shortname, mtry, sample.fraction, min.node.size, mcc_reduced)

best_hyperparameter_set_overall_logloss <- model_skill_overall |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::filter(logloss_reduced == min(logloss_reduced, na.rm = TRUE)) |> 
  dplyr::select(biome_shortname, mtry, sample.fraction, min.node.size, logloss_reduced)

model_skill_overall |> 
  dplyr::right_join(best_hyperparameter_set_overall_mcc) |> 
  dplyr::select(biome_shortname, n_important_variables, important_variable_rf_formula)

model_skill_overall |> 
  dplyr::right_join(best_hyperparameter_set_overall_logloss) |> 
  dplyr::select(biome_shortname, n_important_variables, important_variable_rf_formula)

# best across folds
best_hyperparameter_set_by_fold_mcc <- model_skill_by_fold |> 
  dplyr::group_by(biome_shortname) |>
  dplyr::filter(mcc_reduced == max(mcc_reduced, na.rm = TRUE)) |> 
  dplyr::select(biome_shortname, mtry, sample.fraction, min.node.size, mcc_reduced)

best_hyperparameter_set_by_fold_logloss <- model_skill_by_fold |> 
  dplyr::group_by(biome_shortname) |>
  dplyr::filter(logloss_reduced == min(logloss_reduced, na.rm = TRUE)) |> 
  dplyr::select(biome_shortname, mtry, sample.fraction, min.node.size, logloss_reduced)

model_skill_by_fold |> 
  dplyr::right_join(best_hyperparameter_set_by_fold_mcc) |> 
  dplyr::select(biome_shortname, n_important_variables, important_variable_rf_formula)

model_skill_by_fold |> 
  dplyr::right_join(best_hyperparameter_set_by_fold_logloss) |> 
  dplyr::select(biome_shortname, n_important_variables, important_variable_rf_formula)

