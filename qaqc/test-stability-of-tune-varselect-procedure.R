### Original summary across folds to see how stable the "important variables" selection process is
# spoiler alert: not very
# mitigation strategy: Plan B is Plan A harder-- re-run 7 more iterations to get more confidence about
# estimating CPI for each feature for each hyperparamter/biome combination
source("./R/utils.R")

tune_varselect_versions_to_collate <- glue::glue("v0.1.{0:7}")

model_skill_by_fold <- purrr::map(
  .x = glue::glue("data/out/rf/tune_varselect/{tune_varselect_versions_to_collate}/model-skill_by-spatial-fold"),
  .f = \(path) {
    list.files(
      path = path,
      full.names = TRUE
    )
  }
) |> 
  unlist() |> 
  purrr::map(.f = data.table::fread) |> 
  data.table::rbindlist()

model_skill_overall <- purrr::map(
  .x = glue::glue("data/out/rf/tune_varselect/{tune_varselect_versions_to_collate}/model-skill_overall"),
  .f = \(path) {
    list.files(
      path = path,
      full.names = TRUE
    )
  }
) |>
  unlist() |> 
  purrr::map(.f = data.table::fread) |> 
  data.table::rbindlist()

cpi_results <- purrr::map(
  .x = glue::glue("data/out/rf/tune_varselect/{tune_varselect_versions_to_collate}/cpi-results"),
  .f = \(path) {
    list.files(
      path = path,
      full.names = TRUE
    )
  }
) |> 
  unlist() |> 
  purrr::map(.f = data.table::fread) |> 
  data.table::rbindlist()


model_skill_overall_cross_version_summary <- model_skill_overall |> 
  dplyr::group_by(biome_shortname, mtry, sample.fraction, min.node.size) |> 
  dplyr::summarize(
    n = dplyr::n(),
    mcc_full_mean = mean(mcc_full), 
    mcc_full_sd = sd(mcc_full), 
    mcc_full_lwr = ifelse(!is.na(mcc_full_sd), getElement(getElement(t.test(mcc_full), name = "conf.int"), 1), NA),
    mcc_reduced_mean = mean(mcc_reduced), 
    mcc_reduced_sd = sd(mcc_reduced),
    mcc_reduced_lwr = ifelse(!is.na(mcc_reduced_sd), getElement(getElement(t.test(mcc_reduced), name = "conf.int"), 1), NA),
    logloss_full_mean = mean(logloss_full), 
    logloss_full_sd = sd(logloss_full), 
    logloss_full_lwr = ifelse(!is.na(logloss_full_sd), getElement(getElement(t.test(logloss_full), name = "conf.int"), 1), NA),
    logloss_reduced_mean = mean(logloss_reduced), 
    logloss_reduced_sd = sd(logloss_reduced),
    logloss_reduced_lwr = ifelse(!is.na(logloss_reduced_sd), getElement(getElement(t.test(logloss_reduced), name = "conf.int"), 1), NA)
  )

best_hyperparameter_set <- model_skill_overall_cross_version_summary |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::filter(mcc_reduced_lwr == max(mcc_reduced_lwr, na.rm = TRUE)) |> 
  dplyr::select(biome_shortname, mtry, sample.fraction, min.node.size, mcc_reduced_mean, mcc_reduced_lwr)

model_skill_overall |> 
  dplyr::right_join(best_hyperparameter_set) |> 
  dplyr::filter(biome_shortname == "tcf") |> 
  dplyr::pull(important_variable_rf_formula) |> 
  gsub(pattern = "ewe ~ ", replacement = "") |>
  purrr::map(.f = \(x) strsplit(x = x, split = " + ", fixed = TRUE)) |> 
  unlist() |> 
  table() |> 
  sort(decreasing = TRUE)
