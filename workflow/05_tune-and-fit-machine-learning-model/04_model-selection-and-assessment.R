library(ggplot2)

source("./R/utils.R")

tune_varselect_version <- glue::glue("v0.1.a")

### Gather analysis ready data
latest_ard_fname <- sort(
  list.files(
    path = here::here("data", "ard"), 
    pattern = "[0-9]",
    full.names = TRUE), 
  decreasing = TRUE
) |> 
  getElement(1)

latest_ard_date <- stringr::str_sub(latest_ard_fname, start = -14, end = -5)

local_out_dir <- here::here("data", "out", "rf", "tuning", latest_ard_date)
dir.create(local_out_dir, recursive = TRUE, showWarnings = FALSE)

ard <- readr::read_csv(
  latest_ard_fname, 
  col_types = list(spatial_fold = "factor")
)

# group by biome
ard_nested <- ard |> 
  dplyr::filter(!validation) |> 
  # Set up an ewe of 1 to be the "positive" class
  dplyr::mutate(ewe = factor(ewe, levels = c("1", "0"))) |> 
  tidyr::nest(.by = "biome_shortname", .key = "data")

### collate all model results
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
) |> 
  tidyr::drop_na() |> 
  # compute maximum correlation among variables in the model
  dplyr::left_join(ard_nested) |> 
  dplyr::mutate(
    max_corr = purrr::pmap_dbl(
      .l = list(important_variable_rf_formula, data), 
      .f = get_max_corr
    )
  )

best_models <- model_skill |> 
  dplyr::select(-n, -logloss_reduced, -logloss_reduced_by_fold) |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::filter(max_corr < 0.8) |> 
  dplyr::group_map(
    .f = ~ rPref::psel(
      df = .x, 
      pref = rPref::high(mcc_reduced) * rPref::low(n_important_variables)
    ),
    .keep = TRUE
  ) |> 
  dplyr::bind_rows() |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::filter(n_important_variables == min(n_important_variables))

best_models

fetch_cpi_results <- function(biome_shortname, tune_varselect_version,
                              mtry, sample.fraction, min.node.size, ...) {
  
  # label for decimal sample.fraction
  sample.fraction_label <- stringr::str_pad(
    round(sample.fraction, digits = 3), 
    side = 'right', 
    width = 5, 
    pad = '0'
  )
  
  # Basename that captures hyperparameter combination
  hyperparameter_basename <- glue::glue(
    "mtry_{mtry}_",
    "sample.fraction_{sample.fraction_label}",
    "_min.node.size_{min.node.size}.csv"
  )
  
  ## The full local output filenames
  cpi_results_fname <- glue::glue(
    "data/out/rf/tune_varselect/{tune_varselect_version}/",
    "cpi-results/",
    "tune_varselect_{tune_varselect_version}_",
    "{biome_shortname}_cpi-results_",
    "{hyperparameter_basename}"
  )
  
  data.table::fread(cpi_results_fname)
  
}

cpi_of_best_models <- best_models |> 
  purrr::pmap(.f = fetch_cpi_results) |> 
  dplyr::bind_rows() |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::group_map(
    .f = ~ .x |> dplyr::arrange(dplyr::desc(ci.lo)) |> dplyr::filter(ci.lo > 0),
    .keep = TRUE
  )

best_models
cpi_of_best_models

fit_best_model <- function(biome_shortname, mtry, sample.fraction, min.node.size, rf_formula, data, ...) {
  
  fm <- ranger::ranger(
    formula = as.formula(rf_formula), 
    data = data,
    num.trees = 1000,
    mtry = mtry,
    sample.fraction = sample.fraction,
    min.node.size = min.node.size, 
    splitrule = "hellinger", 
    probability = TRUE, 
    class.weights = 1/c(0.05, 0.95), 
    classification = TRUE,
    replace = FALSE,
    keep.inbag = TRUE
  )
  
  list(biome_shortname = biome_shortname, fm = fm, data = data)
  
}

fitted_models <- best_models |> 
  dplyr::rename(rf_formula = important_variable_rf_formula) |> 
  purrr::pmap(
  .f = fit_best_model
)

names(fitted_models) <- purrr::map_chr(
  .x = fitted_models, 
  .f = ~purrr::pluck(.x, "biome_shortname")
  )

# Define custom predict function for ranger classification (probabilities)
ale_ranger_predict <- function(object, newdata, type) {
  # 'type' is typically passed as 'response' by ale()
  res <- predict(object, data = newdata, type = type)
  # Returns a matrix where each column is a class probability
  res$predictions[, 1]
}

# ale_results <- purrr::map(
#   .x = fitted_models,
#   .f = ~ ale::ALE(
#     model = .x[["fm"]], 
#     y_col = "ewe",
#     data = .x[["data"]],
#     pred_fun = ale_ranger_predict
#   )
# )

ale_tcf <- ale::ALE(
  model = fitted_models[["tcf"]]$fm, 
  y_col = "ewe",
  data = fitted_models[["tcf"]]$data,
  pred_fun = ale_ranger_predict
)

ale_mfws <- ale::ALE(
  model = fitted_models[["mfws"]]$fm,
  y_col = "ewe",
  data = fitted_models[["mfws"]]$data,
  pred_fun = ale_ranger_predict
  )

ale_tcf_plots <- plot(ale_tcf)
ale_mfws_plots <- plot(ale_mfws)