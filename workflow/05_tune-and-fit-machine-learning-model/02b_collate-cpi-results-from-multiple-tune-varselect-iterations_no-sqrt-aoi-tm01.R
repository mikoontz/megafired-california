source("./R/utils.R")

tune_varselect_versions_to_collate <- glue::glue("v0.2.0")
new_tune_varselect_version <- "v0.2.a"

hyperparameters_for_collating_cpi <- tidyr::expand_grid(
  biome_shortname = c("tcf", "mfws"),
  mtry = 3:11,
  sample.fraction = c(0.1, 0.3, 0.5, (1 - 1/exp(1)), 0.7, 0.8, 0.9),
  min.node.size = c(1, 5, 10, 25, 50, 60),
  tune_varselect_versions_to_collate = list(tune_varselect_versions_to_collate),
  new_tune_varselect_version = new_tune_varselect_version,
  overwrite_collated_cpi_results = FALSE
) |>
  dplyr::arrange(biome_shortname, min.node.size, sample.fraction, mtry)

cpi_all_iter <- purrr::pmap(
  .l = hyperparameters_for_collating_cpi,
  .f = collate_cpi_results,
  .progress = list(
    name = "Collate CPI results", 
    type = "iterator", 
    clear = TRUE
  )
)

cpi_all_iter |> 
  dplyr::group_by(biome_shortname, mtry, sample.fraction, min.node.size, n_iter, new_tune_varselect_version) |> 
  dplyr::filter(cpi.lo > 0) |> 
  dplyr::summarize(important_variables = paste(Variable, collapse = " + "))

# # testing
# idx <- 700
# biome_shortname = hyperparameters_for_collating_cpi$biome_shortname[[idx]]
# mtry = hyperparameters_for_collating_cpi$mtry[[idx]]
# sample.fraction = hyperparameters_for_collating_cpi$sample.fraction[[idx]]
# min.node.size = hyperparameters_for_collating_cpi$min.node.size[[idx]]
# tune_varselect_versions_to_collate = hyperparameters_for_collating_cpi$tune_varselect_versions_to_collate[[idx]]
# new_tune_varselect_version = hyperparameters_for_collating_cpi$new_tune_varselect_version[[idx]]
# 
# cpi_all_iter <- purrr::pmap(
#   .l = hyperparameters_for_collating_cpi[idx, ],
#   .f = collate_cpi_results
# )

