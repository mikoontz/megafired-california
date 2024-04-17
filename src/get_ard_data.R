get_ard_data <- function(model_path) {
  fitted_model <- readr::read_rds(model_path)
  
  ard = fitted_model$forest$data
  
  return(ard)
}

ard_tcf_early <- get_ard_data(
  model_path = here::here("data/out/rf/fitted/early/2023-06-21/rf_ranger_fitted-model_rtma_tcf_early.rds")
)

ard_tcf_late <- get_ard_data(
  model_path = here::here("data/out/rf/fitted/late/2023-06-21/rf_ranger_fitted-model_rtma_tcf_late.rds")
)

ard_mfws_early <- get_ard_data(
  model_path = here::here("data/out/rf/fitted/early/2023-06-21/rf_ranger_fitted-model_rtma_mfws_early.rds")
)

ard_tcf_late <- get_ard_data(
  model_path = here::here("data/out/rf/fitted/late/2023-06-21/rf_ranger_fitted-model_rtma_mfws_late.rds")
)