#' @description
#' Computes sum of squares for residuals
#' @param obs observed values, y
#' @param pred predicted values, yhat
#' @returns The sum of squares of the residuals
#' 
sum_squares_residuals <- function(obs, pred) {
  sum((obs - pred)^2)
}

#' @description
#' Computes sum of squares total
#' @param obs observed values, y
#' @returns The sum of squares total
#' 
sum_squares_total <- function(obs) {
  sum((obs - mean(obs))^2)
}

#' @description
#' Computes the coefficient of determination
#' @param obs observed values, y
#' @param pred predicted values, yhat
#' @returns The coefficient of determination (R^2)
#' 
coef_of_determin <- function(obs, pred) {
  ss_res <- sum_squares_residuals(obs, pred)
  ss_tot <- sum_squares_total(obs)
  1 - (ss_res / ss_tot)
}

#' @description
#' Calculate logloss
#' @param obs observation (1 or 0)
#' @param pred prediction (1 or 0)
#' @returns a numeric

logloss <- function(o, p) {
  
  -mean(
    o * log(pmax(pmin(p, 1 - 1e-15), 1e-15)) + 
      (1 - o) * log(1 - pmax(pmin(p, 1 - 1e-15), 1e-15))
  )
}

#' @description
#' Calculate's Matthew's Correlation Coefficient
#' @param tp number of true positives (observation was positive, )
#' @param tn n
#' @returns A numeric
#' 
mcc <- function(o, p) {
  
  tp <- as.numeric(length(which(o == 1 & p == 1)))
  fp <- as.numeric(length(which(o == 0 & p == 1)))
  fn <- as.numeric(length(which(o == 1 & p == 0)))
  tn <- as.numeric(length(which(o == 0 & p == 0)))
  
  mcc <- ((tp * tn) - (fn * fp)) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  
  mcc
}

#' @description
#' Takes rsample-like data and returns in a more interoperable format
#' @param id A unique id for the split
#' @param splits the rsample split itself
#' @returns A tibble
unpack_rsample_splits <- function(id, splits) {
  
  spatial_fold <- id
  assessment_data <- splits |>
    rsample::assessment() |>
    sf::st_drop_geometry()
  
  cbind(assessment_data, spatial_fold)
}

#' @description
#' Tunes the ranger random forest model, selects variables, and measures model skill
#' using spatial cross validation 
#' @param variablesPerSplit ranger random forest hyperparameter
#' @param bagFraction ranger random forest hyperparameter
#' @param minLeafPopulation ranger random forest hyperparameter
#' @param resampling_approach One of "resampling" or "oob" to determine how
#' the conditional predictive impact works
#' @param ard A tibble representing the data to be modeled; must have already
#' been divided up into proper spatial folds with a factor attribute that
#' represents factor level/spatial fold membership
#' @param domain The name of the region that the `ard` data represent (either
#' 'western-us' or one of the ecoregions)
#' @returns A tibble with the result of every variables conditional predictive
#' impact for the given hyperparameter combination and input data
#' 
tune_validate_varselect_assess <- function(biome_shortname,
                                           tune_varselect_version,
                                           mtry, 
                                           sample.fraction, 
                                           min.node.size, 
                                           num.trees,
                                           use_class_weights,
                                           resampling_approach,
                                           target,
                                           features,
                                           ard,
                                           overwrite_model_skill = FALSE,
                                           overwrite_cpi_results = FALSE) {
  
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
  
  # subdirectory names that capture three different types of data collection
  # results of running the CPI analysis to get important variables
  cpi_results_out_dirname <- here::here(
    glue::glue(
      "data/out/rf/tune_varselect/{tune_varselect_version}/",
      "cpi-results"
    )
  )
  
  # Model skill results that are measured per spatial fold (number of rows is
  # equivalent to the number of spatial folds)
  model_skill_by_fold_out_dirname <- here::here(
    glue::glue(
      "data/out/rf/tune_varselect/{tune_varselect_version}/",
      "model-skill_by-spatial-fold"
    )
  )
  
  # Model skill results that are measured overall (number of rows is one)
  model_skill_overall_out_dirname <- here::here(
    glue::glue(
      "data/out/rf/tune_varselect/{tune_varselect_version}/",
      "model-skill_overall"
    )
  )
  
  ## The full local output filenames
  cpi_results_fname <- glue::glue(
    "{cpi_results_out_dirname}/",
    "tune_varselect_{tune_varselect_version}_",
    "{biome_shortname}_cpi-results_",
    "{hyperparameter_basename}"
  )
  
  model_skill_by_fold_fname <- glue::glue(
    "{model_skill_by_fold_out_dirname}/",
    "tune_varselect_{tune_varselect_version}_",
    "{biome_shortname}_model-skill_by-spatial-fold_",
    "{hyperparameter_basename}"
  )
  
  model_skill_overall_fname <- glue::glue(
    "{model_skill_overall_out_dirname}/",
    "tune_varselect_{tune_varselect_version}_",
    "{biome_shortname}_model-skill_overall_",
    "{hyperparameter_basename}"
  )
  
  # Using the model_skill_overall_fname filename to check whether the 
  # hyperparameter set has been analyzed, but could use by spatial fold also
  if(!file.exists(model_skill_overall_fname) | 
     !file.exists(model_skill_by_fold_fname) | 
     overwrite_model_skill) {
    
    if (!use_class_weights) {
      class.weights <- c(1, 1)
    } else {
      class.weights <- 1 / table(ard$ewe)
    }
    
    overall_n <- nrow(ard)
    by_fold_n <- ard |> 
      dplyr::group_by(spatial_fold) |> 
      dplyr::tally() |> 
      dplyr::mutate(iteration = as.numeric(spatial_fold),
                    spatial_fold = as.character(spatial_fold))
    
    # Model skill measures that we want to record
    model_skill_colnames <- c("tp", "tn", "fp", "fn", "mcc", "logloss")
    
    classif_measures <- purrr::map(
      .x = paste0("classif.", model_skill_colnames), 
      .f = msr
    )
    
    classif_measures_pool_first <- purrr::map(
      .x = paste0("classif.", model_skill_colnames), 
      .f = msr, 
      average = "micro"
    )
    
    # Set up the leaner with the (currently) 3 hyperparameters
    learner_ewe <- mlr3::lrn(
      .key = "classif.ranger",
      predict_type = "prob",
      mtry = mtry,
      num.trees = num.trees,
      sample.fraction = sample.fraction,
      min.node.size = min.node.size,
      class.weights = class.weights,
      replace = FALSE,
      splitrule = "hellinger",
      num.threads = 1,
      keep.inbag = TRUE
    )
    
    # The full model formula
    full_rf_formula <- glue::glue(
      "{target} ~ {paste(features, collapse = ' + ')}"
    )
    
    # Set up the task using the formula notation with the full set of predictors
    task_ewe <- mlr3::as_task_classif(
      x = as.formula(full_rf_formula),
      data = ard[, c(target, features)],
      id = target
    )
    
    # Set up and instantiate the resampler using the known spatial folds as the
    # groups
    resampler_ewe <- rsmp("custom_cv")
    resampler_ewe$instantiate(
      task_ewe, 
      f = ard$spatial_fold
    )
    
    # Spatially cross validated model assessment the {mlr3} way
    assessment_full <- resample(
      task = task_ewe, 
      learner = learner_ewe, 
      resampling = resampler_ewe
    )
    
    # Get per-fold model skill measure
    skill_metrics_by_spatial_fold_full <- assessment_full$score(
      measures = classif_measures
    ) |> 
      tibble::as_tibble() |> 
      dplyr::select(iteration, tidyselect::starts_with("classif")) |> 
      dplyr::mutate(model_type = "full")
    
    skill_metrics_overall_full <- assessment_full$aggregate(
      measures = classif_measures_pool_first
    ) |> 
      tibble::enframe() |> 
      tidyr::pivot_wider() |> 
      dplyr::mutate(model_type = "full")
    
    if(!file.exists(cpi_results_fname) | overwrite_cpi_results) {
      # Find important variables, reduce the model, and refit
      if (resampling_approach == "resampler") {
        
        # Calculate conditional predictive impact
        cpi_results <- cpi::cpi(
          task = task_ewe,
          learner = learner_ewe,
          measure = "classif.logloss",
          resampling = resampler_ewe,
          test = "t"
        )
        
      } else if (resampling_approach == "oob") {
        
        cpi_results <- cpi::cpi(
          task = task_ewe,
          learner = learner_ewe,
          measure = "classif.logloss",
          resampling = "oob",
          test = "t"
        )
        
      }
      
      cpi_results_out <- cpi_results |> 
        dplyr::mutate(
          tune_varselect_version = tune_varselect_version,
          biome_shortname = biome_shortname,
          n = overall_n,
          mtry = mtry,
          sample.fraction = sample.fraction,
          min.node.size = min.node.size
        )
      
      dir.create(
        dirname(cpi_results_fname), 
        recursive = TRUE, 
        showWarnings = FALSE
      )
      
      readr::write_csv(x = cpi_results_out, file = cpi_results_fname)
      
    } # end check if CPI results were created already
    
    cpi_results <- data.table::fread(input = cpi_results_fname)
    
    # Important variables are those that have a significant positive conditional
    # predictive impact
    important_variables <- cpi_results |> 
      dplyr::filter(ci.lo > 0) |> 
      dplyr::pull(Variable)
    
    # Create the formula that could be used for the next iteration of the 
    # spatial cross validation (using the reduced set of 
    # only important predictors)
    important_variable_rf_formula = glue::glue(
      "{target} ~ {paste(important_variables, collapse = ' + ')}"
    )
    
    if (mtry <= length(important_variables)) {
      # Set up the task using the formula notation with the 
      # full set of predictors
      task_ewe_important_variables <- mlr3::as_task_classif(
        x = as.formula(important_variable_rf_formula),
        data = ard[, c(target, important_variables)],
        id = target
      )
      
      resampler_ewe_important_variables <- rsmp("custom_cv")
      resampler_ewe_important_variables$instantiate(
        task_ewe_important_variables, 
        f = ard$spatial_fold
      )
      
      # Spatially cross validated model assessment the {mlr3} way
      assessment_important_variables <- resample(
        task = task_ewe_important_variables, 
        learner = learner_ewe, 
        resampling = resampler_ewe_important_variables
      )
      
      skill_metrics_by_spatial_fold_important_variables <- assessment_important_variables$score(
        measures = classif_measures
      ) |> 
        tibble::as_tibble() |>
        dplyr::mutate(n_obs = purrr::map_int(.x = prediction_test, .f = \(x) length(x$row_ids))) |> 
        dplyr::select(iteration, tidyselect::starts_with("classif")) |> 
        dplyr::mutate(model_type = "reduced")
      
      skill_metrics_overall_important_variables <- assessment_important_variables$aggregate(
        measures = classif_measures_pool_first
      ) |> 
        tibble::enframe() |> 
        tidyr::pivot_wider() |> 
        dplyr::mutate(model_type = "reduced")
      
    } else {
      
      # If mtry is greater than the number of important variables then make
      # all the metrics NA because using an mtry that is larger than the number
      # of variables in the model won't be meaningful
      skill_metrics_by_spatial_fold_important_variables <- skill_metrics_by_spatial_fold_full |> 
        dplyr::mutate(
          dplyr::across(
            .cols = tidyselect::starts_with("classif"), 
            .fns = \(x) NA
          )
        ) |> 
        dplyr::mutate(model_type = "reduced")
      
      skill_metrics_overall_important_variables <- skill_metrics_overall_full |> 
        dplyr::mutate(
          dplyr::across(
            .cols = tidyselect::starts_with("classif"), 
            .fns = \(x) NA
          )
        ) |> 
        dplyr::mutate(model_type = "reduced")
      
      
    }
    
    # build final output data frames to write to disk
    model_skill_by_fold <- rbind(
      skill_metrics_by_spatial_fold_full,
      skill_metrics_by_spatial_fold_important_variables
    ) |> 
      dplyr::rename_with(
        .fn = \(x) gsub(x = x, pattern = "classif.", replacement = ""), 
        .cols = tidyselect::starts_with("classif")
      ) |> 
      tidyr::pivot_wider(
        id_cols = "iteration", 
        values_from = tidyselect::all_of(model_skill_colnames),
        names_from = "model_type",
        names_vary = "slowest"
      ) |> 
      dplyr::mutate(
        biome_shortname = biome_shortname,
        mtry = mtry,
        sample.fraction = sample.fraction,
        min.node.size = min.node.size
      ) |> 
      dplyr::left_join(by_fold_n, by = "iteration") |> 
      dplyr::select(-iteration) |> 
      dplyr::mutate(
        tune_varselect_version = tune_varselect_version,
        n_important_variables = length(important_variables),
        important_variable_rf_formula
      ) |> 
      dplyr::select(
        tune_varselect_version,
        biome_shortname, mtry, sample.fraction, min.node.size, 
        spatial_fold, n,
        n_important_variables, important_variable_rf_formula,
        mcc_full, mcc_reduced,
        logloss_full, logloss_reduced,
        tidyselect::everything()
      )
    
    model_skill_overall <- rbind(
      skill_metrics_overall_full,
      skill_metrics_overall_important_variables
    ) |> dplyr::rename_with(
      .fn = \(x) gsub(x = x, pattern = "classif.", replacement = ""), 
      .cols = tidyselect::starts_with("classif")
    ) |> 
      tidyr::pivot_wider(
        values_from = tidyselect::all_of(model_skill_colnames),
        names_from = "model_type",
        names_vary = "slowest"
      ) |> 
      dplyr::mutate(
        tune_varselect_version = tune_varselect_version,
        biome_shortname = biome_shortname, 
        mtry = mtry,
        sample.fraction = sample.fraction,
        min.node.size = min.node.size,
        n = overall_n,
        n_important_variables = length(important_variables),
        important_variable_rf_formula = important_variable_rf_formula
      ) |> 
      dplyr::select(
        tune_varselect_version,
        biome_shortname, mtry, sample.fraction, min.node.size, n,
        n_important_variables, important_variable_rf_formula,
        mcc_full, mcc_reduced,
        logloss_full, logloss_reduced,
        tidyselect::everything()
      )
    
    dir.create(dirname(model_skill_by_fold_fname), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(model_skill_overall_fname), recursive = TRUE, showWarnings = FALSE)
    
    readr::write_csv(x = model_skill_by_fold, file = model_skill_by_fold_fname)
    readr::write_csv(x = model_skill_overall, file = model_skill_overall_fname)
    
  }
  
  cpi_results_out <- data.table::fread(input = cpi_results_fname)
  model_skill_by_fold <- data.table::fread(input = model_skill_by_fold_fname)
  model_skill_overall <- data.table::fread(input = model_skill_overall_fname)
  
  list(
    cpi_results = cpi_results_out,
    model_skill_by_fold = model_skill_by_fold,
    model_skill_overall = model_skill_overall
  )
}

#' @description
#' Reads in a set of CPI results and summarizes the estimated conditional
#' predictive impact per variable for the unique hyperparameter/biome 
#' combination, calculating a lower bounds for the 95% confidence interval
#' of the estimated CPI (the mean CPI for that variable across all the 
#' iterations). Also memorializes the number of iterations that went into
#' deriving the mean CPI and confidence interval
#' @param biome_shortname
#' @param mtry
#' @param sample.fraction
#' @param min.node.size
#' @param tune_varselect_versions_to_collate A character vector of the versions
#' of CPI results to pull together and summarize
#' @param new_tune_varselect_version The new entry for tune_varselect_version
#' such that the collated versions are separated from the individual iteration
#' runs. We will use patch version in Arabic numbers to denote different
#' iterations, and lowercase letters to denote different collations of versions
#' E.g., v0.1.0 is a single iteration of CPI results (and model skill 
#' assessment) for a suite of hyperparameter sets. v0.1.a would be the CPI
#' results collated across a number of v0.1.[0-9] to produce a new set of 
#' model skill assessments and CPI results
#' @param overwrite_collated_cpi_results Should the collated cpi_results be
#' overwritten if they've already been created?
#' @returns A tibble with the side effect of writing new CPI results to disk

collate_cpi_results <- function(biome_shortname,
                                mtry,
                                sample.fraction,
                                min.node.size,
                                tune_varselect_versions_to_collate, 
                                new_tune_varselect_version,
                                overwrite_collated_cpi_results = FALSE,
                                ...) {
  
  
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
  cpi_results_fnames_to_collate <- glue::glue(
    "data/out/rf/tune_varselect/{tune_varselect_versions_to_collate}/",
    "cpi-results/",
    "tune_varselect_{tune_varselect_versions_to_collate}_",
    "{biome_shortname}_cpi-results_",
    "{hyperparameter_basename}"
  )
  
  cpi_results_collated_fname <- glue::glue(
    "data/out/rf/tune_varselect/{new_tune_varselect_version}/",
    "cpi-results/",
    "tune_varselect_{new_tune_varselect_version}_",
    "{biome_shortname}_cpi-results_",
    "{hyperparameter_basename}"
  )
  
  if(
    !file.exists(cpi_results_collated_fname) | overwrite_collated_cpi_results
  ) {
    
    cpi_results_all_iter <- purrr::map(
      .x = cpi_results_fnames_to_collate,
      .f = data.table::fread
    ) |> 
      data.table::rbindlist()
    
    cpi_results <- cpi_results_all_iter |> 
      dplyr::group_by(
        Variable, 
        biome_shortname, 
        n, 
        mtry, 
        sample.fraction, 
        min.node.size
      ) |> 
      dplyr::summarize(
        n_iter = dplyr::n(),
        cpi_mean = mean(CPI),
        ci.lo = getElement(getElement(t.test(CPI), name = "conf.int"), 1),
        .groups = "drop"
      ) |> 
      dplyr::mutate(tune_varselect_version = new_tune_varselect_version)
    
    dir.create(
      path = dirname(cpi_results_collated_fname), 
      recursive = TRUE, 
      showWarnings = FALSE
    )
    
    readr::write_csv(x = cpi_results, file = cpi_results_collated_fname)
    
  }
  
  cpi_results <- data.table::fread(cpi_results_collated_fname)
  
  cpi_results
  
}

#' @description
#' Performs a cross-validation of a ranger random forest model
#' @param data 
#' @param hyperparameters A tibble representing the hyperparameter values to use
#' for the random forest model
#' @returns A tibble with each row representing the observed percent basal area
#' loss for one observation and the model prediction using a model trained on
#' all the data not belonging to the observation's spatial fold

cross_validate <- function(data, hyperparameters) {
  results <- list() 
  
  spatial_folds <- unique(data$spatial_fold)
  
  for(i in seq_along(spatial_folds)) {
    
    train_data = data |> 
      dplyr::filter(spatial_fold != spatial_folds[i])
    
    test_data = data |> 
      dplyr::filter(spatial_fold == spatial_folds[i])
    
    fm = ranger::ranger(
      formula = as.formula(hyperparameters$important_variable_rf_formula), 
      data = train_data, 
      num.trees = 1000, 
      mtry = hyperparameters$mtry, 
      min.node.size = hyperparameters$min.node.size, 
      sample.fraction = hyperparameters$sample.fraction
    )
    # Store results with UniqueID and BA loss
    fold_results <- data.frame(
      unique_id = test_data$UniqueID,  
      obs = test_data$pcnt_ba_mo,  
      pred = predict(object = fm, data = test_data)$predictions,
      fold = i  
    )
    
    # Append to the list of results
    results[[i]] <- fold_results
  }
  
  # Combine all fold results into a single data frame
  combined_results <- do.call(rbind, results)
  
  # Check for potential mismatches
  if (any(is.na(combined_results$obs) | is.na(combined_results$pred))) {
    warning("NA values found in observations or predictions.")
  }
  
  combined_results
  
}


#' @description
#' Merges together all the disparate data source
#' @returns A tibble suitable for final data quality checks

collate_ard <- function(fluc_static_fname, 
                        landfire_fname,
                        wx_fname,
                        fired_events_fname,
                        other_fires_summary_fname,
                        fired_behavior_metrics_fname) {
  
  fluc_static <- data.table::fread(
    input = fluc_static_fname
  ) |>
    dplyr::select(
      did, id, date,
      elevation, rumple_index, caltrans_road_density_mpha,
      ndvi, veg_structure_rumple, 
      peak_ridge_cliff, valleys, slope_warm, slope_cool, slope_neutral, flat, 
      trees_tm01, shrubs_tm01, grass_forb_herb_tm01, barren_tm01,
      landform_diversity, landcover_diversity_tm01
    )
  
  landfire <- data.table::fread(
    input = landfire_fname
  ) |>
    dplyr::select(
      did, id, date,
      fire_high_tm01_tm05, fire_high_tm06_tm10,
      fire_not_high_tm01_tm05, fire_not_high_tm06_tm10,
      insect_disease_tm01_tm10
    )
  
  # ERA5 Land data
  # original
  weather_drivers <- data.table::fread(
    input = wx_fname
  ) |>
    dplyr::select(!tidyselect::contains("rtma")) |> # not the RTMA drivers; use the ERA5 drivers in their place
    dplyr::select(
      did, id, date,
      wind_dir_ns_era5, wind_dir_ew_era5,
      wind_anisotropy_ns_era5, wind_anisotropy_ew_era5,
      min_wind_speed_era5_pct, max_wind_speed_era5_pct,
      min_rh_era5_pct, max_rh_era5_pct, min_temp_era5_pct, max_temp_era5_pct, 
      min_vpd_era5_pct, max_vpd_era5_pct,
      spei14d, spei30d, spei90d, spei180d, spei270d, spei1y, spei2y, spei5y, 
      pdsi_z, erc_pct, bi_pct, fm100_pct, fm1000_pct
    ) |>  
    dplyr::filter(did %in% fluc_static$did)
  
  other_fires_summary <- data.table::fread(
    input = other_fires_summary_fname
  ) |>
    dplyr::select(-concurrent_fires, -cumu_count, -cumu_area_ha) |>  
    dplyr::filter(did %in% fluc_static$did)
  
  # Remove fires that never reached more than 121 hectares (300 acres)
  target_event_ids <- sf::read_sf(
    fired_events_fname
  )
  
  target_event_ids <- target_event_ids |> 
    dplyr::mutate(area_ha = as.numeric(sf::st_area(target_event_ids)) / 10000) |> 
    dplyr::filter(area_ha >= 121.406) |> 
    dplyr::pull(id)
  
  # Using ERA5 means we can go back to 2003
  drivers <-
    merge(x = weather_drivers, y = other_fires_summary, by = c("did", "id", "date"), all = TRUE) |>
    merge(y = fluc_static, by = c("did", "id", "date"), all = TRUE) |>
    merge(y = landfire, by = c("did", "id", "date"), all = TRUE) |>
    dplyr::filter(id %in% target_event_ids) |>
    dplyr::filter(date >= as.Date("2003-01-01"))
  
  # For defining "ewe" or not, what is the percentage threshold? E.g., 0.95 means an "ewe" is in the top
  # 5th percentile for daily area of increase
  pct_threshold <- 0.95
  
  # Defining "ewe" as whether daily area of increase was >95th percentile for the biome
  biome_lookup <- 
    tibble::tibble(
      biome_name_daily = c("Temperate Conifer Forests", 
                           "Mediterranean Forests, Woodlands & Scrub", 
                           "Temperate Grasslands, Savannas & Shrublands", 
                           "Deserts & Xeric Shrublands"),
      biome_shortname = c("tcf", "mfws", "tgss", "dxs")
    )
  
  # We need to get X/Y coordinates for biggest polygon
  fired_biggest_poly <- 
    data.table::fread("data/out/fired/03_joined-with-other-data/fired-biggest-poly-info.csv") |>
    dplyr::select(did, id, date, samp_id, x_biggest_poly_3310, y_biggest_poly_3310)
  
  fires <- data.table::fread(
    input = fired_behavior_metrics_fname
  ) |>
    dplyr::mutate(
      area_log10 = log10(daily_area_ha),
      sqrt_aoi_tm1 = sqrt(daily_area_tminus1_ha),
      fireline_length_proxy_km = sqrt((sqrt_aoi_tm1^2*1e4)/pi)/1e3*pi
    ) |>
    dplyr::rename(cumu_area_tm01 = cum_area_ha_tminus1) |>
    dplyr::left_join(biome_lookup, by = "biome_name_daily")
  
  fires <-
    merge(x = fires, y = fired_biggest_poly, by = c("did", "id", "date", "samp_id")) |>
    dplyr::select(did, id, date, biome_shortname, biome_name_daily, eco_name_daily,  x_biggest_poly_3310, y_biggest_poly_3310, 
                  daily_area_ha, area_log10, fireline_length_proxy_km, sqrt_aoi_tm1, event_day, cumu_area_tm01)
  
  # Merge fire data with drivers data
  collated_ard <- merge(drivers, fires, by = c("did", "id", "date")) |>
    # dplyr::group_by(biome_name_daily) |> 
    dplyr::mutate(area_log10_pct = ecdf(area_log10)(area_log10),
                  ewe = ifelse(area_log10_pct >= pct_threshold, yes = 1, no = 0)) |> 
    # dplyr::ungroup() |>
    # dplyr::mutate(early_late = as.numeric(event_day <= 7)) |> 
    as.data.frame()
  
  collated_ard
}

#' @description
#' Finalizes analysis ready data by biome by doing some data quality checks and
#' dropping some rows
#' @returns A tibble

create_ard_by_biome <- function(fires_drivers, predictor.variable.names) {
  
  # Drop all rows that have an NA in any column
  out <- fires_drivers[complete.cases(fires_drivers), ]
  
  out <- out[
    , 
    c(
      "did", "event_day", "daily_area_ha", "cumu_area_tm01", "ewe", 
      "biome_name_daily", "biome_shortname", "eco_name_daily", 
      "x_biggest_poly_3310", "y_biggest_poly_3310", predictor.variable.names
    )
  ]
  
  # set up spatial folds
  # https://spatialsample.tidymodels.org/articles/spatialsample.html
  out <- out |> 
    sf::st_as_sf(coords = c("x_biggest_poly_3310", "y_biggest_poly_3310"), 
                 crs = 3310, remove = FALSE) |>
    spatialsample::spatial_clustering_cv(v = 10) |> 
    purrr::pmap(.f = function(id, splits) {
      spatial_fold <- id
      assessment_data <- splits |> 
        rsample::assessment() |> 
        sf::st_drop_geometry()
      
      return(cbind(assessment_data, spatial_fold))
    }) |> 
    data.table::rbindlist()
  
  # Remove spatial folds with fewer than 40 observations or 2 EWE's
  n_in_fold <- out |> 
    dplyr::group_by(spatial_fold) |> 
    dplyr::summarize(n = dplyr::n(),
                     n_ewe = length(which(ewe == 1)))
  
  enough_n_folds <- 
    n_in_fold |> 
    dplyr::filter(n_ewe >= 1) |> 
    dplyr::pull(spatial_fold)
  
  out <- out[spatial_fold %in% enough_n_folds, ]
  
  # no columns with 0 variance (rounded to 4 decimal places) across whole dataset
  zero_variance_columns <-
    out |>
    tidyr::pivot_longer(cols = tidyselect::all_of(predictor.variable.names),
                        names_to = "variable",
                        values_to = "value") |>
    dplyr::group_by(variable) |>
    dplyr::summarize(zero_var = any(round(var(value, na.rm = TRUE), digits = 4) == 0)) |>
    dplyr::filter(zero_var) |>
    dplyr::pull(variable)
  
  out <- out |> 
    dplyr::select(!tidyselect::all_of(zero_variance_columns))
  
  out
}

