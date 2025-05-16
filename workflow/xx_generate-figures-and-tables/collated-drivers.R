driver_paths <- tidyr::expand_grid(
  biome = c("tcf", "mfws", "tgss", "dxs"),
  early_late = c("early", "late"),
  date = "2023-06-21"
) |> 
  dplyr::mutate(
    driver_path = glue::glue(
      "./data/ard/{early_late}/{date}/daily-drivers-of-california-megafires_{biome}_{early_late}.csv"
    ),
    short_name = glue::glue("{biome}_{early_late}")
  )

drivers <- purrr::map(driver_paths$driver_path, .f = readr::read_csv) |> 
  purrr::list_rbind() |> 
  dplyr::mutate(early_late = ifelse(test = event_day < 8, yes = "early", no = "late"))

drivers

readr::write_csv(x = drivers, file = "./docs/tables/drivers.csv")
