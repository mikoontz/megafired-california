drivers <- readr::read_csv(file = "./docs/tables/drivers.csv")

drivers |> 
  dplyr::group_by(biome_shortname, ewe) |> 
  dplyr::summarize(
    n = dplyr::n(),
    min_size = min(daily_area_ha)
  )

ese_summary_table <- drivers |> 
  dplyr::group_by(biome_shortname) |> 
  dplyr::summarize(
    n = dplyr::n(),
    n_ese = sum(ewe),
    min_ese_size = min(daily_area_ha * 1/ewe)
  )

readr::write_csv(x = ese_summary_table, file = "./docs/tables/ese-summary-table.csv")
