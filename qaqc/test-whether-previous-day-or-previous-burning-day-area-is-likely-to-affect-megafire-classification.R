fires_orig <- data.table::fread(
  input = "data/out/fired/05_daily-with-behavior-metrics/fired_daily_ca_behavior-metrics.csv"
)

fires <- fires_orig |>
  dplyr::rename(cumu_area_tm01 = cum_area_ha_tminus1) |> 
  dplyr::select(id, did, date, event_day, daily_area_ha)

skeleton <- fires[, .(event_day = seq(min(event_day), max(event_day))), by = id]

fires_complete <- fires[skeleton, on = .(id, event_day)]

fires_complete[is.na(daily_area_ha), daily_area_ha := 0]
fires_complete[, date := min(date, na.rm = TRUE) + (event_day - 1), by = id]
fires_complete[, did := paste0(id, "-", format(date, "%Y-%m-%d"))]
fires_complete[, daily_area_tminus1_ha2 := data.table::shift(daily_area_ha, n = 1, fill = 0, type = "lag"), by = id]

new <- fires_complete[daily_area_ha > 0] |> 
  merge(fires_orig, by = c("id", "did", "date", "event_day", "daily_area_ha")) |> 
  dplyr::select(biome_name_daily, id, did, date, event_day, daily_area_ha, daily_area_tminus1_ha, daily_area_tminus1_ha2) |> 
  dplyr::mutate(
    daily_area_ha_pct = ecdf(daily_area_ha)(daily_area_ha),
    ewe = ifelse(daily_area_ha_pct >= 0.95, yes = 1, no = 0)
  )

new |> 
  dplyr::group_by(biome_name_daily, ewe) |> 
  dplyr::summarize(
    daily_area_tminus1_ha = mean(sqrt(daily_area_tminus1_ha)),
    daily_area_tminus1_ha2 = mean(sqrt(daily_area_tminus1_ha2))
  )
