driver_descriptions <- read.csv("data/out/drivers/driver-descriptions.csv")
full_predictor_variable_names <- driver_descriptions$variable

latest_ard_date_early <- sort(list.files(path = here::here("data", "ard", "early")), 
                              decreasing = TRUE)[1]
latest_ard_date_late <- sort(list.files(path = here::here("data", "ard", "late")), 
                             decreasing = TRUE)[1]

latest_ard_dir_early <- here::here("data", "ard", "early", latest_ard_date_early)
latest_ard_dir_late <- here::here("data", "ard", "late", latest_ard_date_late)

ard_early <- purrr::map(
  .x = c("tcf", "mfws"), 
  .f = \(biome_shortname) {
    read.csv(paste0(latest_ard_dir_early, "/daily-drivers-of-california-megafires_", biome_shortname,"_early.csv"))
  }
) |> 
  data.table::rbindlist() |> 
  dplyr::mutate(early_or_late = "early")

ard_late <- purrr::map(
  .x = c("tcf", "mfws"), 
  .f = \(biome_shortname) {
    read.csv(paste0(latest_ard_dir_late, "/daily-drivers-of-california-megafires_", biome_shortname,"_late.csv"))
  }
) |> 
  data.table::rbindlist() |> 
  dplyr::mutate(early_or_late = "late")

ard = rbind(ard_early, ard_late)
full_predictor_variable_names

driver_descriptions
purrr::map(
  .x = 1:nrow(driver_descriptions), 
  .f = \(i) {
    
    variable_name = driver_description$variable[i]
    plot_title = driver_description$display_name[i]
    
    plot_data |> 
      dplyr::select(early_or_late, biome_name_daily) |> 
      table()
    
    plot_data = ard |> 
      dplyr::select(did, early_or_late, ewe, biome_name_daily, !!variable_name) |> 
      dplyr::rename(feature = !!variable_name)
    
    xintercepts = plot_data |> 
      dplyr::group_by(early_or_late, biome_name_daily) |> 
      dplyr::summarize(xintercept = mean(feature))
    
    ggplot(data = plot_data, aes(x = feature, fill = biome_name_daily)) +
      geom_histogram() +
      geom_text(data = xintercepts, aes(x = 0, y = 200, label = round(xintercept, 3))) +
      geom_vline(data = xintercepts, aes(xintercept = xintercept, color = "red"), lwd = 1.1) +
      facet_grid(early_or_late ~ biome_name_daily) + 
      scale_fill_manual(values = c("blue", "forestgreen")) +
      labs(
        fill = "Biome type", 
        color = "Mean value"
      ) +
      theme_bw() + 
      ggtitle(plot_title)
  
    ggsave(filename = here::here(glue::glue("figs/covariate-histograms/{variable_name}.png")))  
  },
  .progress = TRUE
)


plot_data = ard_early |> 
  dplyr::select(did, event_day, ewe, biome_name_daily, !!full_predictor_variable_names[i]) |> 
  dplyr::rename(feature = !!full_predictor_variable_names[i])

ggplot(data = plot_data, aes(x = feature, fill = biome_name_daily)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(feature), color = "red"), lwd = 1.1) +
  facet_wrap(facets = "biome_name_daily") + 
  scale_fill_manual(values = c("blue", "forestgreen")) +
  labs(
    fill = "Biome type", 
    color = "Mean value"
  ) +
  theme_bw() + 
  ggtitle(full_predictor_variable_names[i])
