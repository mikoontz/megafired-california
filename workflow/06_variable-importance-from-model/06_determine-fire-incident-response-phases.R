library(dplyr)
library(data.table)
library(ggplot2)

ics <- 
  fread("data/raw/ics209-plus-wildfire/ics209-plus-wildfire/ics209-plus-wf_sitreps_1999to2014.csv") %>% 
  dplyr::filter(POO_STATE == "CA" & !(IMT_MGMT_ORG_DESC %in% c("FUM1", "FUM2", "SOPL", "FUMT"))) %>% 
  dplyr::mutate(complexity = dplyr::case_when(IMT_MGMT_ORG_DESC %in% c("Type 2 Team", "Type 1 Team", "Area Command", "Type 2 IC", "Type 1 IC") ~ "complex",
                                              IMT_MGMT_ORG_DESC %in% c("Type 3 Team", "Type 3 IC", "Unified Command", "Type 5 IC", "Type 4 IC") ~ "simple")) %>% 
  dplyr::filter(!is.na(complexity))

events_that_become_complex <-
  ics %>% 
  dplyr::group_by(FIRE_EVENT_ID) %>% 
  dplyr::summarize(n_complexity_classes = length(unique(complexity))) %>% 
  dplyr::arrange(desc(n_complexity_classes)) %>% 
  dplyr::filter(n_complexity_classes == 2)

last_simple_size <- 
  ics %>% 
  dplyr::select(FIRE_EVENT_ID, REPORT_TO_DATE, NEW_ACRES, IMT_MGMT_ORG_DESC, complexity) %>% 
  dplyr::filter(FIRE_EVENT_ID %in% events_that_become_complex$FIRE_EVENT_ID) %>% 
  dplyr::group_by(FIRE_EVENT_ID) %>% 
  dplyr::arrange(REPORT_TO_DATE) %>% 
  dplyr::mutate(cum_ha = 0.404686 * cumsum(NEW_ACRES)) %>% 
  dplyr::mutate(simple_to_complex = as.numeric(lead(complexity) == "complex" & complexity == "simple")) %>% 
  dplyr::filter(simple_to_complex == 1) %>% 
  dplyr::ungroup()

last_simple_size %>% arrange(desc(cum_ha))

zero_offset <- 0.1
mean_switching_cumarea <- 10^mean(log10(last_simple_size$cum_ha + zero_offset))
mean_switching_cumarea / 0.404686
# 880.4882
median_switching_cumarea <- 10^median(log10(last_simple_size$cum_ha + zero_offset))
median_switching_cumarea / 0.404686
# 800.2471

last_simple_gg <-
  ggplot2::ggplot(last_simple_size, aes(x = cum_ha + zero_offset)) +
  geom_histogram() +
  scale_x_log10(labels = c("0", "10", "100", "1,000", "10,000", "100,000"), breaks = c(zero_offset, 10^(1:5))) +
  labs(x = "Cumulative area prior to switching to\nincident management designed for a complex event (ha)",
       y = "Count") +
  geom_vline(xintercept = mean_switching_cumarea, color = "red") +
  # geom_vline(xintercept = median_switching_cumarea, color = "blue") + 
  theme_bw()

ggsave(filename = "figs/switching-size-to-complex-event.png", plot = last_simple_gg)

