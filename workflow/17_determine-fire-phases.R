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

ggplot2::ggplot(last_simple_size, aes(x = cum_ha)) +
  geom_histogram() +
  scale_x_log10()

ics[ics$FIRE_EVENT_ID == "CA-LPF-001087|2007|1", ]

10^mean(log10(last_simple_size$cum_ha + 1)) / 0.404686

last_simple_size[last_simple_size$cum_ha == 0, ]

test <- sf::read_sf("C:/Users/mikoo/Downloads/cbi_data_review_v4(1)/conus_cbi_v4.shp")
names(test)
