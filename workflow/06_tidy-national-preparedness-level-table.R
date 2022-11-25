library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

dir.create("data/out/drivers", recursive = TRUE, showWarnings = FALSE)

# https://www.nifc.gov/sites/default/files/2020-09/PreparednessLevels.xlsx
if(!file.exists("data/raw/PreparednessLevels.xlsx")) {
  download.file(url = "https://www.nifc.gov/sites/default/files/2020-09/PreparednessLevels.xlsx",
                destfile = "data/raw/PreparednessLevels.xlsx")
}

npl <- 
  lapply(X = 1:12, FUN = function(month) {
    out <- readxl::read_excel(path = "data/raw/PreparednessLevels.xlsx", 
                              sheet = month, 
                              skip = 1, 
                              col_types = c("text"))
    
    if (month == 4) {
      out <- 
        out %>% 
        dplyr::rename(`2021` = `...33`)
    }
    
    month_string <- stringr::str_pad(string = month, width = 2, side = "left", pad = "0")
    
    out <- out[1:lubridate::days_in_month(paste0("2020-", month_string, "-01")), 1:33]
    
    out <-
      out %>% 
      tidyr::pivot_longer(cols = -1, names_to = "year", values_to = "npl") %>% 
      dplyr::mutate(month = month) %>% 
      dplyr::rename(day = Day) %>% 
      dplyr::select(year, month, day, npl) %>% 
      dplyr::mutate_all(as.integer)
    
    return(out)
  })

npl <-
  dplyr::bind_rows(npl) %>% 
  dplyr::arrange(year, month, day) %>% 
  dplyr::filter(year >= 2000 & year <= 2020) %>% 
  dplyr::filter(!(is.na(npl) & !lubridate::leap_year(year)))

unique(npl$year)
summary(npl)
npl

write.csv(x = npl, file = "data/out/drivers/national-preparedness-level.csv", row.names = FALSE)
