remotes::install_github("jimhester/archive")
library(archive)
library(sf)
library(USAboundaries)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out", recursive = TRUE, showWarnings = FALSE)

download.file(url = "https://scholar.colorado.edu/downloads/2r36tz557",
              destfile = "data/raw/FIRED_CONUS_Daily_w_README.7z")

### Currently in 7-zip format, so need external software to unzip it
### MacOS built-in unzipper works, but can't be done programmatically.
### Navigate to the file at data/raw/FIRED_CONUS_Daily_w_README.7z and
### double click to extract
fired <- sf::read_sf("data/raw/FIRED_CONUS_Daily_w_README/fired_conus_daily_nov2001-jan2019.gpkg")

us <- USAboundaries::us_states(resolution = "high")
conus <- us[!(us$name %in% c("United States Virgin Islands", "Commonwealth of the Northern Mariana Islands", "Guam", "Hawaii", "Alaska", "Puerto Rico", "American Samoa")), "name"]
conus <- sf::st_transform(conus, sf::st_crs(fired))

fired_by_state <- sf::st_intersection(fired, conus)
colnames(fired_by_state)[colnames(fired_by_state) == "name"] <- "us_state"

sf::st_write(fired_by_state, "data/out/fired_conus_daily_nov2001-jan2019_by-state.gpkg", delete_dsn = TRUE)

california_fired_ids <- fired_by_state$id[fired_by_state$us_state == "California"]

california_fired <- fired[fired$id %in% unique(california_fired_ids), ]

### nrow(california_fired) ## 15594 day/event unique combinations
### length(unique(california_fired$id)) ## 2415 unique fire events
sf::st_write(california_fired, "data/out/fired_conus_daily_nov2001-jan2019_california.gpkg", delete_dsn = TRUE)
