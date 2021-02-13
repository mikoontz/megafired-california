# remotes::install_github("jimhester/archive")
# library(archive)
library(sf)
library(USAboundaries)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/out", recursive = TRUE, showWarnings = FALSE)

# Data source for FIRED daily perimeters
# https://scholar.colorado.edu/concern/datasets/765372341
# download.file(url = "https://scholar.colorado.edu/downloads/2r36tz557",
#               destfile = "data/raw/FIRED_CONUS_Daily_w_README.7z")

# Data source for FIRED events
# https://scholar.colorado.edu/concern/datasets/nv935382p
# download.file(url = "https://scholar.colorado.edu/concern/parent/nv935382p/file_sets/hx11xg09n",
#               destfile = "data/raw/fired_events_conus_nov2001-jan2019.gpkg")


### Currently in 7-zip format, so need external software to unzip it
### MacOS built-in unzipper works, but can't be done programmatically.
### Navigate to the file at data/raw/FIRED_CONUS_Daily_w_README.7z and
### double click to extract
fired_daily <- sf::read_sf("data/raw/FIRED_CONUS_Daily_w_README/fired_conus_daily_nov2001-jan2019.gpkg")

us <- USAboundaries::us_states(resolution = "high")
conus <- us[!(us$name %in% c("United States Virgin Islands", "Commonwealth of the Northern Mariana Islands", "Guam", "Hawaii", "Alaska", "Puerto Rico", "American Samoa")), "name"]
conus <- sf::st_transform(conus, sf::st_crs(fired_daily))

fired_daily_by_state <- sf::st_intersection(fired_daily, conus)
colnames(fired_daily_by_state)[colnames(fired_daily_by_state) == "name"] <- "us_state"

sf::st_write(fired_daily_by_state, "data/out/fired_conus_daily_nov2001-jan2019_by-state.gpkg", delete_dsn = TRUE)

california_fired_daily_ids <- fired_daily_by_state$id[fired_daily_by_state$us_state == "California"]

california_fired_daily <- fired_daily[fired_daily$id %in% unique(california_fired_daily_ids), ]

california_fired_daily <- sf::st_transform(california_fired_daily, 3310)
### nrow(california_fired_daily) ## 15594 day/event unique combinations
### length(unique(california_fired_daily$id)) ## 2415 unique fire events
sf::st_write(california_fired_daily, "data/out/fired_conus_daily_nov2001-jan2019_california.gpkg", delete_dsn = TRUE)

california_ids <- unique(california_fired_daily$id)

fired_events <- sf::read_sf("data/raw/fired_events_conus_nov2001-jan2019.gpkg")

fired_events_california <- fired_events[fired_events$id %in% california_ids, ]

fired_events_california <- sf::st_transform(fired_events_california, 3310)

sf::st_write(fired_events_california, dsn = "data/out/fired_events_conus_nov2001-jan2019_california.gpkg", delete_dsn = TRUE)
