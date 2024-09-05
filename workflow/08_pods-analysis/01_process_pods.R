googledrive::drive_auth(email = "mikoontz@gmail.com")

links = read.csv(here::here('data', 'googledrive-links.csv'))

pods_local_fname = here::here(tempdir(), 'sierra-nevada-pods.gpkg')

googledrive::drive_download(
  file = googledrive::as_id(
    links$gdrive_id[links$description == "sn_pods"]
  ),
  path = pods_local_fname
)

pods = sf::st_read(pods_local_fname) |> 
  sf::st_transform(4326) |> 
  dplyr::rename(id = POD_ID) |> 
  dplyr::mutate(
    samp_id = 0,
    date = lubridate::ymd("2020-08-01"),
    did = glue::glue("{id}-{date}")
  ) |> 
  dplyr::select(-Shape_Leng, -Shape_Area)

sf::st_write(
  obj = pods, 
  dsn = here::here("data", "out", "sierra-nevada-pods.shp"),
  append = FALSE)

sf::st_write(
  obj = sf::st_transform(pods, 3310), 
  dsn = here::here("data", "out", "sierra-nevada-pods.gpkg"),
  append = FALSE
)

