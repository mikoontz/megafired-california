googledrive::drive_auth(email = "mikoontz@gmail.com")

links = read.csv(here::here('data', 'googledrive-links.csv'))

fireshed_zip = here::here(tempdir(), 'RDS-2020-0054-2_Data.zip')
  
googledrive::drive_download(
  file = googledrive::as_id(
    links$gdrive_id[links$description == "fireshed_zip"]
  ),
  path = fireshed_zip
)

fireshed_dir = here::here(tempdir(), 'firesheds')
unzip(zipfile = fireshed_zip, exdir = fireshed_dir)

fireshed_path = here::here(fireshed_dir, 'Data', 'Firesheds_CONUS.gdb')

firesheds = sf::st_read(
  fireshed_path, layer = "Firesheds"
  ) |> 
  sf::st_transform(4326)

firesheds <-
  firesheds |> 
  dplyr::rename(area_ha = Area_HA,
                id = Fireshed_ID, 
                name = Fireshed_Name, 
                code = Fireshed_Code, 
                state = Fireshed_State,
                noPAs = NoPAs, 
                majregion = MajRegion,
                exposure = AnnualExposure,
                pct_dist = PctRecentlyDisturbed) |> 
  dplyr::select(-Shape_Length, -Shape_Area)

sf::st_write(obj = firesheds, dsn = here::here("data", "out", "firesheds_conus.shp"))
sf::st_write(obj = firesheds, dsn = here::here("data", "out", "firesheds_conus.gpkg"))
