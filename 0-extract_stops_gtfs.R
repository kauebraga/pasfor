library(kauetools)
library(sf)
library(gtfstools)

# open
gtfs <- read_gtfs("data-raw/gtfs_20230519_mod.zip")
gtfs$calendar



stops <- extract_scheduled_stops(gtfs, service_id = "U")

# save
readr::write_rds(stops, "data/stops_gtfs_routes.rds")
