library(kauetools)
library(sf)
library(gtfstools)

# open
# gtfs <- read_gtfs("data-raw/gtfs_20230519_mod.zip", encoding = "Latin-1")
gtfs <- read_gtfs("data-raw/2025/gtfs_202503.zip")
gtfs$calendar

gtfs$stops$stop_name


stops <- extract_scheduled_stops(gtfs, service_id = "U")

# save
readr::write_rds(stops, "data/stops_gtfs_routes.rds")
