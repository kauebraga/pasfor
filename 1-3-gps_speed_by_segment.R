library(data.table)
library(gtfstools)
library(sf)
library(dplyr)
library(mapview)
library(furrr)
library(purrr)
library(Hmisc)
library(lubridate)

# open data
files <- dir("data/gps_by_stop", full.names = TRUE, pattern = "gps_by_stop")
ui <- lapply(files, readRDS) %>% rbindlist()

# remove first and last segments of each trip
ui <- ui %>% group_by(viagem) %>% slice(-1, -n()) %>% setDT()

stop_time_segments <- ui[, ':='(stop_sequence_depois = shift(stop_sequence, type = "lead"),
                                stop_id_depois = shift(stop_id, type = "lead"),
                                hora_depois = shift(hora, type = "lead"),
                                dist_depois = shift(dist_acc, type = "lead")
),
by = .(route_id, shape_id, vehicleid, viagem, dia)]
stop_time_segments <- stop_time_segments[, .SD[-.N], by = .(route_id, shape_id, vehicleid, viagem, dia)]
stop_time_segments[, ':='(segment_id = paste0(stop_id, "-", stop_id_depois),
                          travel_time = hora - hora_depois,
                          dist = dist_acc - dist_depois,
                          hora_depois = hora_depois)]

# filter between 7h 8h
stop_time_segments <- stop_time_segments[hora_depois %between% c(as.ITime("07:00:00"), as.ITime("08:00:00"))]

library(ggplot2)
stop_time_segments %>%
  mutate(a = data.table::hour(hora)) %>%
  count(a)


# stop_linhas_segments_distinct <- unique(stop_time_segments, by = c("segment_id"))
stop_time_segments <- stop_time_segments[, .(route_id,  shape_id, vehicleid, viagem, dia, segment_id,travel_time, dist, stop_id_start = stop_id, stop_id_end = stop_id_depois)]

# group by segment id
trips_by_segments <- stop_time_segments %>%
  mutate(velocidade = (dist / 1000)/as.numeric(travel_time/3600)) %>%
  filter(!is.infinite(velocidade)) %>%
  filter(!is.na(velocidade)) %>%
  filter(!(velocidade <= 0 | velocidade > 70 )) %>%
  # only between 7h - 8h
  group_by(segment_id) %>%
  summarise(velocidade = median(velocidade, na.rm = TRUE),
            n = n())

speed_total <- trips_by_segments %>%
  mutate(city = "Fortaleza") %>%
  group_by(city) %>%
  summarise(velocidade = weighted.mean(velocidade, n))

# get segments sf
gtfs <- read_gtfs("data-raw/gtfs_20230519_mod.zip")
segments_gtfs <- setDT(readRDS("data/segments_sf.rds"))
segments_gtfs <- distinct(segments_gtfs, segment_id, .keep_all = TRUE)
segments_gtfs <- segments_gtfs %>% select(segment_id, geometry) %>% st_sf(crs = 4326)

segments_gtfs_speeds <- segments_gtfs %>%
  left_join(trips_by_segments, by = "segment_id") %>%
  filter(!is.na(velocidade)) %>%
  mutate(velocidade = ifelse(velocidade >= 40, 40, velocidade))


ui <- segments_gtfs_speeds %>% arrange(desc(n))

mapview(ui %>% slice(1:50), zcol = "velocidade")
mapview(segments_gtfs_speeds %>% filter(velocidade < 15), zcol = "velocidade")

mapshot(mapview(segments_gtfs_speeds, zcol = "velocidade"),
        url = "gps_velocidades_trechos.html", selfcontained = TRUE)

mapshot(mapview(segments_gtfs_speeds %>% filter(velocidade < 15), zcol = "velocidade"),
        url = "gps_velocidades_trechos_less15.html", selfcontained = TRUE)
