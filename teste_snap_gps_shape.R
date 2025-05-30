library(data.table)
library(gtfstools)
library(sf)
library(dplyr)
library(mapview)
library(furrr)
library(purrr)
library(Hmisc)
library(lubridate)



gtfs <- read_gtfs("data-raw/gtfs_20230519_mod.zip")
gtfs$trips <- gtfs$trips %>% mutate(direction_id = ifelse(grepl(pattern = "I$", x = shape_id), 0, 1))


shapes <- gtfstools::convert_shapes_to_sf(read_gtfs("data-raw/gtfs_20230519_mod.zip"))
distance_stops <- readRDS("data/distance_stops.rds")

# gps_path <- "data/gps_clean/gps_clean_2023-03-01.rds"

snap_trips <- function(gps_path) {

# open gps
gps <- readRDS(gps_path)
gps <- gps %>% 
  mutate(trip_id = paste0(vehicleid, "-", trip))

# shape_id1 <- shapes_linhas$shape_id[20]
# trip1 <- "57847-6"

snap_by_trip <- function(trip1) {
  
  # filter shape_id
  stop_linhas_filter   <-   gps %>% 
    filter(trip_id == trip1) %>%
    mutate(stop_sequence = 1:n())
  
  shapes_linhas_filter <- shapes %>%
    filter(shape_id == unique(stop_linhas_filter$shape_id))
  # standardize shape resolution - at least every 50 meters
  shapes_linhas_filter <- st_segmentize(shapes_linhas_filter, 50)
  # shapes_linhas_filter <- sfheaders::sf_cast(shapes_linhas_filter, "POINT")
  # transform to lon lat
  shapes_linhas_filter <- sfheaders::sf_to_df(shapes_linhas_filter) %>% setDT()
  # rename
  shapes_linhas_filter <- shapes_linhas_filter[, .(shape_pt_lon = x, shape_pt_lat = y)]
  
  # create shape_pt_sequence
  shapes_linhas_filter[, shape_pt_sequence := 1:.N]
  # calculate shape dist traveled
  get.dist <- function(lon, lat) geosphere::distGeo(tail(cbind(lon,lat),-1),head(cbind(lon,lat),-1))
  shapes_linhas_filter[, shape_dist_traveled := c(0,cumsum(get.dist(shape_pt_lon,shape_pt_lat)))]
  
  
  # mapview(shapes_linhas_filter) + st_as_sf(stop_linhas_filter, coords = c("lon", "lat"), crs = 4326)
  
  
  # # Separar a primeira e ultima parada
  # stops_linhas_ok <- stops_linhas_df %>%
  #   # slice(-1, -n()) %>%
  #   slice(-1) %>%
  #   mutate(stop_sequence_id = 1:n())
  #
  # # stops_linhas_ultimas <- slice(stops_linhas_df, 1, n()) %>% mutate(dist = 0) %>% select(-stop_name)
  # stops_linhas_ultimas <- slice(stops_linhas_df, 1) %>% mutate(dist = 0) %>% select(-stop_name)
  
  uui <- RANN::nn2(shapes_linhas_filter[, .(shape_pt_lon, shape_pt_lat)], stop_linhas_filter[,. (lon, lat)], 25)
  # uui <- RANN::nn2(shapes_linhas_filter[, .(shape_pt_lon, shape_pt_lat)], stop_linhas_filter[,. (stop_lon, stop_lat)], searchtype = "radius", radius = 0.004491556)
  uui_df <- as.data.frame( uui$nn.idx)
  # uui_df <- cbind(as.data.frame( uui$nn.idx), as.data.frame(uui$nn.dists * 111320))
  
  # create tidy df with points an distances
  
  uui_df_tidy <- as.data.table(uui$nn.idx)
  uui_df_tidy[, stop_sequence := 1:.N]
  colnames(uui_df_tidy) <- c(paste0(rep("option_", 25), c(1:25)), "stop_sequence")
  uui_df_tidy <- tidyr::pivot_longer(uui_df_tidy, cols = option_1:option_25, names_to = "option", values_to = "shape_pt_sequence") %>% setDT()
  
  uui_df_tidy2 <- as.data.table(uui$nn.dists * 111320)
  uui_df_tidy2 <- tidyr::pivot_longer(uui_df_tidy2, cols = V1:V25, names_to = "option", values_to = "dist_point") %>% setDT()
  
  uui_df_tidy[, dist_from_shape := uui_df_tidy2$dist_point]
  
  
  
  
  # teste
  # uui_df[11,10] <- 900
  
  
  # make sure that the first and the last stops are located in the beginning
  # of the shape
  # in this case, in the first points of the shape that sum a max of 200m of distance
  # if (stop_linhas_filter[[1, 4]] == 1) {
  
  # shape_start_point <- max(shapes_linhas_filter[shape_dist_traveled < 500]$shape_pt_sequence)
  first_stop_row <- uui_df[1, ][uui_df[1,] %between% c(1, 50)]
  last_stop_row <- uui_df[nrow(uui_df), ]
  last_stop_row <- last_stop_row[uui_df[nrow(uui_df), ] %between% c(max(last_stop_row)-15, max(last_stop_row))]
  uui_df$ordered[1] <- first(first_stop_row)
  uui_df$ordered[nrow(uui_df)] <- first(last_stop_row)
  
  # a <- shapes_linhas_filter %>% st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
  # b <- stop_linhas_filter %>% st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
  # mapview::mapview(a[1:100,], zcol = "shape_pt_sequence") + mapview::mapview(b)
  
  # } else uui_df$ordered[1] <- uui_df[1,1]
  
  # search_results[nrow(search_results),c("shape_pt_sequence")] <-
  #   ifelse(search_results[nrow(search_results),c("shape_pt_sequence")] %between% c(nrow(shapes_df_sf_break) - 20, nrow(shapes_df_sf_break)),
  #          search_results[nrow(search_results),c("shape_pt_sequence")], nrow(shapes_df_sf_break))
  
  # i <- 2
  # i <- 10
  # i <- 11
  # i <- 17
  # i <- 69
  # i <- 38
  # i <- 40
  # i <- 41
  # i <- 43
  
  # iterate from the stop_sequence 2 and after, because we already locked to stop_sequence 1
  for (i in 2:(nrow(uui_df)-1)) {
    
    # extract the values related to the stop_sequence in question
    x <- uui_df[i, 1:25]
    
    if (is.na(uui_df$ordered[i-1])) {
      
      # get first non NA
      target <- last(uui_df$ordered[1:i-1][!is.na(uui_df$ordered[1:i-1])])
      
      # the target will be the snapped point from the previous interation
    } else {target <- uui_df$ordered[i-1]}
    
    # make sure that all possible snap values are after the previous snapped point
    # this ensure that we will not have negative distances - the most problematic situations
    x <- x[x > target]
    
    # sanity check:
    if (length(x) == 0) {
      
      go <- NA
      
      # make sure that the closest point is not too far away from the previous point.
      # one situation that can happen is when the snapped point is after the previous snap
      # (it's ok), but it was snapped far down the road, and not very close to the previous point
    } else if (x[1] - target <= 20) {
      
      go <- x[1]
      
      # if the above situation occur, we will atribute 'weights' to each point, based on its difference to the previous snapped
      # so, if the closest point is 400 (wrong snap) and the second is 20 (correct snap),
      # the frist point will be assigned a new value of (400 - preivous_snapped (2)) * 1 = 398
      # the second point will be assigned a new value of (20 - preivous_snapped(2)) * 2 = 38
      # and we select the minimium of these weighted values, so the second points would be choosen
      # because 38 < 398
    } else {
      
      go <- x[which.min((x - target) * seq(1, by = 1, length.out = length(x)))] %>% as.numeric()
    }
    
    
    if(length(go) == 0) go <- NA
    
    uui_df$ordered[i] <- go
    
  }
  
  search_results <- setDT(uui_df)
  uui_df[, stop_sequence := 1:.N]
  # rename columns
  uui_df <- uui_df[, .(shape_pt_sequence = ordered, stop_sequence)]
  uui_df[uui_df_tidy, on = c("shape_pt_sequence", "stop_sequence"),
         c("dist_from_shape") := list(i.dist_from_shape)]
  
  
  
  # uui_df_tidy_ai <- uui_df_tidy %>%
  #   left_join(search_results %>% select(shape_pt_sequence, stop_sequence), by = "stop_sequence")
  # # group_by(stop_sequence) %>%
  # # filter(between(shape_pt_sequence, ordered - 10, ordered + 10))
  
  
  # jogar no arquivo de shapes
  shapes_linhas_filter[uui_df, on = "shape_pt_sequence",
                       c("stop_sequence", "dist_from_shape") := list(i.stop_sequence, i.dist_from_shape)]
  shapes_linhas_filter[, stop_sequence := as.numeric(stop_sequence)]
  
  # keep only the shapes points where there was the best match between shape and stop
  shapes_linhas_filter <- shapes_linhas_filter[!is.na(stop_sequence)]
  # discount the distance from the first point
  shapes_linhas_filter[, shape_dist_traveled := shape_dist_traveled - shape_dist_traveled[1]]
  # select vars
  shapes_linhas_filter <- shapes_linhas_filter[, .(stop_sequence, shape_dist_traveled, dist_from_shape)]
  
  # bring the results (cumulative distance) to the scheduled service
  stop_linhas_filter[shapes_linhas_filter, on = "stop_sequence",
                     c("dist_acc", "dist_error") := list(i.shape_dist_traveled, i.dist_from_shape)]
  
  # save
  # readr::write_rds(stop_linhas_filter, sprintf("teste/data/snap_shape_stop/snap_shape_stop_%s.rds", shape_id1))
  distancia_paradas_teste <- distance_stops %>% filter(shape_id == unique(stop_linhas_filter$shape_id))
  
  
  # Se a distancia ate a ultima parada for menor que a distancia ate a penultima, excluir ultima obs
  if (stop_linhas_filter$dist_acc[nrow(distancia_paradas_teste)] < stop_linhas_filter$dist_acc[nrow(stop_linhas_filter) - 1]) {
    
    x <- stop_linhas_filter$hora[-nrow(stop_linhas_filter)]
    y <- stop_linhas_filter$dist_acc[-nrow(stop_linhas_filter)]
    
  } else {
    
    x <- stop_linhas_filter$hora
    y <- stop_linhas_filter$dist_acc
    
  }
  
  # Pegar a distancia sem a primeira e ultima
  # y <- vai$dist[-c(1, nrow(vai))]
  # Pegar as paradas sem a primeira e ultima (xout representa as distancias em que eu quero estimar
  # a hora, que no caso sao as paradas)
  xout <- distancia_paradas_teste$dist_acc[-c(1, nrow(distancia_paradas_teste))]
  # xout <- distancia_paradas_teste$dist
  
  x_novo <- x
  y_novo <- y
  
  # plot(x = y_novo, y = x_novo)
  
  uhlala <- as.POSIXct(approx(x = y_novo, y = x_novo, xout = xout, ties = mean, rule = 2)$y, 
                       origin = "1970-01-01", tz = "UTC")
  
  
  fim_ne <- distancia_paradas_teste %>%
    mutate(hora = c(stop_linhas_filter$hora[1], uhlala, stop_linhas_filter$hora[nrow(stop_linhas_filter)])) %>%
    mutate(viagem = trip1,
           vehicleid = unique(stop_linhas_filter$vehicleid)) %>%
    mutate(hora1 = as.ITime(hora),
           dia = as.Date(hora)) %>%
    select(-hora) %>%
    select(route_id, shape_id, vehicleid, viagem, stop_id, stop_sequence, dia, hora = hora1, dist_acc)
  
  
  return(fim_ne)
  
  
  
}

# apply to every trip
trips_all <- unique(gps$trip_id)

plan(multisession, workers = 12)
options(future.globals.maxSize= 1291289600)
trips_stops_all <- furrr::future_map(trips_all, possibly(snap_by_trip))
# filter onlyt those that are ok
trips_stops_all_ok <- trips_stops_all[!purrr::map_lgl(trips_stops_all, is.null)]
trips_stops_all_ok <- rbindlist(trips_stops_all_ok)
# create interval
# trips_stops_all_ok[, interval := as.ITime(lubridate::round_date(hms(hora), "15 mins"))]
# trips_stops_all_ok %>%
#   mutate(a = data.table::hour(hora)) %>%
#   count(a)


day <- stringr::str_extract(gps_path, "\\d{4}-\\d{2}-\\d{2}")
readr::write_rds(trips_stops_all_ok, sprintf("data/gps_by_stop/gps_by_stop_%s.rds", day))


}