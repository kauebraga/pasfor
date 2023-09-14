library(data.table)
library(gtfstools)
library(sf)
library(dplyr)
library(mapview)
library(furrr)
library(purrr)
library(Hmisc)
library(lubridate)


shapes <- gtfstools::convert_shapes_to_sf(read_gtfs("data-raw/gtfs_20230519_mod.zip"))
distance_stops <- readRDS("data/distance_stops.rds")

# gps_path <- "data/gps_clean/gps_clean_2023-03-01.rds"
# gps_path <- "data/gps_clean/gps_clean_2023-03-02.rds"
# gps_path <- "data/gps_clean/gps_clean_2023-03-03.rds"

snap_trips <- function(gps_path) {
  
  # open gps
  gps <- readRDS(gps_path)
  gps <- gps %>% 
    mutate(trip_id = paste0(vehicleid, "-", trip))
  
  # gps %>%
  #   mutate(a = data.table::hour(hora)) %>%
  #   count(a)
  
  
  shapes_linha <- shapes %>% filter(shape_id %in% gps$shape_id)
  
  # snap by trip
  
  # trip1 <- "43993-1"
  # trip1 <- trips_all[683]
  # trip1 <- "32996-1"
  # trip1 <- "32719-1"
  # trip1 <- "34276-2"
  # trip1 <- "57847-6"
  
  # calculate the network distance on the route for each trip -----------------------------------
  snap_by_trip <- function(trip1) {
    
    
    gps_viagem <- gps %>% 
      filter(trip_id == trip1)
    
    gps_viagem_sp <- gps_viagem %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove =F) %>%
      st_transform(3857) %>%
      as_Spatial()
    
    linha_escolhida <- shapes %>%
      filter(shape_id == unique(gps_viagem$shape_id)) %>%
      st_transform(3857) %>%
      as_Spatial()
    
    # mapview(linha_escolhida) + gps_viagem_sp
    
    # Aplicar geosphere
    fim2 <- rgeos::gProject(linha_escolhida, gps_viagem_sp)
    
    gps_viagem_list <- gps_viagem %>% mutate(dist = fim2) %>%
      # arredondar para o metro
      mutate(dist = round(dist, 0)) %>%
      # checar se as distancias acumuladas estao aumentando mesmo!
      mutate(dif_dist = dist - lag(dist)) %>%
      mutate(dif_time = difftime(hora, lag(hora), units = "secs")) %>%
      mutate(dif_time = as.numeric(dif_time)) %>%
      mutate(vel = (dif_dist / dif_time) * 3.6) %>%
      mutate(vel = ifelse(is.nan(vel), 0, vel)) %>%
      # # excluir velocidades maiores que 70km/h e menores que -10km/h, mantendo sempre as duas primeiras
      # # observacoes
      filter(!(vel < 0 & row_number() %nin% c(1, n()))) %>%
      filter(!(vel >  50 & row_number() %nin% c(1, n())))
    
    distancia_paradas_teste <- distance_stops %>% filter(shape_id == unique(gps_viagem$shape_id))
    
    
    # Se a distancia ate a ultima parada for menor que a distancia ate a penultima, excluir ultima obs
    if (gps_viagem_list$dist[nrow(gps_viagem_list)] < gps_viagem_list$dist[nrow(gps_viagem_list) - 1]) {
      
      x <- gps_viagem_list$hora[-nrow(gps_viagem_list)]
      y <- gps_viagem_list$dist[-nrow(gps_viagem_list)]
      
    } else {
      
      x <- gps_viagem_list$hora
      y <- gps_viagem_list$dist
      
    }
    
    # Pegar a distancia sem a primeira e ultima
    # y <- vai$dist[-c(1, nrow(vai))]
    # Pegar as paradas sem a primeira e ultima (xout representa as distancias em que eu quero estimar
    # a hora, que no caso sao as paradas)
    xout <- distancia_paradas_teste$dist_acc[-c(1, nrow(distancia_paradas_teste))]
    # xout <- distancia_paradas_teste$dist
    
    
    
    # # tirar outliers
    # a <- cooks.distance(lm(as.integer(x, origin = "1970-01-01") ~y))
    # mean_out <- 4*mean(a)
    # pontos_fora <- which(a > mean_out)
    # # tirar
    # if (length(pontos_fora) == 0) {
    #   
    #   x_novo <- x
    #   y_novo <- y
    #   
    # } else {
    #   x_novo <- x[-pontos_fora]
    #   y_novo <- y[-pontos_fora]
    # }
    
    x_novo <- x
    y_novo <- y
    
    # plot(x = y_novo, y = x_novo)
    
    uhlala <- as.POSIXct(approx(x = y_novo, y = x_novo, xout = xout, ties = mean, rule = 2)$y, 
                         origin = "1970-01-01", tz = "UTC")
    
    
    fim_ne <- distancia_paradas_teste %>%
      mutate(hora = c(gps_viagem_list$hora[1], uhlala, gps_viagem_list$hora[nrow(gps_viagem_list)])) %>%
      mutate(viagem = trip1,
             vehicleid = unique(gps_viagem$vehicleid)) %>%
      mutate(hora1 = as.ITime(hora),
             dia = as.Date(hora)) %>%
      select(-hora) %>%
      select(route_id, shape_id, vehicleid, viagem, stop_id, stop_sequence, dia, hora = hora1, dist_acc)
    
    
    return(gps_viagem_list)
    
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


ui1 <- walk(c("data/gps_clean/gps_clean_2023-03-01.rds"),
             snap_trips)
ui2 <- walk(c("data/gps_clean/gps_clean_2023-03-02.rds"),
             snap_trips)
ui3 <- walk(c("data/gps_clean/gps_clean_2023-03-03.rds"),
             snap_trips)
walk(c("data/gps_clean/gps_clean_2023-03-06.rds"),
     snap_trips)
walk(c("data/gps_clean/gps_clean_2023-03-08.rds"),
     snap_trips)
walk(c("data/gps_clean/gps_clean_2023-03-09.rds"),
     snap_trips)



