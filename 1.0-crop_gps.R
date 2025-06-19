library(data.table)
library(gtfstools)
library(sf)
library(dplyr)
# library(mapview)
library(furrr)
library(purrr)
# remotes::install_github("kauebraga/kauetools")


# gps_path <- "data-raw/arquivo_paitt_diario_2023-03-01.csv"
# bilhetagem_path <- "data-raw/V20230329.csv"
# gtfs_path <- "data-raw/gtfs_20230519_mod.zip"
gtfs_path <- "data/gtfs_202503_mod.zip"


shapes <- gtfstools::convert_shapes_to_sf(read_gtfs(gtfs_path))


# gps_path <- "data-raw/2025/arquivo_paitt_diario_2025-05-01.csv"

integrar_gps <- function(gps_path, gtfs_path) {
  
  # ABRIR DADOS -------------------------------------------------------------
  # GPS
  gps <- fread(gps_path)
  gps[, vehicleid := as.numeric(V9)]
  gps[, hora := fasttime::fastPOSIXct(V4, tz="America/Fortaleza", fixed = 4)]
  gps <- setorder(gps, vehicleid, hora)
  gps[, id_gps := 1:.N, by = vehicleid]
  gps <- gps[, .(id_gps, linha = V6, vehicleid, hora, lon = V3, lat = V2)]
  # filter only the current day
  # gps <- gps[hora == as.POSIXct("2023-03-29")]
  
  # hist(gps$hora, breaks = "hours")
  
  # gps %>%
  #   mutate(a = data.table::hour(hora)) %>%
  #   count(a)
  # 
  # 
  # library(ggplot2)
  # gps %>%
  #   # mutate(a = data.table::hour(hora)) %>%
  #   # count(a) %>%
  #   ggplot() +
  #   geom_histogram(aes(hora))+
  #   scale_x_datetime(name="Hour", 
  #                    labels = scales::date_format("%H", tz = "America/Fortaleza"),
  #                    date_breaks = "1 hour") 
  #   # scale_x_discrete(breaks = seq(5:10))
  
  
  
  
  # quick clean gps -----------------------------------------------------------------------------
  # remove points for teh same vehicle for the same time
  gps <- gps[!(hora == lag(hora) & vehicleid == lag(vehicleid))]
  
  
  
  # abrir stops
  stops_routes <- readRDS("data/stops_gtfs_routes.rds")
  routes_limits <- readRDS("data/routes_limits.rds")
  
  
  # 4) Fazer limpeza nos dados de GPS -----------------------------------------------------------
  
  #' Foram observados diversos pontos mortos de GPS na linha, o que estava prejudicando as analises
  #' Situacoes como centanas de registros de GPS de uma linha sendo identificados em uma mesma localidade
  #' Ainda nao eh possivel saber se essas concentracoes de pontos sao uma garagem, final de linha, ou
  #' qualquer outra coisa
  #' Para contornar isso, essa etapa identifica quando essas concentracoes acontecem e diminui a quantidade
  #' de pontos das concentracoes, de forma a garantir que os pontos de GPS que estao na base sejam de
  #' quando o veiculo esteja realmente em movimento
  
  
  # remove vehiles with less than 50 observations
  gps[, n := .N, by = vehicleid]
  gps <- gps[n >= 50]
  gps <- gps[, !"n"]
  
  # ou pode-se usar essa funcao
  dtHaversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
    radians <- pi/180
    lat_to <- lat_to * radians
    lat_from <- lat_from * radians
    lon_to <- lon_to * radians
    lon_from <- lon_from * radians
    dLat <- (lat_to - lat_from)
    dLon <- (lon_to - lon_from)
    a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
    return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
  }
  
  # # 4.2) Estabelecer funcao para calcular dist entre um ponto e seu anterior
  # get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1), head(cbind(lon,lat),-1))
  
  # 4.3) Calcular essa distancia, agrupando por veiculo
  gps_join_linha_fora1 <- copy(gps)
  
  
  # gps_join_linha_fora1[, dist := c(0, get.dist(as.numeric(lon), as.numeric(lat))), by = vehicleid]
  gps_join_linha_fora1[, `:=`(Lat_to = shift(lat, type = "lag"),
                              Lon_to = shift(lon, type = "lag")), by = vehicleid]
  gps_join_linha_fora1[, dist := dtHaversine(lat, lon, Lat_to, Lon_to)]
    # group_by(vehicleid) %>%
    # mutate(dist = c(0, get.dist(as.numeric(lon), as.numeric(lat))))
  # remove columns
  gps_join_linha_fora1 <- gps_join_linha_fora1[,  !c("Lat_to", "Lon_to")]
  
  #' Aqui, vamos estabelecer a distancia de 50 metros como uma distancia limite entre um ponto e seu anterior
  #' para identificar esses pontos como uma 'aglomeracao' onde o veiculo estava parado/pouco se mexendo
  #' se a distancia for maior que 50m, sera atribuido um conjunto de letras que sao diferentes
  #' se a distancia for menor que 50m, sera atribuido o numeral 1
  #' isso eh um recurso de programacao para ajudar no agrupamento desses pontos
  gps_join_linha_fora1[, oi := ifelse(dist > 20, c("a", "b", "c"), "1")]
    # mutate(oi = ifelse(dist > 20, c("a", "b", "c"), "1"))
  
  # agrupar esses pontos
  #' a funcao 'rleid' cria numeros que vao percorrendo o data.frame e se mantem iguais quando a coluna 
  #' de referencia for igual
  #' por ex, quando a coluna de referencia estiver com os valores "a b c", a funcao vai retornar "1 2 3"
  #' porem, quando a coluna de referencia estiver com os valores "1 1 1", a funcao vai retornar "4 4 4",
  #' respeitando a sequencia que foi estabelecida
  gps_join_linha_fora1[, seq:= rleid(oi)]
  
  
  # Com as concentracoes ja identificadas, eh necessario identificar quando essas concentracoes
  # devem ser diminuidas
  # O criterio estabelecido aqui estabelece que uma concentracao com mais de 15 pontos de GPS deve ser
  # reduzida para 2 pontos, que vao representar o primeiro e o ultimo ponto da concentracao
  
  # Primeiramente, eh calculado o tamanho da concentracao (o tamanho de pontos dentro da concentracao)
  # A funcao 'add_count' adiciona uma nova coluna com a quantidade de cada 'seq'
  gps_join_linha_fora1 <- gps_join_linha_fora1 %>%
    add_count(seq)
  
  # Fazer entao o filtro para concentracoes que tenham mais de 15 pontos - essas vao ser reduzidas
  gps_join_linha_fora2 <- gps_join_linha_fora1 %>%
    # fazer o filtro de 15 pontos
    filter(n >= 10)  %>%
    # agruparar por veiculo e sequencia
    group_by(vehicleid, seq) %>%
    # a funcao 'slice' serve para extrair as observacoes por posicao - nesse caso, vamos tirar a 
    # primeira (1) e a ultima (n())
    slice(1, 2, n()-1, n()) %>%
    ungroup()
  
  # Os pontos concentrados que foram reduzidos precisam ser colados a base original
  # Para isso, precisamos primeiro manter somente os nao-concentrados da base original
  gps_join_linha_fora1_new <- gps_join_linha_fora1 %>%
    # filtrar somente os nao-concetrados, ou seja, as concentracoes com menos de 15 pontos
    filter(n < 10) %>%
    ungroup()
  
  # Em seguida, juntar os nao-concentrados com os concentrados corridigos 
  gps_clean <- gps_join_linha_fora1_new %>%
    # a funcao rbind faz essa juncao
    rbind(gps_join_linha_fora2) %>%
    # ordenar novamente por veiculo e hora
    arrange(vehicleid, hora) %>%
    select(-oi, -seq, -n)
  
  
  # get only peak time?
  gps_clean <- gps_clean %>%
    mutate(hora1 = as.ITime(hora)) %>%
    filter(hora1 %between% c(as.ITime("05:00:00"), as.ITime("12:00:00")))
  
  
  
  
  # filtro espacial ---------------------------------------------------------
  gtfs <- read_gtfs(gtfs_path)
  
  shapes <- convert_shapes_to_sf(gtfs)
  shapes <- st_make_valid(shapes)
  # identify the route from each shape
  routes <- gtfs$trips %>%
    distinct(route_id, shape_id)
  
  # abrir
  linhas <- shapes %>%
    # trazer informacao da linha
    left_join(routes, by = "shape_id") %>%
    # transformar para utm
    st_transform(31984) %>%
    # criar buffer de 400 metros
    st_buffer(250) %>%
    st_transform(4326) %>%
    group_by(route_id) %>%
    summarise(n = n(),
              do_union = TRUE) %>%
    st_make_valid() %>%
    select(-n)
  # count(route_id)
  
  
  
  
  
  # 3) Verificar quais pontos de GPS estão dentro/fora dessa linha com sf::st_join --------------
  
  #' Filtrar somente os pontos de GPS que estao FORA da linha
  #' Para fazer isso, eh preciso filtrar os pontos de GPS que nao tiveram equivalente na juncao espacial
  #' Esses pontos sao identificados quando um ponto de GPS tem um valor NA em alguma coluna que veio do shape das linhas
  
  # 3.1) Para identificar a linha que esta no GPS e a que vai vir do shape das linhas, eh preciso diferenciar
  # o nome das colunas de linha das duas bases:
  # gps <- gps %>% rename(linha_gps = linha)
  # linhas_shape_buffer <- linhas_shape_buffer %>% rename(linha_shape = linha)
  
  gps_sf <- gps_clean %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # vehicle <- unique(gps$vehicleid)[100]
  # vehicle <-  unique(gps_clean$vehicleid)[1200]
  # vehicle <-  "40976"
  # vehicle <-  "41015"
  # vehicle <-  "43993"
  # vehicle <-  "32513"
  # vehicle <-  "32675"
  # vehicle <-  "42885"
  # vehicle <-  "32996"
  # vehicle <- "43659"
  # vehicle <- vehicles[20]
  
  # run this fuction for each pt vehicle
  get_gps_line_by_vehicle <- function(vehicle) {
    
    # filter the vehicle
    gps_vehicle <- gps_sf %>% filter(vehicleid == vehicle)
    
    # 3.2) Realizar a intersecao espacial
    sf::sf_use_s2(TRUE)
    gps_vehicle1 <- st_join(gps_vehicle, linhas)
    
    # a coluna 'linha_shape' veio dos shapes das linhas, entao todas as observacoes em que ela esteja como NA
    # serao observacoes do GPS que estao FORA da linha
    gps_join_linha_fora <- gps_vehicle1 %>%
      # para filtrar os pontos de GPS que nao estao inseridos na linha, pegamos os NA da coluna 'name' 
      filter(is.na(route_id))
    
    gps_join_linha_ok <- gps_vehicle1 %>%
      # para filtrar os pontos de GPS que nao estao inseridos na linha, pegamos os NA da coluna 'name' 
      filter(!is.na(route_id))
    
    
    # qual a porcentagem dos pontos que estao dentro da linha?
    # identificar quando um ponto esta dentro/fora
    gps_probable_lines <- gps_join_linha_ok %>%
      # deletar a parte espacial do dataframe que nao vamos precisar aqui (fica bem + rapido)
      st_set_geometry(NULL) %>%
      count(route_id) %>%
      mutate(perc = n / length(unique(gps_join_linha_ok$id_gps))) %>% 
      filter(perc > 0.75)
      # slice(which.max(perc))
    
    
    
    if (nrow(gps_probable_lines) == 0) stop("not enough points identified for a bus line")
    # make sure there is not too much space inside the line that is occupied by points
    
    # crop the gps points to these lines
    # a <- "043"
    if (nrow(gps_probable_lines) > 1) {
      
      
    identify_line <- function(a) {
      
      a1 <- st_join(gps_vehicle, linhas %>% filter(route_id == a)) %>%
        filter(!is.na(route_id))
      
      # buffer points
      a2 <- a1 %>% group_by(vehicleid) %>%
        summarise(do_union = TRUE) %>%
        st_buffer(a1, dist = 100) %>%
        mutate(area_gps = st_area(.))
      
      # check intersection with line
      a3 <- linhas %>% filter(route_id == a) %>%
        mutate(area_route = st_area(.)) %>%
        st_intersection(a2) %>%
        mutate(area_intersection = st_area(.)) %>%
        mutate(perc = area_intersection / area_route) %>%
        st_set_geometry(NULL)
        
    }
    
    identify_line_ok <- lapply(gps_probable_lines$route_id, identify_line) %>% rbindlist() %>%
      slice(which.max(perc))
    
    gps_line <- st_join(gps_vehicle, linhas %>% filter(route_id %in% identify_line_ok$route_id))
    line_ok <- unique(identify_line_ok$route_id)
      
      
    } else if (nrow(gps_probable_lines) == 1) {
      
      
      gps_line <- st_join(gps_vehicle, linhas %>% filter(route_id %in% gps_probable_lines$route_id))
      line_ok <- unique(gps_probable_lines$route_id)
      
    }
    
    
    # # visualizar esses pontos
    # mapview(gps_join_linha_ok) + mapview(linhas)
    # mapview(gps_line) + linhas %>% filter(route_id == line_ok)
    # mapview(gps_join_linha_ok) + mapview(linhas %>% filter(route_id == gps_probable_lines$route_id))
    
    # delete NA's points that are either on the beggining or end of bus service
    gps_line1 <-  gps_line %>% mutate(a = rleid(route_id)) %>%
      # delete the first and last sequence
      # mutate(max = max(a))
      # filter(!(a == 1 | a == max(.$a))) %>%
      # fill the values in between - they may deviations from the route
      mutate(route_status = ifelse(is.na(route_id), "out_route", "in_route")) %>%
      tidyr::fill(route_id) 
    
    
    # identify periods where the vehicle scapes from the route, but returns after --------
    
    gps_line1_crop <- gps_line1 %>%
      st_set_geometry(NULL) %>%
      # identify each block size
      group_by(a) %>%
      summarise(n = n(),
                route_status = first(route_status)) %>%
      ungroup() %>%
      # identify the problematics that need fixing
      mutate(route_status_new = ifelse(route_status %in% c("out_route") & n <= 10 & lag(route_status) %in% "in_route" & lag(n) >= 20 & lead(route_status) %in% "in_route" & lead(n) >= 20, 
                                       "in_route", route_status)) %>%
      select(-n, -route_status)
    
    # bring the nex status to fix
    gps_line1 <- gps_line1 %>%
      left_join(gps_line1_crop, by = "a") %>%
      select(-route_status) %>%
      rename(route_status = route_status_new) %>%
      mutate(a = rleid(route_status))
    
    # identify trip beggining and end -------------------------------------------------------------
    
    # open
    stop_routes_ok_ends <-routes_limits %>% filter(route_id == line_ok) %>%
      select(stop_id)
    
    # join
    gps_line1 <- gps_line1 %>%
      st_join(stop_routes_ok_ends) %>%
      mutate(route_status = ifelse(!is.na(stop_id), "start_end", route_status))
    
    # mapview(gps_line1[140:156,], zcol = "id_gps") + stop_routes_ok_ends + linhas %>% filter(route_id %in% line_ok)

    
    
    # crop the period where vehicles are inside of the route, but still have not started the trips --------
    # https://stackoverflow.com/questions/55626654/filter-to-remove-all-rows-before-the-first-time-a-particular-value-in-a-specific
    gps_line2 <- gps_line1 %>%
      # slice between the first and the last ocurrence of "start_end"
      slice(min(which(route_status == "start_end")): max(which(route_status == "start_end"))) %>%
      mutate(b = rleid(route_status))
    
    
    
    # identify and fix the periods where the vehicle scapes from the start/end of trip zone, but still are at the start/end
    gps_line1_test <- gps_line2 %>%
      st_set_geometry(NULL) %>%
      # identify each block size
      group_by(b) %>%
      summarise(n = n(),
                route_status = first(route_status)) %>%
      ungroup() %>%
      # identify the problematics that need fixing
      mutate(route_status_new = ifelse(route_status %in% c("in_route") & n <= 20 & lag(route_status) %in% "start_end", "start_end", route_status)) %>%
      select(-n, -route_status)
    
    # bring the nex status to fix
    gps_line2 <- gps_line2 %>%
      left_join(gps_line1_test, by = "b") %>%
      select(-route_status) %>%
      rename(route_status = route_status_new) %>%
      mutate(b = rleid(route_status))
    
    
    
    
    
    # identify the trip direction -----------------------------------------------------------------
    sfc_as_cols <- function(x, names = c("lon","lat")) {
      stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
      ret <- sf::st_coordinates(x)
      ret <- tibble::as_tibble(ret)
      stopifnot(length(names) == ncol(ret))
      x <- x[ , !names(x) %in% names]
      ret <- setNames(ret,names)
      ui <- dplyr::bind_cols(x,ret)
      st_set_geometry(ui, NULL)
    }
    
    gps_line3 <- gps_line2 %>%
      sfc_as_cols() %>%
      select(-stop_id)
    
    
    stop_routes_ok <- stops_routes %>% filter(route_id == line_ok) %>%
      group_by(shape_id) %>%
      mutate(a = group_indices()) %>%
      # filtrar somente ida
      filter(a == 1) %>%
      ungroup() %>%
      select(shape_id, stop_id, stop_sequence, stop_lon, stop_lat)
    
    
    opa <- RANN::nn2(select(stop_routes_ok,  lon = stop_lon, lat = stop_lat), select(gps_line3, lon, lat), 1)
    
    setDT(gps_line3)
    vamos <- gps_line3[, ':='(stop_sequence = opa$nn.idx, dist = opa$nn.dists*111320)]
    # Trazer o stop_id, lon e lat de cada parada
    vamos <- merge(vamos, stop_routes_ok, by = "stop_sequence", suffixes = c(".gps", ".parada"), sort = FALSE)
    # Tirar o stop_sequence_id
    # vamos <- vamos[, !c("stop_sequence")]
    # vamos[, id_gps_temp := 1:nrow(vamos)]
    vamos <- vamos[order(hora)]
    
    # identify consistently either stop_sequence is increasing or is decreasing
    vamos_teste <- vamos %>%
      filter(route_status != "start_end") %>%
      group_by(b) %>%
      mutate(dif = stop_sequence - lag(stop_sequence)) %>%
      # remove 0 - where there was not different in the stop snap
      filter(dif != 0) %>%
      summarise(dif_mean = median(dif, na.rm = TRUE)) %>%
      mutate(direction_id = ifelse(dif_mean > 0, 0, 1)) %>%
      select(b, direction_id)
    
    vamos_v1 <- vamos %>%
      left_join(vamos_teste, by = "b")
    
    
    
    # define the trips ----------------------------------------------------------------------------
    
    # the trip starts/end at the first point the vehicle enters/exists the route limit
    vamos_trips <- vamos_v1 %>%
      filter(route_status != "start_end") %>%
      mutate(trip = rleid(b)) %>%
      # calcuate the cumulative distance in the trip
      mutate(dist_acc = ifelse(row_number() == 1, 0, dist)) %>%
      # select columns
      select(vehicleid, hora, route_id, direction_id, trip, dist_acc, lon, lat)
    
    
    # define the shape id -------------------------------------------------------------------------
    
    trips <- gtfs$trips    
    routes_shapes <- trips %>%
      mutate(direction_id = ifelse(grepl(pattern = "I$", x = shape_id), 0, 1)) %>%
      distinct(route_id, shape_id, direction_id)
    
    vamos_trips <- vamos_trips %>%
      left_join(routes_shapes, by = c("route_id", "direction_id"))
    
    # print(sprintf("finishd vehicle %s", vehicle))
    
    return(vamos_trips)
    
    
  }
  
  vehicles <- unique(gps_clean$vehicleid)
  # gtfs_lines_all <- lapply(vehicles, possibly(get_gps_line_by_vehicle))

  gtfs_lines_all <- furrr::future_map(vehicles, possibly(get_gps_line_by_vehicle))
  # filter onlyt those that are ok
  gtfs_lines_all_ok <- gtfs_lines_all[!purrr::map_lgl(gtfs_lines_all, is.null)]
  gtfs_lines_all_ok <- rbindlist(gtfs_lines_all_ok)
  
  
  day <- stringr::str_extract(gps_path, "\\d{4}-\\d{2}-\\d{2}")
  readr::write_rds(gtfs_lines_all_ok, sprintf("data/gps_clean/gps_clean_%s.rds", day))
  
  
  
}


plan(multisession, workers = 8)
options(future.globals.maxSize= 1291289600)

# APLICAR -----------------------------------------------------------------


# dezembro/2024 ---------------------------------------------------------------------------------------
integrar_gps(gps_path = "data-raw/arquivo_paitt_diario_2023-04-14.csv",
             gtfs_path = "data-raw/gtfs_20230519_mod.zip")
