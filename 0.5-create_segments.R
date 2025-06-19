library(data.table)
library(gtfstools)
library(sf)
library(dplyr)
library(mapview)
library(furrr)
library(purrr)
library(Hmisc)
library(stringr)


gtfs <- read_gtfs("data/gtfs_202503_mod.zip")

# ABRIR SHAPES ------------------------------------------------------------

shapes <- gtfs$shapes %>%
  mutate(route_id = str_extract(shape_id, "\\d{4}")) %>% 
  mutate(direction_id = str_sub(shape_id, -1, -1)) %>%
  mutate(direction_id = ifelse(direction_id == "I", 0 ,1))

# Isolar linhas dos shapes para comparacao
linhas_shapes <- unique(shapes$shape_id)



# ABRIR LINHAS_PARADAS ----------------------------------------------------

stops_linhas <- readRDS("data/stops_gtfs_routes.rds")

# Agora, filtra somente as linhas dos stops nos shapes
linhas_stops <- unique(stops_linhas$shape_id)

shapes <- shapes %>% filter(shape_id %in% linhas_stops)


# CONFERIR SE AS LINHAS DAS DUAS TABELAS BATEM ----------------------------
a <- unique(shapes$linha_sentido)
b <- unique(stops_linhas$linha_sentido)

identical(a, b)

which(a != b)



# CRIAR FUNCAO ------------------------------------------------------------

# shape <- "shape075-I"

# Em qual ponto do arquivo shapes encontra-se a parada daquela linha e sentido?
shapes_stops_dists <- function(shape) {
  
  stops_linhas_df <- stops_linhas %>% 
    filter(shape_id %in% shape) %>%
    rename(lon = stop_lon, lat = stop_lat)
  shapes_df <- shapes %>% filter(shape_id %in% shape) %>%
    rename(lon = shape_pt_lon, lat = shape_pt_lat)
  
  
  uui <- RANN::nn2(select(stops_linhas_df, lon, lat), select(shapes_df, lon,lat), 1)
  
  # pegar o shape_pt_sequence que refere a qual parada
  vamos <- shapes_df %>% 
    mutate(stop_sequence = uui$nn.idx[,1], dist = uui$nn.dists*111320) %>%
    group_by(stop_sequence) %>%
    slice(which.min(dist)) %>%
    ungroup() %>%
    select(shape_pt_sequence, stop_sequence)
  
  # jogar no arquivo de shapes
  shapes_df_v1 <- shapes_df %>%
    left_join(vamos, by = "shape_pt_sequence") %>%
    mutate(stop_sequence = as.numeric(stop_sequence)) %>%
    tidyr::fill(stop_sequence)
  # mutate(stop_sequence = ifelse(is.na(stop_sequence), 0, stop_sequence)) %>%
  # mutate(trecho = rep(1:length(rle(stop_sequence)$lengths), times = rle(stop_sequence)$lengths))
  
  # criar data.frame so com as paradas
  shape_df_colar <- shapes_df_v1 %>%
    group_by(shape_id, stop_sequence) %>%
    slice(1) %>%
    ungroup() %>%
    slice(-1) %>%
    mutate(stop_sequence = stop_sequence - 1)
  
  
  to_spatial <- function(df1, coordenada = c("lon", "lat")) {
    x <- st_as_sf(df1, coords = coordenada, crs = 4326)
  }
  
  # Colar
  shapes_df_v2 <- shapes_df_v1 %>%
    rbind(shape_df_colar) %>%
    arrange(shape_pt_sequence, stop_sequence) %>%
    to_spatial() %>%
    group_by(shape_id, stop_sequence) %>%
    summarise(n = n(), do_union=FALSE) %>%
    st_cast("LINESTRING") %>%
    ungroup() %>%
    mutate(stop_sequence_inicio = stop_sequence, stop_sequence_fim = stop_sequence + 1) %>%
    mutate(length = st_length(.)) %>%
    rename(trecho = stop_sequence) %>%
    left_join(select(stops_linhas_df, stop_id, stop_sequence), by = c("stop_sequence_inicio" = "stop_sequence")) %>%
    left_join(select(stops_linhas_df, stop_id, stop_sequence), by = c("stop_sequence_fim" = "stop_sequence")) %>%
    rename(stop_id_inicio = stop_id.x, stop_id_fim = stop_id.y) %>%
    select(-n) %>%
    # Tirar a ultima coluna
    slice(-n())
  
  
}


# APLICAR FUNCAO ----------------------------------------------------------

shapes_all <- unique(shapes$shape_id)
trechos <- lapply(shapes_all, shapes_stops_dists)
trechos <- rbindlist(trechos) 
trechos <- st_sf(trechos, crs = 4326)
trechos <- trechos %>% mutate(segment_id = paste0(stop_id_inicio, "-", stop_id_fim))

# save
readr::write_rds(trechos, "data/segments_sf.rds")
