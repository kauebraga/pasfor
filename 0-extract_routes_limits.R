library(data.table)
library(gtfstools)
library(sf)
library(dplyr)
library(mapview)


# open terminais
terminais <- readRDS("data/terminais.rds")

# open stops
stops_routes <- readRDS("data/stops_gtfs_routes.rds") %>%
  # get the first and last stops
  group_by(shape_id) %>%
  filter(stop_sequence == 1 | stop_sequence == n()) %>%
  # to sf
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
# # transformar para utm
# st_transform(31984) %>%
# # criar buffer de 400 metros
# st_buffer(50) %>%
# st_transform(4326) %>%
# group_by(route_id) %>%
# summarise(n = n(),
#           stop_id = paste0(stop_id, collapse = ","),
#           do_union = TRUE) %>%
# select(stop_id)

library(Hmisc)
stops_routes_nterminais <- stops_routes %>%
  mutate(stop_name1 = janitor::make_clean_names(stop_name)) %>%
  filter(stop_name1 %nin% c("pra_a_terminal_antonio_bezerra_sn",
                           "pra_a_terminal_messejana_sn",
                           "pra_a_terminal_parangaba_sn",
                           "pra_a_terminal_lagoa_sn",
                           "pra_a_terminal_siqueira_sn",
                           "pra_a_terminal_papicu_sn",
                           "pra_a_terminal_conjunto_ceara_sn")) %>%
  # transformar para utm
  st_transform(31984) %>%
  # criar buffer de 400 metros
  st_buffer(100) %>%
  st_transform(4326)
  
  

stops_routes_terminais <- stops_routes %>%
  st_set_geometry(NULL) %>%
  # Ajeitar o nome dos terminais
  mutate(stop_name1 = janitor::make_clean_names(stop_name)) %>%
  filter(stop_name1 %in% c("pra_a_terminal_antonio_bezerra_sn",
                          "pra_a_terminal_messejana_sn",
                          "pra_a_terminal_parangaba_sn",
                          "pra_a_terminal_lagoa_sn",
                          "pra_a_terminal_siqueira_sn",
                          "pra_a_terminal_papicu_sn",
                          "pra_a_terminal_conjunto_ceara_sn")) %>%
# Colocar no formato comum
mutate(stop_name1 = case_when(stop_name1 == "pra_a_terminal_antonio_bezerra_sn" ~ "ant_bezerra",
                             stop_name1 == "pra_a_terminal_messejana_sn" ~ "messejana",
                             stop_name1 == "pra_a_terminal_parangaba_sn" ~ "parangaba",
                             stop_name1 == "pra_a_terminal_lagoa_sn" ~ "lagoa",
                             stop_name1 == "pra_a_terminal_siqueira_sn" ~ "siqueira",
                             stop_name1 == "pra_a_terminal_papicu_sn" ~ "papicu",
                             stop_name1 == "pra_a_terminal_conjunto_ceara_sn" ~ "cj_ceara")) %>%
  # brign the shapes
  left_join(terminais %>% rename(stop_name1 = terminal)) %>%
  st_sf()
  
st_crs(stops_routes_terminais) <- 4326

# bind
stop_routes_fim <- rbind(stops_routes_nterminais, stops_routes_terminais) %>%
  arrange(route_id, shape_id, stop_sequence) %>%
  select(-stop_name1)  %>% 
  group_by(route_id) %>%
  summarise(n = n(),
            stop_id = paste0(stop_id, collapse = ","),
            do_union = TRUE) %>%
  select(route_id, stop_id)

# save
readr::write_rds(stop_routes_fim, "data/routes_limits.rds")
