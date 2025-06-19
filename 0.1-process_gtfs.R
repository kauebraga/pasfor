library(kauetools)
library(gtfstools)
library(stringr)

# open
# gtfs <- read_gtfs("data-raw/gtfs_20230519_mod.zip", encoding = "Latin-1")
gtfs <- read_gtfs("data-raw/2025/gtfs_202503.zip", encoding = "Latin-1")

# create direction id
gtfs$trips <- gtfs$trips %>%
  mutate(direction_id = ifelse(stringr::str_sub(shape_id, start = -1, end = -1) == "I", 0, 1))

# save
write_gtfs(gtfs, "data/gtfs_202503_mod.zip")
