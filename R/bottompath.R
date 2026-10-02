library(tidyverse)
library(DBI)

config <- read_yaml("config.yaml")
path.database <- config[[1]]$rov$database

con <- DBI::dbConnect(RSQLite::SQLite(), path.database)
deployment <- DBI::dbReadTable(con, "deployment")

telemetry.bin <- map(1:nrow(deployment), \(.x){
    station_id <- deployment[[.x, "station_id"]]
    t_start <- deployment[[.x, "timestamp_AttheBottom"]] 
    t_end <- deployment[[.x, "timestamp_OfftheBottom"]] 
    tbl(con, "telemetry") %>% 
        filter(
            !is.na(lat_rov),
            !is.na(lon_rov),
            timestamp >= t_start,
            timestamp <= t_end
        ) %>% 
        collect() %>% 
        mutate(
            timestamp_s = as.POSIXct(timestamp),
            timestamp = floor_date(timestamp_s, unit = "5 seconds")
        ) %>% 
        distinct(timestamp, .keep_all = TRUE)
})

if (!dir.exists(dirname(config[[1]]$rov$path_perMin))) {dir.create(dirname(config[[1]]$rov$path_perMin), recursive = TRUE)}
saveRDS(telemetry.bin, config[[1]]$rov$path_perMin)
