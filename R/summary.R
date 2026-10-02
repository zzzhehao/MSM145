# Config 

library(tidyverse)
library(yaml)
library(DBI)
library(geosphere)

config <- read_yaml("config.yaml")
path.database <- config[[1]]$rov$database

con <- DBI::dbConnect(RSQLite::SQLite(), path.database)
milestone <- DBI::dbReadTable(con, "milestone")

#### Deployment summary

# milestone
summary_milestone <- milestone %>% 
    mutate(timestamp = as.POSIXct(timestamp, tz = "utc")) %>% 
    pivot_wider(
        names_from = log_concept, 
        names_prefix = "timestamp_",
        values_from = timestamp
    ) %>% 
    # summarize
    mutate(
        summary_diveTime = as.numeric(difftime(timestamp_OnDeck, timestamp_IntheWater, tz = "utc", unit = "secs")),
        summary_bottomTime = as.numeric(difftime(timestamp_OfftheBottom, timestamp_AttheBottom, tz = "utc", unit = "secs"))
    )

station_ids <- unique(milestone$station_id)

# telemetry summary

station_id = station_ids[3]

summary_telemetry <- map_dfr(station_ids, \(station_id){
    telemetry <- tbl(con, "telemetry") %>% 
        filter(
            station_id == !!station_id,
            !is.na(lat_rov),
            !is.na(lon_rov)
        ) %>% 
        collect()
    
    # avg speed
    bottomTime_s <- summary_milestone[[which(station_id == station_ids), "summary_bottomTime"]]
    
    tel.sum <- telemetry %>% 
        arrange(timestamp) %>%
        mutate(
            prev_lon = lag(lon_rov),
            prev_lat = lag(lat_rov),
            step_dist_m = distGeo(
                p1 = cbind(prev_lon, prev_lat), 
                p2 = cbind(lon_rov, lat_rov))
        ) %>% 
        group_by(station_id) %>% 
        summarise(
            summary_bottomDistance = sum(step_dist_m, na.rm = TRUE),
            summary_avgSpeedMetric = summary_bottomDistance / bottomTime_s,
            summary_avgSpeedNautical = summary_avgSpeedMetric*1.94384,
            summary_avgDepth = mean(ctd_depth, na.rm = TRUE),
            summary_avgTemperature = mean(ctd_temperature, na.rm = T),
            summary_avgSalinity = mean(ctd_salinity, na.rm = T),
        )

    tel.se <- 
        telemetry %>% 
        filter(timestamp == min(timestamp) | timestamp == max(timestamp)) %>% 
        dplyr::select(station_id, lat_rov, lon_rov, lat_ship, lon_ship) %>% 
        cbind(data.frame(se = c("start", "end"))) %>% 
        pivot_wider(
            names_from = se,
            names_sep = "_",
            values_from = contains(c("rov", "ship"))
        )
    
    left_join(tel.sum, tel.se, by = "station_id")
})

#### Merge and write

deployment <- left_join(summary_milestone, summary_telemetry, by = "station_id")

dbWriteTable(con, "deployment", deployment, overwrite = TRUE)

