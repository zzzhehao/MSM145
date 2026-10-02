library(tidyverse)
library(fs)
library(cli)
library(DBI)
library(yaml)

config <- read_yaml("config.yaml")

path.zip <- config[[1]]$rov$zip
path.unzip <- config[[1]]$rov$unzip
path.database <- config[[1]]$rov$database

# Unzip 
files.zip <- list.files(path.zip, recursive = T, pattern = "\\.zip$", full.names = T)
folder.unzip <- path(path.unzip, path_ext_remove(path_rel(files.zip, start = path.zip)))

walk2(files.zip, folder.unzip, \(zip_path, target_dir) {
    dir_create(target_dir)
    unzip(zipfile = zip_path, exdir = target_dir)
})

# Remove OFOP image
file.remove(list.files(path.unzip, recursive = T, pattern = "\\.bmp$", full.names = T))

# HF
parse_protocol <- function(path) {
  
    # Header
    header <- read.table(path, sep = "\t", nrow = 3, strip.white = TRUE) %>% 
        as_tibble() %>% 
        rename(field = V1, value = V2) %>% 
        mutate(field = str_remove_all(field, ":|\\s+"))
  
    # Operation Data
    operation <- read.table(path, sep = "\t", skip = 5, nrow = 4, strip.white = TRUE, header = FALSE, fill = TRUE) %>% 
        set_names(c("Task", "PC Date and Time", "UTC Time", "UTC Date", "SHIP Latitude", "SHIP Longitude", "SUB_1 Latitude", "SUB_1 Longitude", "Water Depth")) %>% 
        mutate(Task = str_remove_all(Task, ":|\\s+"))
  
    # Main Data
    data <- read.table(path, sep = "\t", skip = 11, header = FALSE, fill = TRUE) %>% 
        set_names(c("Date", "Time", "PC_Time", "SHIP_Lon", "SHIP_Lat", "SHIP_SOG", "SHIP_COG", "SHIP_Hdg", "Water_Depth", "SUB1_Lon", "SUB1_Lat", "SUB1_Depth", "SUB1_Altitude", "Elapsed video Time", "Image-Video Path", "Observations/Comments"))
    
    attr(data, "header") <- header
    attr(data, "operation") <- operation
    
    return(data)
}

dive.folder_unzip.master <- list.dirs(path.unzip, full.names = TRUE, recursive = FALSE)

rov.data <- map(dive.folder_unzip.master, \(dive_folder) {
  
    #### Science protocols
    sp.path <- list.files(dive_folder, pattern = "(scienceprotocol).+(prot)", recursive = TRUE, full.names = TRUE)
    
    if (length(sp.path) > 0) {
        if (length(sp.path) > 1) {
            # FIXED: which.max() gets the index, max() gets the value
            row_counts <- map_dbl(sp.path, ~ nrow(read.table(.x, sep = "\t", skip = 5, nrow = 4, fill = TRUE)))
            sp_target <- sp.path[which.max(row_counts)]
        } else {
            sp_target <- sp.path[1]
        }
            scienceprotocol <- parse_protocol(sp_target)
    } else {
        cli_alert_warning("No science protocol found for dive `{basename(dive_folder)}`.")
        scienceprotocol <- NA
    }
    
    #### Science room protocols
    srp.path <- list.files(dive_folder, pattern = "(scienceroomprotocol).+(prot)", recursive = TRUE, full.names = TRUE)
    
    if (length(srp.path) > 0) {
        scienceroomprotocol <- parse_protocol(srp.path[1])
    } else {
        cli_alert_warning("No science room protocol found for dive `{basename(dive_folder)}`.")
        scienceroomprotocol <- NA
    }

    #### Metadata

    # Safely pull metadata from whichever protocol exists
    meta_source <- if (is.data.frame(scienceroomprotocol)) {
        scienceroomprotocol
    } else if (is.data.frame(scienceprotocol)) {
        scienceprotocol
    } else {
        NULL
    }

    #### Compile List 1/2
    rov.data <- list(
        scienceprotocol = scienceprotocol,
        scienceroomprotocol = scienceroomprotocol
    )

    #### Metadata Extraction
    attr(rov.data, "dive") <- basename(dive_folder)
    
    if (!is.null(meta_source)) {
        header <- attr(meta_source, "header")
        operation <- attr(meta_source, "operation")
        
        cruise_val <- header[[1, 2]]
        station_val <- as.numeric(str_extract(header[[2, 2]], "^[0-9]+"))
        
        attr(rov.data, "cruise") <- cruise_val
        attr(rov.data, "station") <- station_val
        attr(rov.data, "deployment") <- operation %>% 
            mutate(
                station = station_val, 
                station_id = paste0(cruise_val, "_", station_val),
                .before = everything()
            )
    }

    station_id <- attr(rov.data, "deployment") %>% pull(station_id) %>% unique()
    if (length(station_id) > 1) {cli::cli_abort("Multiple station ID ({paste(station_id)}) found when processing folder `{dive_folder}`.")}
    
    #### Telemetry
    tele.path <- list.files(dive_folder, pattern = "TLS", recursive = TRUE, full.names = TRUE)
    
    telemetry <- map_dfr(tele.path, ~{
        if (length(readLines(.x, n = 2)) == 2) { 
            # the last file in telemetry series are often blank or only contains the header
            read_csv(.x, show_col_types = FALSE, trim_ws = TRUE)
        } else {
            NULL
        }
    }) %>% 
    as_tibble() %>% 
    mutate(
        Timestamp = as.POSIXct( # format timestamp as POSIX
            Timestamp, 
            tz = "UTC", 
            format = "%d.%m.%Y %H:%M:%S"
        ), 
        station_id = station_id,
        .before = everything()
    ) %>% 
    filter(!is.na(Timestamp)) # remove comment lines

    #### Compile list 2/2
    rov.data$telemetry <- telemetry
    
    return(rov.data)
})


sci.ptc <- map_dfr(rov.data, \(dive_data) {
    dive_name <- attr(dive_data, "dive")
    
    if (is.data.frame(dive_data$scienceroomprotocol)) {
        ptc <- dive_data$scienceroomprotocol
    } else if (is.data.frame(dive_data$scienceprotocol)) {
        ptc <- dive_data$scienceprotocol
    } else {
        cli::cli_abort("Dive `{dive_name}` does not have any science protocol.")
    }
    
    # append the station ID
    ptc %>% 
        mutate(station_id = paste0(attr(dive_data, "cruise"), "_", attr(dive_data, "station")))
})

#### Consolidate

scilog <- sci.ptc %>% 
    dplyr::select(
        timestamp = PC_Time,
        station_id,
        log_concept = `Observations/Comments`
    ) %>% 
    mutate(
        timestamp = as.POSIXct(timestamp, tz = "utc", format = "%d.%m.%Y %H:%M:%S"),
        log_owner = "onboard", 
        .after = everything()
    )

telemetry_raw <- map_dfr(rov.data, "telemetry")

milestone <- map_dfr(rov.data, \(dive_data) {
    attr(dive_data, "deployment") %>% 
        dplyr::select(-`Water Depth`)
}) %>% 
    dplyr::select(
        station_id, 
        Task,
        `UTC Date`, 
        `UTC Time`
    ) %>% 
    mutate(
        log_concept = Task,
        timestamp = paste(`UTC Date`, `UTC Time`) %>% as.POSIXct(tz = "utc", format = "%d.%m.%Y %H:%M:%S"),
        .keep = "unused"
    )

#### Clean data: Telemetry

#' Convert OFOP coordinates (in DDM but without separator) into DD coordinates
ofopcoords2dd <- function(ddmcat, dir) {
    if (!class(ddmcat) == "numeric") {
        ddmcat <- as.numeric(ddmcat)
    }
    deg <- as.numeric(trunc(ddmcat/100))
    min <- ddmcat %% 100
    deg.d <- min / 60
    ddeg <- deg + deg.d

    # direction
    neg <- ifelse(str_detect(dir, "W|S"), -1, 1)
    neg <- ifelse(is.na(dir), NA, neg) # if direction is NA then drop coords
    ddeg <- ddeg * neg
    return(ddeg)
}

telemetry <- telemetry_raw %>% 
    dplyr::select(
        timestamp = Timestamp,
        station_id = station_id,
        heading = ROV.ROV.Heading,
        depth = SBE49CTD.CTD.Depth,
        lat_rov = OFOP.OFOP_GLL.Lat,
        lat_rov.dir = OFOP.OFOP_GLL.LatNS,
        lon_rov = OFOP.OFOP_GLL.Lon,
        lon_rov.dir = OFOP.OFOP_GLL.LonWE,
        lat_ship = Merian.MERIAN.SysPosLat,
        lat_ship.dir = Merian.MERIAN.SysPosLatN,
        lon_ship = Merian.MERIAN.SysPosLon,
        lon_ship.dir = Merian.MERIAN.SysPosLonW,
        ctd_scannr = SBE49CTD.CTD.ScanNr,
        ctd_temperature = SBE49CTD.CTD.Temperature,
        ctd_salinity = SBE49CTD.CTD.Salinity,
        ctd_pressure = SBE49CTD.CTD.Pressure,
        ctd_density = SBE49CTD.CTD.Density,
        ctd_depth = SBE49CTD.CTD.Depth
    ) %>% 
    mutate(
        station_id = factor(station_id),
        heading = as.numeric(heading),
        depth = as.numeric(depth),
        lat_rov = ofopcoords2dd(lat_rov, lat_rov.dir),
        lon_rov = ofopcoords2dd(lon_rov, lon_rov.dir),
        lat_ship = ofopcoords2dd(lat_ship, lat_ship.dir),
        lon_ship = ofopcoords2dd(lon_ship, lon_ship.dir),
        across(starts_with("ctd"), ~as.numeric(.x)),
        .keep = "unused"
    )

## Separate source for ROV1 (OFOP data source corrupted)
# input ranger2 transponder data
rov1.coord <- read.table(
    "data/ranger2/MSM145_ranger2_rov1.txt", 
    sep = "\t", 
    fill = T, 
    header = TRUE, 
    comment.char = "", 
    na.strings = c("#", "")
) %>% as_tibble()
rov1.coord <- rov1.coord %>% 
    mutate(timestamp = as.POSIXct(date.time, tz = "UTC", format = "%Y/%m/%d %H:%M:%S"), .keep = "unused", .before = everything()) %>% 
    filter(!is.na(timestamp)) 

# extract ROV1 period

rov1.milestone <- attr(rov.data[[1]], "deployment") %>% select(`UTC Date`, `UTC Time`) %>% 
    mutate(timestamp = as.POSIXct(paste(`UTC Date`, `UTC Time`), tz = "UTC", format = "%d.%m.%Y %H:%M:%S"))

rov1.start <- min(rov1.milestone$timestamp)
rov1.end <- max(rov1.milestone$timestamp)
rov1.bottomCoord <- rov1.coord %>% 
    filter(
        timestamp >= rov1.start & timestamp <= rov1.end) %>% 
    mutate(
        lat_rov = as.numeric(Ranger2.PSONLLD.2601.position_latitude),
        lon_rov = as.numeric(Ranger2.PSONLLD.2601.position_longitude),
        .keep = "unused"
    ) %>% 
    dplyr::select(timestamp, lat_rov, lon_rov) %>% 
    filter(
        !is.na(lat_rov) & 
        as.numeric(lat_rov) != 0 &
        !is.na(lon_rov) & 
        as.numeric(lon_rov) != 0
    )

# overwrite OFOP coords for ROV1

telemetry <- telemetry %>% 
    rows_update(rov1.bottomCoord, unmatched = "ignore")

# clean up erroneous 0N 0E coords

check0coords <- function(coords) {
    avg.coords <- mean(coords[coords != 0], na.rm = T)
    if (avg.coords > 1) around0 <- F else around0 <- T # 1 degree offset around 0N 0E ~ 100 km distance. It'd be a madness for one ROV dive
    coords 
}

telemetry <- telemetry %>% 
    group_by(station_id) %>% 
    mutate(across(c(lat_rov, lon_rov, lat_ship, lon_ship), \(x) {
            valid_x <- x[!is.na(x) & x != 0] # validity check
            if (length(valid_x) > 0) {
                mean.x <- median(valid_x)

                if (abs(mean.x) > 1) {
                    x <- na_if(x, 0)
                } else {
                    cat(sprintf("Current coordinate series (station {cur_group()[[1]]}, column {cur_column()} seems to be around 0N 0E. Validate and clean data manually."))
                }
                
                # some erroneous data in ROV1 around 11N
                outlier_mask <- !is.na(x) & abs(x - mean.x) > 2
                x[outlier_mask] <- NA_real_
            }
                
            return(x)
        }
    )) %>%
    ungroup()

#### Initiate database

if (!dir.exists(dirname(path.database))) dir.create(dirname(path.database), recursive = T)
con <- dbConnect(RSQLite::SQLite(), dbname = path.database)

rov_tables <- list(
    scilog = scilog,
    telemetry_raw = telemetry_raw,
    telemetry = telemetry,
    milestone = milestone
)

iwalk(rov_tables, \(df, table_name) {
    dbWriteTable(
        conn = con, 
        name = table_name, 
        value = df, 
        overwrite = TRUE 
    )
})

DBI::dbDisconnect(con)
