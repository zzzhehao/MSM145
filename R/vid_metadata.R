library(tidyverse)
library(jsonlite)
library(fs)
library(yaml)
library(DBI)

config <- read_yaml("config.yaml")
path.database <- config[[1]]$rov$database

get_video_info <- function(file_path) {
    cmd <- sprintf('ffprobe -v quiet -print_format json -show_format -show_streams "%s"', file_path)
    output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    
    if (length(output) == 0 || !is.null(attr(output, "status"))) return(NULL)
    info <- fromJSON(paste(output, collapse = ""))
    
    if (!"streams" %in% names(info) || !"format" %in% names(info)) return(NULL)
    
    video_streams <- info$streams %>% filter(codec_type == "video")
    if (nrow(video_streams) == 0) return(NULL)
    
    video_stream <- video_streams %>% slice(1)
    format_info <- info$format
    
    tibble(
        file_path = file_path,
        file_size_mb = as.numeric(format_info$size) / (1024^2),
        duration_sec = as.numeric(format_info$duration),
        bitrate_kbps = as.numeric(format_info$bit_rate) / 1000,
        width = as.integer(video_stream$width),
        height = as.integer(video_stream$height),
        codec = as.character(video_stream$codec_name)
    )
}

safe_get_video_info <- possibly(get_video_info, otherwise = NULL)
root_dir <- "/Volumes/MSM145 ROV-Dives"

video_files <- dir_ls(root_dir, recurse = TRUE, glob = "*.mov") %>%
    str_subset(pattern = fixed("$RECYCLE.BIN"), negate = TRUE) %>%
    str_subset(pattern = fixed(".Trash"), negate = TRUE)

video_data <- map_dfr(video_files, safe_get_video_info)

video_data.processed <- video_data %>%
    mutate(
        rel_path = path_rel(file_path, start = root_dir),
        camera_folder = map_chr(str_split(rel_path, "/"), 2, .default = NA_character_),
        camera_name = map_chr(str_split(camera_folder, "_"), ~ {
            if (length(.x) >= 4) paste(.x[3], .x[4], sep = "_") else "Unknown_Camera"
        }),
        timestamp = str_extract(basename(file_path), "\\d{8}_\\d{2}-\\d{2}-\\d{2}") %>% as.POSIXct(format="%Y%m%d_%H-%M-%S", tz="UTC"),
        timestamp_end = timestamp + duration_sec,
        station_id = str_extract(rel_path, "^[0-9]+(?=-)") %>% as.numeric() %>% sprintf("MSM145_%s", .)
    ) %>% 
    dplyr::select(timestamp, station_id, everything())

con <- dbConnect(RSQLite::SQLite(), dbname = path.database)
dbWriteTable(con, "video_file", video_data.processed)
dbDisconnect(con)

# Camera summary
camera_summary <- video_data %>%
  group_by(camera_name) %>%
  summarise(
    total_files = n(),
    total_duration_hrs = sum(duration_sec, na.rm = TRUE) / 3600,
    total_size_gb = sum(file_size_mb, na.rm = TRUE) / 1024,
    avg_bitrate_kbps = mean(bitrate_kbps, na.rm = TRUE),
    .groups = "drop"
  )
print(camera_summary)
