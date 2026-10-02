# MSM145 Post-cruise Data Handling by Zhehao

## ROV Telemetry and Logs

### Getting started

Raw data from all dives are cleaned and consolidated into a SQL database. All actual data files are shared via a private Google Drive link. By placing `data/` at working directory, you can use scripts `db_init.R`, `summary.R` and `vid_metadata.R` to recreate the database.

You can manage your database location using `config.yaml`.

You can use `database2csv.R` to generate CSV from all tables in the SQL database. 

Use `DBI::dbConnect(RSQLite::SQLite(), "msm145_rov.sqlite")` to connect SQLite database in R, and use [{{dbplyr}}](https://dbplyr.tidyverse.org/articles/dbplyr.html) to interact with the database in the tidyverse way. 

### Details

- Tables
    - `deployment`: ROV deployment-level data per dive. 
    - `milestone`: Timestamp of all deployment-level event. 
    - `scilog`: Science logging protocol from OFOP (conference room).
    - `telemetry_raw`: Raw telemetry data from OFOP with minimal data cleaning.
    - `telemetry`: Curated and cleaned telemetry data.
    - `video_file`: Metadata of all video files. 
- Timestamp is UNIX Epoch and the primary key in all tables. 
- Coordinates from ROV1 (MSM145_25) originate from Ranger2 extracts from DSHIP (see `db_init.R`). All other telemetry data was consolidated from OFOP telemetry protocol. 
