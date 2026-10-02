library(tidyverse)
library(DBI)

config <- yaml::read_yaml("config.yaml")
path.database <- config[[1]]$rov$database

con <- dbConnect(RSQLite::SQLite(), dbname = path.database)
tbl.name <- dbListTables(con)

walk(tbl.name, ~{
    tbl <- dbReadTable(con, .x)
    write.table(tbl, sprintf("data/database/csv/%s.csv", .x), sep = "\t", row.names = F, fileEncoding = "UTF-8")
})
