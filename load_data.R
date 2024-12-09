# load_data.R
library(DBI)
library(RMySQL)
library(dplyr)
library(tibble)

# Function to load most recent bike data
load_most_recent_bike_data <- function() {
  # Database connection
  con <- dbConnect(
    MySQL(),
    dbname = "bikeshare_data",
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = 3306,
    local_infile = 1,
    client.flag = CLIENT_LOCAL_FILES
  )

  # Fetch the most recent record
  latest_station_status_data <- dbGetQuery(
    con,
    "SELECT * FROM bike_station_raw
     WHERE time = (SELECT MAX(time) FROM bike_station_raw)"
  ) %>%
    tibble::tibble()

  latest_station_info_data <- dbGetQuery(con, "SELECT * FROM bike_station_info") %>%
    tibble::tibble()

  # Join and process data
  df <- latest_station_status_data %>%
    left_join(latest_station_info_data, by = "station_id") %>%
    select(-last_updated)

  # Close database connection
  dbDisconnect(con)

  return(df)
}


