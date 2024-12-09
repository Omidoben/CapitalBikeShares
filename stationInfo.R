library(BikeDataCollection)
library(dplyr)
library(DBI)
library(RMySQL)
library(dotenv)


collect_station_info <- function(){

  station_info <- feeds_urls() %>%
    filter(name == "station_information") %>%
    pull(url) %>%
    get_data()

  # Tidy the data
  station_info <- station_info %>%
    magrittr::extract2("data") %>%
    dplyr::mutate(last_updated = station_info$last_updated) %>%
    dplyr::select(
      station_id,
      name,
      lat,
      lon,
      capacity,
      last_updated
    )

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

  # Create table to store the data if it doesn't exist
  if(!dbExistsTable(con, "bike_station_info")){
  con %>% dbCreateTable(
    name = "bike_station_info",
    fields = station_info)
  }

  # write station_info only if table is empty
  if(dbGetQuery(con, "SELECT COUNT(*) FROM bike_station_info")[[1]] == 0){
    con %>%
      dbWriteTable(
        name = "bike_station_info",
        value = station_info,
        append = TRUE,
        row.names = FALSE
        )
    message("Station information collected and stored successfully.")
  } else{
    message("Station information already exists in the database.")
  }

  # Update pin
  board <- pins::board_folder("StationInfoPin")

  existing_pins <- pins::pin_list(board)
  if(!"bikeshare-station-info-pin" %in% existing_pins) {
    pins::pin_write(
      board,
      x = station_info,
      type = "csv",
      name = "bikeshare-station-info-pin",
      title = "Capital Bikeshare Station Information",
      description = "Bike station details from Capital Bikeshare API"
    )

    # prune versions
    pins::pin_versions_prune(board, "bikeshare-station-info-pin", 10)

    message("Station information pinned successfully.")
  } else {
    message("Station information pin already exists.")
  }

  dbDisconnect(con)
}
















