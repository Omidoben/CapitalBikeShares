library(BikeDataCollection)
library(dplyr)
library(DBI)
library(RMySQL)
library(dotenv)
library(lubridate)
library(logging)
library(blastula)
library(pins)

# Source the individual scripts
source("stationInfo.R")
source("bike_station_status.R")
source("load_data.R")
#source("R/bike_station_dashboard.R")

# Master workflow function
execute_bike_share_workflow <- function() {
  tryCatch({

    loginfo("Starting station information collection")
    collect_station_info()

    loginfo("Starting daily station status collection")
    safe_collect_daily_status()

    loginfo("Verifying data loading")
    latest_data <- load_most_recent_bike_data()

    if (nrow(latest_data) == 0) {
      stop("No data available for the Shiny app")
    }

    loginfo(paste("Loaded", nrow(latest_data), "station records"))

    loginfo("Bike share data collection workflow completed successfully")

    return(TRUE)

  }, error = function(e) {
    logerror(paste("Workflow execution failed:", e$message))

    stop("Bike share workflow failed")
  })
}

# If the script is run directly (not sourced), execute the workflow
if (sys.nframe() == 0) {
  execute_bike_share_workflow()
}





