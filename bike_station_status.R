library(BikeDataCollection)
library(dplyr)
library(DBI)
library(RMySQL)
library(dotenv)
library(lubridate)
library(logging)
library(blastula)

# Create the credentials file
#create_smtp_creds_file(
#  file = "email_creds",
#  user = "benomido2001@gmail.com",
#  provider = "gmail"
#)

# Configure logging
basicConfig(level='FINEST')
addHandler(writeToFile, file="bike_data_collection.log")


safe_collect_daily_status <- function() {
  tryCatch({
    # Log start of data collection
    loginfo("Starting daily bike station status collection")

    # Validate environment variables
    required_env_vars <- c("DB_HOST", "DB_USER", "DB_PASSWORD")
    for (var in required_env_vars) {
      if (Sys.getenv(var) == "") {
        stop(paste("Missing required environment variable:", var))
      }
    }

    # Collect station status data
    station_status <- tryCatch({
      feeds_urls() %>%
        filter(name == "station_status") %>%
        pull(url) %>%
        get_data()
    }, error = function(e) {
      logerror(paste("Failed to fetch station status:", e$message))
      stop("Data collection failed")
    })

    # Process and clean data
    processed_data <- station_status %>%
      magrittr::extract2("data") %>%
      dplyr::mutate(time = station_status$last_updated) %>%
      dplyr::select(
        is_installed, num_bikes_available, last_reported, is_renting,
        num_docks_available, num_docks_disabled, is_returning,
        station_id, num_ebikes_available, num_bikes_disabled,
        num_scooters_available, time
      ) %>%
      dplyr::distinct()

    if (nrow(processed_data) == 0) {
      stop("No data collected after processing")
    }

    # Database connection
    con <- tryCatch({
      dbConnect(
        MySQL(),
        dbname = "bikeshare_data",
        host = Sys.getenv("DB_HOST"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = 3306,
        local_infile = 1,
        client.flag = CLIENT_LOCAL_FILES
      )
    }, error = function(e) {
      logerror(paste("Database connection failed:", e$message))
      stop("Could not connect to database")
    })

    # Ensure the connection is closed after execution
    on.exit(if (exists('con') && dbIsValid(con)) dbDisconnect(con))

    # Create table to store the data if not exists
    if (!dbExistsTable(con, "bike_station_raw")) {
      loginfo("Creating bike_station_raw table")
      dbCreateTable(con, "bike_station_raw",
                    fields = processed_data)
    }

    # Write data
    dbWriteTable(
      con,
      name = "bike_station_raw",
      value = processed_data,
      append = TRUE,
      row.names = FALSE
    )
    loginfo(paste("Successfully collected and stored",
                  nrow(processed_data), "records"))

    # Send success email
    email <- compose_email(
      body = md(glue::glue("
        **Bike Station Data Collection Successful!**

        - Records Collected: {nrow(processed_data)}
        - Timestamp: {Sys.time()}
      "))
    )

    smtp_send(
      email,
      from = "benomido2001@gmail.com",
      to = "benomido2001@gmail.com",
      subject = "Daily Bike Station Data Collection - Success",
      credentials = creds_file("email_creds")
    )
  }, error = function(e) {
    # Log the error
    logerror(paste("An error occurred in safe_collect_daily_status:", e$message))

    # Send failure email
    email <- compose_email(
      body = md(glue::glue("
        **Bike Station Data Collection Failed**

        - Error Message: {e$message}
        - Timestamp: {Sys.time()}
      "))
    )

    smtp_send(
      email,
      from = "benomido2001@gmail.com",
      to = "benomido2001@gmail.com",
      subject = "Daily Bike Station Data Collection - Failure",
      credentials = creds_file("email_creds")
    )

    stop("safe_collect_daily_status failed")
  })
}

#safe_collect_daily_status()







