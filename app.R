library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(plotly)


source("load_data.R")

ui <- dashboardPage(
  dashboardHeader(title = "Bike Share Station Status"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Station Overview", tabName = "overview", icon = icon("bicycle")),
      menuItem("Geographic Distribution", tabName = "map", icon = icon("map-marker")),
      menuItem("Bike Availability", tabName = "availability", icon = icon("chart-bar"))
    ),
    # Refresh button
    actionButton("refresh", "Refresh Data", icon = icon("sync"))
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_stations"),
                valueBoxOutput("total_bikes"),
                valueBoxOutput("total_ebikes")
              ),
              fluidRow(
                box(title = "Station Status Summary", status = "primary", solidHeader = TRUE,
                    plotlyOutput("station_status_plot")
                ),
                box(title = "Select a Station", status = "info", solidHeader = TRUE,
                    selectInput("station_name", "Station Name", choices = NULL, selected = NULL),
                    valueBoxOutput("selected_station_bikes"),
                    valueBoxOutput("selected_station_ebikes")
                )
              )
      ),

      # Map Tab
      tabItem(tabName = "map",
              fluidRow(
                box(leafletOutput("bike_map"), width = 12)
              )
      ),

      # Availability Tab
      tabItem(tabName = "availability",
              fluidRow(
                box(title = "Bike Availability Distribution", status = "primary", solidHeader = TRUE,
                    plotlyOutput("bike_availability_histogram")
                ),
                box(title = "Stations by Bike Types", status = "primary", solidHeader = TRUE,
                    plotlyOutput("bike_type_distribution")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive data loading
  latest_data <- reactiveVal()

  observe({
    # Load data
    data <- load_most_recent_bike_data()
    latest_data(data)

    # Update station names for dropdown
    updateSelectInput(session, "station_name", choices = latest_data()$name, selected = latest_data()$name[1])
  })

  observeEvent(input$refresh, {
    # Load data when refresh is clicked
    data <- load_most_recent_bike_data()
    latest_data(data)

    # Update station names for dropdown
    updateSelectInput(session, "station_name", choices = latest_data()$name, selected = latest_data()$name[1])
  })

  # Value Boxes
  output$total_stations <- renderValueBox({
    req(latest_data())
    valueBox(
      nrow(latest_data()),
      "Total Stations",
      icon = icon("bicycle"),
      color = "blue"
    )
  })

  output$total_bikes <- renderValueBox({
    req(latest_data())
    valueBox(
      sum(latest_data()$num_bikes_available),
      "Total Bikes Available",
      icon = icon("bicycle"),
      color = "green"
    )
  })

  output$total_ebikes <- renderValueBox({
    req(latest_data())
    valueBox(
      sum(latest_data()$num_ebikes_available),
      "E-Bikes Available",
      icon = icon("bolt"),
      color = "yellow"
    )
  })

  # Station Status Plot
  output$station_status_plot <- renderPlotly({
    req(latest_data())
    station_status <- latest_data() %>%
      summarise(
        Installed = sum(is_installed),
        Renting = sum(is_renting),
        Returning = sum(is_returning),
        Disabled = sum(num_docks_disabled > 0)
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Status", values_to = "Count")

    ggplot(station_status, aes(x = Status, y = Count, fill = Status)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(y = "Number of Stations")
  })

  # Bike Availability Histogram
  output$bike_availability_histogram <- renderPlotly({
    req(latest_data())
    ggplot(latest_data(), aes(x = num_bikes_available)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      theme_minimal() +
      labs(
        title = "Distribution of Bikes Available per Station",
        x = "Number of Bikes Available",
        y = "Number of Stations"
      )
  })

  # Bike Type Distribution
  output$bike_type_distribution <- renderPlotly({
    req(latest_data())
    bike_types <- latest_data() %>%
      summarise(
        Regular = sum(num_bikes_available),
        EBikes = sum(num_ebikes_available),
        Disabled = sum(num_bikes_disabled)
      ) %>%
      tidyr::pivot_longer(everything(), names_to = "Type", values_to = "Count")

    ggplot(bike_types, aes(x = Type, y = Count, fill = Type)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Bike Type Distribution", y = "Total Count")
  })

  # Interactive Map
  output$bike_map <- renderLeaflet({
    req(latest_data())
    leaflet(latest_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        radius = ~(num_bikes_available + 1),  # Size based on bike availability
        popup = ~paste(
          "Station:", name,
          "<br>Bikes Available:", num_bikes_available,
          "<br>E-Bikes:", num_ebikes_available,
          "<br>Capacity:", capacity
        ),
        color = ~ifelse(num_bikes_available > 0, "green", "red"),
        fillOpacity = 0.7
      )
  })

  # Value Boxes for Selected Station
  filtered_station_data <- reactive({
    req(latest_data())
    latest_data() %>% filter(name == input$station_name)
  })

  output$selected_station_bikes <- renderValueBox({
    req(filtered_station_data())
    valueBox(
      filtered_station_data()$num_bikes_available[1],
      "Bikes Available",
      icon = icon("bicycle"),
      color = "green"
    )
  })

  output$selected_station_ebikes <- renderValueBox({
    req(filtered_station_data())
    valueBox(
      filtered_station_data()$num_ebikes_available[1],
      "E-Bikes Available",
      icon = icon("bolt"),
      color = "yellow"
    )
  })
}

# Run the application
shinyApp(ui, server)
