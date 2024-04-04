# Load required libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Geolocation Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      # Add input controls for filtering/selecting variables
      selectizeInput("display_variable", "Select Variable to Display", choices = c("nb_seniors", "nb_trains")),
      selectizeInput("filter_variable", "Select Variable to Filter", choices = c("groupe_accident", "consequences"), multiple = TRUE),
      # Add date range input control
      dateRangeInput("dateRange", "Select Date Range", start = "2022-01-01", end = "2023-01-01"),
      uiOutput("categoryFilters"),
      # Add other input controls as needed
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define Server function
server <- function(input, output, session) {
  
  # Load dummy dataset
  data <- reactive({
    # Load your dataset here
    read_csv("data/CSV_OTC_ACCIDENTS/OTC_ACCIDENTS_GPS.csv")
  })
  
  # Dynamically generate category filters based on selected variable to filter
  output$categoryFilters <- renderUI({
    if (!is.null(input$filter_variable)) {
      var_choices <- unique(data()[[input$filter_variable]])
      if (!is.null(var_choices)) {
        selectizeInput("categories", "Select Categories", choices = var_choices, selected = var_choices, multiple = TRUE)
      }
    }
  })
  
  # Clean and filter data based on user inputs
  filtered_data <- reactive({
    filtered <- data()
    if (!is.null(input$display_variable)) {
      filtered <- filtered %>% select(date, lat, long, all_of(c(input$display_variable, input$filter_variable)))
    }
    if (!is.null(input$dateRange)) {
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      filtered <- filtered %>% filter(date >= start_date, date <= end_date)
    }
    if (!is.null(input$categories) && !is.null(input$filter_variable) && length(input$categories) > 0) {
      filtered <- filtered %>% filter(get(input$filter_variable) %in% input$categories)
    }
    filtered
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = filtered_data(), 
                       radius = 5,
                       color = "red",
                       fillOpacity = 0.8)
  })
  
  # Update filtered data whenever any UI input changes
  observeEvent(c(input$display_variable, input$filter_variable, input$dateRange, input$categories), {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = filtered_data(), 
                       radius = 5,
                       color = "red",
                       fillOpacity = 0.8)
  })
}

# Run the app
shinyApp(ui, server)
