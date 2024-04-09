# Load required libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(shinydashboard)
library(mapview)

#define ui 
ui <- dashboardPage(
  dashboardHeader(title = "Geolocation Data Visualization"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Add input controls for filtering/selecting variables
    fluidRow(
      column(4,
        selectizeInput("display_variable", "Select Variable to Display", choices = c("velo", "velo_lent", "velo_25", "velo_45", "velo_elec", "velo_tous", "pietons", "nb_enfants_impliques", "nb_enfants_ecole", "nb_blesses_legers", "nb_blesses_graves", "nb_tues", "nb_pietons", "total_personnes"))
      ),
      column(4,
        selectizeInput("filter_variable", "Select Variable to Filter", choices = c("groupe_accident", "type", "cause", "commune", "conditions_lumineuses", "conditions_meteo", "consequences", "etat_route", "genre_route"), multiple = TRUE),
        uiOutput("categoryFilters")
      ),
      column(4,
        dateRangeInput("dateRange", "Select Date Range", start = "2022-01-01", end = "2023-01-01"),
        textInput("id_accident", "Enter Accident ID")  # Add text input for "id_accident"
        # Add other input controls as needed
      )
    ),
    
    # Add map
    fluidRow(
      column(12,
        leafletOutput("map", height = 600)  # adjust the height as needed
      )
    ),

    tags$hr(),  # Add horizontal line

    fluidRow(
      column(6,
        infoBoxOutput("totalDisplayVariable")
      ),
      column(6,
        downloadButton("downloadData", "Download Filtered Data"),
        downloadButton("dl", "Download Map Image")
      ) 
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
      filtered <- filtered %>% select(date, lat, long, id_accident, all_of(c(input$display_variable, input$filter_variable)))
      filtered <- filtered[filtered[[input$display_variable]] != 0, ]

    }
    if (!is.null(input$dateRange)) {
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      filtered <- filtered %>% filter(date >= start_date, date <= end_date)
    }
    if (!is.null(input$categories) && !is.null(input$filter_variable) && length(input$categories) > 0) {
      filtered <- filtered %>% filter(get(input$filter_variable) %in% input$categories)
    }
    if (!is.null(input$id_accident) && input$id_accident != "") {
      filtered <- filtered %>% filter(id_accident == as.numeric(input$id_accident))  # Filter data based on entered "id_accident"
    }
    filtered
  })
  
  # Create a reactiveValues object to store the map
  map <- reactiveValues(dat = NULL)

  # Render map
  output$map <- renderLeaflet({
    map$dat <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = filtered_data(), 
                       radius = ~get(input$display_variable)*7, 
                       color = "grey",
                       fillOpacity = 0.3,
                       popup = ~paste("Accidents:", id_accident),  # add popups
                       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                             zoomToBoundsOnClick = FALSE,
                                                             spiderfyOnMaxZoom = FALSE,
                                                             removeOutsideVisibleBounds = TRUE,
                                                             disableClusteringAtZoom = 18))
    map$dat
  })

  # Update filtered data whenever any UI input changes
  observeEvent(c(input$display_variable, input$filter_variable, input$dateRange, input$categories), {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = filtered_data(), 
                       radius = ~get(input$display_variable)*7, 
                       color = "grey",
                       fillOpacity = 0.3,
                       popup = ~paste("Accidents:", id_accident),  # add popups
                       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                                             zoomToBoundsOnClick = FALSE,
                                                             spiderfyOnMaxZoom = FALSE,
                                                             removeOutsideVisibleBounds = TRUE,
                                                             disableClusteringAtZoom = 18))
  })

  output$totalDisplayVariable <- renderInfoBox({
    total <- sum(filtered_data()[, input$display_variable], na.rm = TRUE)
    infoBox("Total", total, icon = icon("list"), color = "purple")
  })

  # Add to server function
  output$downloadData <- downloadHandler(
  filename = function() {
    paste("filtered_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(filtered_data(), file, row.names = FALSE)
  }
 )

  # Add download handler for the map
  output$dl <- downloadHandler(
  filename = "map.png",
  content = function(file) {
    tmp_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(map$dat, tmp_file, selfcontained = FALSE)
    webshot2::webshot(tmp_file, file = file)
  }
 )
}

# Run the app
shinyApp(ui, server)