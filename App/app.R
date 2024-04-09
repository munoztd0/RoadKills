# Load required libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(shinydashboard)

options(warn = -1)


#define ui 
ui <- dashboardPage(
  dashboardHeader(title = "Accidents Genève"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$footer(style = "position: fixed; bottom: 0; width: 100%; background-color: #f5f5f5; padding: 10px; text-align: center;",
                HTML("Created by <a href='https://github.com/munoztd0' target='_blank'>David Munoz Tord</a>, the data is downloaded from the SITG <a href='https://ge.ch/sitg/geodata/SITG/OPENDATA/8139/CSV_OTC_ACCIDENTS.zip' target='_blank'>website</a>")
    ),
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
      column(4,
             infoBoxOutput("totalDisplayVariable")
      ) ,
      column(4,
             downloadButton("downloadData", "Download Filtered Data")
      ),
      column(4,
             tags$p("To download the map, just take a screenshot.")
      )
    )
  )
)

# Define Server function
server <- function(input, output, session) {
  
  # Load dummy dataset
  data <- reactive({
    # Load your dataset here
    read_csv("data/data.csv")
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
  
  # Render map
  output$map <- renderLeaflet({
    leaflet() %>%
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
  
}

# Run the app
shinyApp(ui, server)