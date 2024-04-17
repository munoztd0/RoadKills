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
      column(12, style = "margin-top: 20px;",
             leafletOutput("map", height = 600),  # adjust the height as needed
             uiOutput("legend"),
             
             
      )
    ),
    

    
    tags$hr(),  # Add horizontal line
    
    fluidRow(
      column(4,
             infoBoxOutput("totalDisplayVariable")
      ) ,
      column(4,
             downloadButton("downloadData", "Download Filtered Data")
      )#,
      # column(4,
      #        tags$p("To download the map, just take a screenshot.")
      # )
    ),
    tags$hr(),  
    # Add plain text output below the map
    fluidRow(
      column(12,
             htmlOutput("text")
      )
    ),
    tags$hr(),  
    tags$hr()
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
        div(style = "max-height: 150px; overflow-y: auto;",
            selectizeInput("categories", "Select Categories", choices = var_choices, selected = var_choices, multiple = TRUE))
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
  
  output$text <- renderUI({
    HTML("
    Explanation of Variables<br/>
    velo: Number of accidents involving non-motorized bicycles.<br/>
    velo_lent: Number of accidents involving slow-moving bicycles (bicycles with a speed limit of 25 km/h + non-motorized bicycles).<br/>
    velo_25: Number of accidents involving bicycles with a speed limit of 25 km/h.<br/>
    velo_45: Number of accidents involving bicycles with a speed limit of 45 km/h.<br/>
    velo_elec: Number of accidents involving electric motor bicycles.<br/>
    velo_tous: Total number of accidents involving all types of bicycles.<br/>
    pietons: Number of accidents involving pedestrians.<br/>
    nb_enfants_impliques: Number of accidents involving children.<br/>
    nb_enfants_ecole: Number of accidents involving school children.<br/>
    nb_blesses_legers: Number of accidents resulting in minor injuries.<br/>
    nb_blesses_graves: Number of accidents resulting in serious injuries.<br/>
    nb_tues: Number of fatal accidents.<br/>
    total_personnes: Total number of individuals involved in accident.<br/>
  ")
  })
  

  # Add legend
  output$legend <- renderUI({
    legend <- tags$div(
      style = "position: absolute; top: 30px; right: 2em; z-index: 1000; background-color: white; padding: 5px; border: 1px solid #ccc;",
      tags$h4("Legend"),
      tags$p("Size represents"),
      tags$p("the number of accidents:"),
      tags$p("1 accident: "),
      tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: grey; border: 1px solid black; border-radius: 50%;"),
      tags$p("5 accidents: "),
      tags$div(style = "display: inline-block; width: 30px; height: 30px; background-color: grey; border: 1px solid black; border-radius: 50%;"),
      
    )
    return(legend)
  })
}

# Run the app
shinyApp(ui, server)
