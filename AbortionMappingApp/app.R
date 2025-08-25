library(shiny)
library(tidyverse)
library(leaflet)
library(osrm)
library(dplyr)
library(sf)
library(rsconnect)
library(tigris)
options(tigris_use_cache = TRUE)

# Read in data
simulated_df <- readRDS("simulated_df.rds")

### UI Component
ui <- fluidPage(
  titlePanel("Abortion Access Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state",
        label = "Select State:",
        choices = c("AL", "NV", "PA", "MI", "OR", "MO")
      ),
      selectizeInput(
        inputId = "county",
        label = "Origin County:",
        choices = NULL
      ),
      selectInput(
        inputId = "year",
        label = "Year:",
        choices = NULL
      ),
      actionButton("go", "Calculate Route", class = "btn-primary"),
      hr(),
      h4("Abortion Access Metrics:"),
      tableOutput("metricsTable")
    ),
    mainPanel(
      leafletOutput("routeMap", height = "600px"),
      h4("Route Details:"),
      verbatimTextOutput("routeDetails")
    )
  )
)

### Server Logic
server <- function(input, output, session) {
  
  # Set the public OSRM server once globally
  options(osrm.server = "https://router.project-osrm.org/", osrm.profile = "car")
  
  # Reactive: Load appropriate dataset based on selected state
  state_data <- reactive({
    req(input$state)
    simulated_df
  })
  
  observe({
    data <- state_data()
    updateSelectizeInput(
      session, 
      "county",
      choices = sort(unique(data$county_name)),
      selected = character(0)
    )
  })
  
  observe({
    data <- state_data()
    updateSelectInput(
      session,
      "year",
      choices = sort(unique(data$year)),
      selected = character(0)
    )
  })
  
  filtered_data <- reactive({
    req(input$county, input$year)
    
    state_data() %>%
      filter(county_name == input$county,
             year == input$year) %>%
      arrange(distance_origintodest) %>%
      slice(1)
  })
  
  route_data <- eventReactive(input$go, {
    data <- filtered_data()
    req(nrow(data) > 0)
    
    tryCatch({
      route <- osrmRoute(
        src = c(lon = data$origin_lon, lat = data$origin_lat),
        dst = c(lon = data$dest_lon, lat = data$dest_lat),
        overview = "full",
        returnclass = "sf"
      )
      
      # Attach metadata for use in outputs
      route$origin <- input$county
      route$destination <- data$dest_county_name
      route$driving_distance <- round(route$distance, 1)
      
      return(route)
    }, error = function(e) {
      showNotification(paste("Routing failed:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$routeMap <- renderLeaflet({
    if (input$state == "AL") {
      view <- list(lng = -86.9023, lat = 32.3182, zoom = 6)
    } else if (input$state == "NV") {
      view <- list(lng = -116.4194, lat = 38.8026, zoom = 6)
    } else if (input$state == "PA") {
      view <- list(lng = -101.0020, lat = 47.5515, zoom = 6)
    } else if (input$state == "MI"){
      view <- list(lng = -84.5068, lat = 44.1822, zoom = 6)
    } else if(input$state == "OR"){
      view <- list(lng = -120.5000, lat = 43.8041, zoom = 6)
    } else {
      view <- list(lng = -92.172851, lat = 38.57932, zoom = 6)
    }
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = view$lng, lat = view$lat, zoom = view$zoom)
  })
  
  observeEvent(input$go, {
    tryCatch({
      data <- filtered_data()
      req(nrow(data) > 0)
      
      route <- route_data()
      req(!is.null(route))
      
      # Extract the snapped route start/end points
      route_coords <- st_coordinates(route$geometry)
      snapped_origin <- route_coords[1, ]
      snapped_dest <- route_coords[nrow(route_coords), ]
      
      # Define red and blue icons using awesomeIcons
      redIcon <- awesomeIcons(
        icon = 'circle',
        iconColor = 'white',
        markerColor = 'red',
        library = 'fa'
      )
      
      blueIcon <- awesomeIcons(
        icon = 'flag',
        iconColor = 'white',
        markerColor = 'blue',
        library = 'fa'
      )
      
      # Update the leaflet map
      leafletProxy("routeMap") %>%
        clearMarkers() %>%
        clearShapes() %>%
        
        # Add route polyline
        addPolylines(
          data = route,
          color = "#FF6B6B",
          weight = 4,
          label = paste("Driving distance:", route$driving_distance, "miles")
        ) %>%
        
        # Add origin marker at snapped location
        addAwesomeMarkers(
          lng = snapped_origin["X"],
          lat = snapped_origin["Y"],
          icon = redIcon,
          label = paste("Origin:", input$county)
        ) %>%
        
        # Add destination marker at snapped location
        addAwesomeMarkers(
          lng = snapped_dest["X"],
          lat = snapped_dest["Y"],
          icon = blueIcon,
          label = paste("Destination:", data$dest_county_name)
        ) %>%
        
        # Optional: diagnostic green dots
        addCircleMarkers(
          lng = c(snapped_origin["X"], snapped_dest["X"]),
          lat = c(snapped_origin["Y"], snapped_dest["Y"]),
          radius = 4,
          color = "green",
          fillColor = "green",
          fillOpacity = 1
        )
      
    }, error = function(e) {
      showNotification(paste("Map update failed:", e$message), type = "error")
    })
  })
  
  output$routeDetails <- renderPrint({
    route <- route_data()
    req(route)
    
    cat(
      "Origin: ", route$origin, "\n",
      "Destination: ", route$destination, "\n",
      "Driving Distance to Nearest Abortion Provider: ", route$driving_distance, " miles"
    )
  })
  
  output$metricsTable <- renderTable({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data.frame(
      Metric = c("Pregnancies", "Clinical Abortions", "Self-Managed Abortions", "Fetal Deaths"),
      Count = c(
        data$births_total,
        data$abortions_total,
        data$self_managed_abortions,
        data$fetal_deaths
      )
    )
  }, striped = TRUE)
}


shinyApp(ui = ui, server = server)