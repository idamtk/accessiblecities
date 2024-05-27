#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
wheels_data_cph <- readr::read_rds("cph_wheels.rds")
wheels_data_aar <- readr::read_rds("aarhus_wheels.rds")
wheels <- list(chp=wheels_data_cph,aar=wheels_data_aar)
amenities <- c("All", "parking", "restaurant")
token <- "pk.eyJ1IjoiaWRhbXRrIiwiYSI6ImNsdGUycjB3dTBhbmkyaXJ0bnh4dGt6MGwifQ.rfH9mhRsZwCVuTXz34PiSA"
library(mapview)
library(mapboxapi)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(
    textInput("place", label = "place",
              placeholder = "Indtast en addresse eller et navn på et sted"),
    actionButton("action", "Søg"),
    width = 3,
    selectInput("dataset", label = "Dataset", choices = names(wheels)),
    selectInput("amenity", label = "Amenity", choices = amenities),
  ),
  
  mainPanel(
    leafletOutput(outputId = "map", width = "100%", height = 1000)
  )
)

server <- function(input, output, session) {
  

  
  
    
  output$map <- renderLeaflet({
    
    if (input$amenity=="All"){
      wheels_data <- wheels[[input$dataset]]
    } else {
      wheels_data <- wheels[[input$dataset]]
      wheels_data <- dplyr::filter(wheels_data, wheelchair=="yes" & amenity==input$amenity)
    }
    leaflet() %>%
      addMapboxTiles(style_id = "satellite-streets-v11",
                     username = "mapbox",
                     access_token = token)  %>%
      addCircleMarkers(data = wheels_data,
                 lat = ~Y, 
                 lng = ~X,
                 color=~color)
    #, popup = ~placename)
  })
  selected_place <- reactive({
    
    req(input$place)  # Ensure input is available
    wheels_data <- wheels[[input$dataset]]
    wheels_data %>% dplyr::filter(name == input$place) ## add address if poss
  })
  observe({
    sel_place <- selected_place()
    
    if (nrow(sel_place) > 0) {
      leafletProxy("map") %>%
        setView(lng = sel_place$X, lat = sel_place$Y, zoom = 50)
    }
  })
}
shinyApp(ui, server)
