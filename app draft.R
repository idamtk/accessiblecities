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
token <- "pk.eyJ1IjoiaWRhbXRrIiwiYSI6ImNsdGUycjB3dTBhbmkyaXJ0bnh4dGt6MGwifQ.rfH9mhRsZwCVuTXz34PiSA"
library(mapview)
library(mapboxapi)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = names(wheels)),
  mainPanel(
    leafletOutput(outputId = "map", width = "100%", height = 1000)
  )
)

server <- function(input, output, session) {

  
  
    
  output$map <- renderLeaflet({
    wheels_data <- wheels[[input$dataset]]
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
}
shinyApp(ui, server)
