#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(stringr)
install.packages("htmltools")
library(htmltools)
wheels_data_cph <- readr::read_rds("../data_processing/cph_map_data.rds")
wheels_data_aar <- readr::read_rds("../data_processing/aarhus_map_data.rds")
wheels <- list(chp=wheels_data_cph,aar=wheels_data_aar)
amenities <- c("All", "parking", "restaurant")
token <- "pk.eyJ1IjoiaWRhbXRrIiwiYSI6ImNsdGUycjB3dTBhbmkyaXJ0bnh4dGt6MGwifQ.rfH9mhRsZwCVuTXz34PiSA"
library(bslib)
library(mapview)
library(mapboxapi)
library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
install.packages("shinyalert")
library(shinyalert)

ui <- fluidPage(
  navbarPage("Accessible Aarhus", id="nav",
    tabPanel("Udforsk",
            fluidRow(
              # Column with map
              column(9,leafletOutput(outputId = "map")),
              column(3, style='padding-left:0px; padding-right:50px; padding-top:5px; padding-bottom:5px',textInput("place", label = "place",
                                                                                                                    placeholder = "Indtast en addresse eller et navn på et sted"),
                     actionButton("action", "Søg"),
                     selectInput("dataset", label = "Dataset", choices = names(wheels)),
                     selectInput("amenity", label = "Amenity", choices = amenities)),
                     actionButton("am_but", "Søg")
    
  )),
  tabPanel("Sammenlign", 
            print("Hvad"))))

server <- function(input, output, session) {
  

  
  
    
  output$map <- renderLeaflet({
      if (input$amenity=="All"){
        wheels_data <- wheels[[input$dataset]]
      } 
      else {
        wheels_data <- wheels[[input$dataset]]
        wheels_data_filtered <- dplyr::filter(wheels_data, wheelchair=="yes" & amenity==input$amenity)
        if (nrow(wheels_data_filtered)==0){
          shinyalert("No data available!", "There are no accessible locations in this category", type = "error")
          
        }
        if (nrow(wheels_data_filtered)>0){
          wheels_data<-wheels_data_filtered
        }
      
    }  
    
    icons <- awesomeIcons(
      icon = 'fa-solid fa-wheelchair',
      library = 'fa',
      markerColor = wheels_data$color,
      )
    leaflet() %>%
      addTiles() %>% addProviderTiles("CartoDB.Voyager")%>%
      addAwesomeMarkers(data=wheels_data, lat=~lat, lng=~lng,
                 icon=icons, clusterOptions = markerClusterOptions(),
                 popup=~paste("<div style='text-align:center;'><b>",name,"</b> <br>",oc_formatted,"</div>"))
    #, popup = ~placename)
  })
  selected_place <- reactive({
    
    req(input$place)  # Ensure input is available
    wheels_data <- wheels[[input$dataset]]
    wheels_data %>% dplyr::filter(name == input$place) ## add address if poss
  })
  observe({
    sel_place <- selected_place()
    
    if (nrow(sel_place) ==1) {
      leafletProxy("map") %>%
        setView(lng = sel_place$lng, lat = sel_place$lat, zoom = 50)
    }
    if (nrow(sel_place) > 1) {
      result <- sel_place[1,]
      leafletProxy("map") %>%
        setView(lng = result$lng, lat = result$lat, zoom = 50)
    }
    if (is.null(sel_place)) {
      shinyalert("No data available!", "There are no accessible locations in this category", type = "error")
    }
  })
}
shinyApp(ui, server)

