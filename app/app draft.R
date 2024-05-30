#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

wheels_data_aar <- wheels_data_aar  %>%
  mutate(cat_da = case_when(
    amenity == "fuel" ~ "Benzintank",
    amenity == "post_office" ~ "Posthus",
    amenity == "cinema" ~ "Biograf",
    amenity == "fast_food" ~ "Restaurant",
    amenity == "nightclub" ~ "Natklub",
    amenity == "bar" ~ "Bar",
    amenity == "cafe" ~ "Café",
    amenity == "pharmacy" ~ "Apotek",
    amenity == "theatre" ~ "teater",
    amenity == "clinic" ~ "Læge",
    amenity == "school" ~ "Skole",
    amenity == "restaurant" ~ "Restaurant",
    amenity == "library" ~ "Bibliotek",
    amenity == "bank" ~ "Bank",
    amenity == "pub" ~ "Bar",
    amenity == "hookah_lounge" ~ "Vandpibecafé",
    amenity == "social_facility" ~ "Sociale ydelser",
    amenity == "ice_cream" ~ "Iskiosk",
    amenity == "bureau_de_change" ~ "Udvekslingsbureau",
    amenity == "doctors" ~ "Læge"
    ))

wheels_data_cph <- wheels_data_cph  %>%
  mutate(label_da = case_when(
    wheelchair == "yes" ~ "Tilgængelig",
    wheelchair == "limited" ~ "Begrænset tilgængelighed",
    wheelchair == "no" ~ "Utilgængelig"
  ))
wheels_data_cph <- wheels_data_cph  %>%
  mutate(cat_da = case_when(
    amenity == "resturant" ~ "Restaurant",
    amenity == "fuel" ~ "Benzintank",
    amenity == "library" ~ "Bibliotek",
    amenity == "cinema" ~ "Biograf",
    amenity == "theatre" ~ "Teater",
    amenity == "cafe" ~ "Café",
    amenity == "pub" ~ "Bar",
    amenity == "fast_food" ~ "Restaurant",
    amenity == "bank" ~ "Bank",
    amenity == "car_rental" ~ "Biludlejning",
    amenity == "bureau_de_change" ~ "Udvekslingsbureau",
    amenity == "pharmacy" ~ "Apotek",
    amenity == "nightclub" ~ "Natklub",
    amenity == "ferry_terminal" ~ "Færge",
    amenity == "school" ~ "Skole",
    amenity == "parking_entrance" ~ "Parkering",
    amenity == "bar" ~ "Bar",
    amenity == "bicycle_rental" ~ "Cykelludlejning",
    amenity == "music_venue" ~ "Koncerthus",
    amenity == "dentist" ~ "Tandlæge",
    amenity == "police" ~ "Politistation",
    amenity == "toilets" ~ "Toilet",
    amenity == "doctors" ~ "Læge",
    amenity == "recycling" ~ "Genbrugsstation",
    amenity == "veterinary" ~ "Dyrlæge",
    amenity == "parcel_locker" ~ "Pakkeboks",
    amenity == "post_office" ~ "Posthus",
    amenity == "ice_cream" ~ "Iskiosk",
    amenity == "bus_station" ~ "Busstation",
    amenity == "community_centre" ~ "Kulturhus",
    amenity == "car_wash" ~ "Bilvask",
    amenity == "driving_school" ~ "Køreskole",
    amenity == "clinic" ~ "Læge",
    amenity == "photo" ~ "Fotografering",
    amenity == "stripclub" ~ "Stripklub",
    amenity == "dancing_school" ~ "Danse undervisning",
    amenity == "brothel" ~ "Bordel",
    amenity == "coworking_space" ~ "Kontorfællesskab",
    amenity == "social_facility" ~ "Sociale initiativer",
    amenity == "events_venue" ~ "Mødested",
    amenity == "studio" ~ "Studio", ## this category is heterogeneous
    amenity == "shelter" ~ "Shelter",
    amenity == "language_school" ~ "Sprogskole",
    amenity == "place_of_worship" ~ "Trossamfund",
    amenity == "casino" ~ "Kasino",
    amenity == "kindergarten" ~ "Børnehave",
    amenity == "hookah_lounge" ~ "Vandpibecafé",
  ))
setwd("~/OneDrive/Skrivebord/Sjette semester/Spatial Analytics/exam/accessibleaarhus/app")
library(stringr)
install.packages("htmltools")
library(htmltools)
wheels_data_cph <- readr::read_rds("../data_processing/cph_map_data.rds")
wheels_data_aar <- readr::read_rds("../data_processing/aarhus_map_data.rds")
metrics<-read.csv("../analysis/metrics.csv",fileEncoding = "WINDOWS-1252")
wheel <- list(cph=wheels_data_cph,aar=wheels_data_aar)
aar_yes<- wheels_data_aar%>% filter(wheelchair=="yes")
cat_aar <- unique(wheels_data_aar$cat_da)
cat_aar <- cat_aar[-1]
cat_cph <- unique(wheels_data_cph$cat_da)
cat_cph[1] <- "Vælg"
categories<-union(cat_cph,cat_aar)
amenities <- c("Vælg", "parking", "restaurant")
token <- "pk.eyJ1IjoiaWRhbXRrIiwiYSI6ImNsdGUycjB3dTBhbmkyaXJ0bnh4dGt6MGwifQ.rfH9mhRsZwCVuTXz34PiSA"
library(bslib)
library(purrr)
library(mapview)
library(mapboxapi)
library(shiny)
library(leaflet)

library(sf)
library(ggplot2)
install.packages("shinyalert")

library(shinyalert)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  tags$head(
    # Include FontAwesome library
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css")
  ),
  tags$head(
    tags$style(HTML("
      .nav .nav-item .nav-link {
        padding-left: 20px;
        padding-right: 20px;
        text-align: center; 
        width: 200px;  /* Adjust the width as needed */
      }
    ")),
    tags$head(
      tags$style(HTML("
      .navbar-nav {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-left: -100px; 
      }
    "))
    ),
  ),
  navbarPage("Accessible Cities", id="nav",
    tabPanel("  Udforsk  ",
             fluidPage(
               tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}",),
            fluidRow(
              # Column with map
              column(9,leafletOutput(outputId = "map")),
              column(3,
                     h5("Find tilgængelige steder i:", style = "font-weight: bold;"),
                     actionButton("aar", "Århus"),
                     actionButton("cph", "København"), 
                     tags$div(style = "height: 10px;"), 
                     p("Du kan finde kørestolstilgængelige steder ved at udforske på kortet eller bruge søgeværktøjerne nedenfor"),
                     hr(),
                     selectInput("amenity", label = "Find tilgængelige steder i denne kategori", choices = categories),
                     hr(),
                     textInput("place", label = "Søg på en addresse og find ud af om stedet er tilgængeligt med kørestol",
                               placeholder = "Indtast en addresse eller et navn på et sted"), actionButton("action", "Søg"),
                     
                     
                     
                     
    
  )))),
  tabPanel("Sammenlign", 
           fluidRow(
           h5("Vælg byer og sammenlign deres tilgængelighed", style = "font-weight: bold; text-align:center;"),
           p("Her kan du udforske vores sammenligningsværktøj, der sammenligner byers tilgængelighed på baggrund af tre mål: gennemsnitlig afstand mellem tilgængelige lokationer, antal tilgængelige lokationer pr. km2 og andelen af tilgængelige lokationer.", style="text-align:center;"),
           div(style = "display: flex; justify-content: center; gap: 10px;",
           actionButton("aar2", "   Århus   "),
           actionButton("aar3", "   Århus C   "),
           actionButton("cph2", "   København   "),
           actionButton("ber", "   Berlin   "),
           actionButton("bre", "   Bremen   "),
           actionButton("ham", "   Hamburg   "),
           actionButton("clear", "   Fjern alle   "),
           ),
           tags$div(style = "height: 20px;"), 
           hr()),
           fluidRow(
             column(4,
                    p("Gennemsnitlig afstand",style="text-align:center;"),
                    HTML("<i class='bi bi-arrow-left-right' style='font-size: 50px; color: #009CDE; text-align: center; display: block;'></i>"),
                    plotOutput("distance")
                    ),
             column(4, p("Andel af tilgængelige steder",style="text-align:center;"),
                    HTML("<i class='bi bi-pie-chart-fill' style='font-size: 50px; color: #009CDE; text-align: center; display: block;'></i>"),
                    plotOutput("ratio")
                    ),
             column(4, p("Tilgængelige steder pr. km2",style="text-align:center;"),
                    div(style="text-align:center;",
                    HTML("<i class='bi bi-geo-fill' style='font-size: 50px; color: #009CDE;'></i>"),
                    HTML("<i class='bi bi-geo-fill' style='font-size: 50px; color: #009CDE;'></i>"),
                    HTML("<i class='bi bi-geo-fill' style='font-size: 50px; color: #009CDE;'></i>")),
                    plotOutput("density"))
           ))
  
  
  ))

server <- function(input, output, session) {
  wheels <- reactiveValues(data = wheel[["aar"]])
  observeEvent(input$aar, {
    wheels$data <- wheel[["aar"]]
  })
  observeEvent(input$cph, {
    wheels$data <- wheel[["cph"]]
  })
  
  
  
    
  output$map <- renderLeaflet({
      if (input$amenity=="Vælg"){
        wheels_data <- wheels$data
      } 
      else {
        wheels_data <- wheels$data
        wheels_data_filtered <- dplyr::filter(wheels_data, wheelchair=="yes" & cat_da==input$amenity)
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
                 icon=icons, clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=FALSE, disableClusteringAtZoom=17),
                 popup=~paste("<div style='text-align:center;'><b>",name,"</b> <br>",oc_formatted,"</div>")) %>%
      addLegend(colors = unique(wheels_data$color), labels=unique(wheels_data$label_da), title="Kørestolstilgængelighed <br> for lokationen", opacity=1)
    #, popup = ~placename)
  })
  selected_place <- reactive({
    
    req(input$place)  # Ensure input is available
    wheels_data <- wheels$data
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
      shinyalert("No data available!", "There are no accessible locations in this category in this city", type = "error")
    }
  })
  
  sel_metrics <- reactiveValues( data=c())
  observeEvent(input$aar2, {
    sel_metrics$data <- c(sel_metrics$data, "Aarhus")
  })
  observeEvent(input$aar3, {
    sel_metrics$data <- c(sel_metrics$data, "Aarhus C")
  })
  observeEvent(input$cph2, {
    sel_metrics$data <- c(sel_metrics$data, "København")
  })
  observeEvent(input$ber, {
    sel_metrics$data <- c(sel_metrics$data, "Berlin")
  })
  observeEvent(input$bre, {
    sel_metrics$data <- c(sel_metrics$data, "Bremen")
  })
  observeEvent(input$ham, {
    sel_metrics$data <- c(sel_metrics$data, "Hamburg")
  })
  observeEvent(input$clear, {
    sel_metrics$data <- c()
  })
  
  plot_data<- reactive({ dplyr::filter(metrics, X %in% sel_metrics$data)})
  
  
  output$ratio <- renderPlot({
    
    
    if (nrow(plot_data()) > 0) {
      ggplot(data = plot_data(), aes(x = X, y = ratio)) +
        geom_bar(stat = "identity", fill = "#009CDE") +
        ylab("Andel tilgængelige steder") +
        xlab("Område")+
      ylim(0, max(metrics$ratio)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))# Optional: Add a minimal theme for better aesthetics
    } else {
      ggplot() + 
        theme_void() # Create an empty plot with no axes
    }
    
  })
  output$distance <- renderPlot({
    if (nrow(plot_data()) > 0) {
      ggplot(data = plot_data(), aes(x = X, y = average_distance)) +
        geom_bar(stat = "identity", fill = "#009CDE") +
        ylab("Gennemsnitlig afstand mellem tilgængelige steder i meter") +
        xlab("Område") +
      ylim(0, max(metrics$average_distance)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))# Optional: Add a minimal theme for better aesthetics
    } else {
      ggplot() + 
        theme_void() # Create an empty plot with no axes
    }
    
  })
  
  output$density <- renderPlot({
    if (nrow(plot_data()) > 0) {
      ggplot(data = plot_data(), aes(x = X, y = density)) +
        geom_bar(stat = "identity", fill = "#009CDE") +
        ylab("Tilgængelige steder pr. km2") +
        xlab("Område") +
        ylim(0, max(metrics$density)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))# Optional: Add a minimal theme for better aesthetics
    } else {
      ggplot() + 
        theme_void() # Create an empty plot with no axes
    }
    
  })
  
}
shinyApp(ui, server)

