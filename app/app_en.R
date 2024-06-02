# load packages
library(tidyverse)
library(stringr)
library(htmltools)
library(sf)
library(bslib)
library(shiny)
library(shinyalert)
library(leaflet)
library(ggplot2)

# load English data
wheels_all <- st_read("../data_processing/map_data_en.shp")
# load metrics file for comparison page
metrics<-read.csv("../analysis/metrics.csv", fileEncoding = "WINDOWS-1252")
# change the city name for Copenhagen to the English version so this version will be displayed on the plots
metrics[metrics$X == "København", "X"] <- "Copenhagen"
# make a list of cities that can be viewed on the map
wheel <- list("cph","aar")
# filter dataset for accessible locations
all_yes<- wheels_all%>% filter(whelchr=="yes")
# make list of categories with accessible locations
cat_all <- unique(all_yes$cat_en)
# turn NA into a different value on the list
cat_all[1] <- "Choose/show all"

# create user interface for shiny app
ui <- fluidPage(
  # use theme
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  
  tags$head(
    # link to external icons
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css"),
    # custom CSS that centers that buttons for the tabs on the page
    tags$style(HTML("
      .nav .nav-item .nav-link {
        padding-left: 20px;
        padding-right: 20px;
        text-align: center; 
        width: 200px;
      }
      .navbar-nav {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-left: -100px;
      }
    "))
  ),
  
  navbarPage("Accessible Cities", id = "nav",
    # crete tab    
    tabPanel("Explore",
      fluidPage(
        # add css to adjust size of map
        tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
        
        fluidRow(
          # column with map
          column(9, leafletOutput(outputId = "map")),
          # column with search functions
          column(3,
                 # buttons for choosing city
                 h5("Accessible places in:", style = "font-weight: bold;"),
                 actionButton("aar", "Aarhus"),
                 actionButton("cph", "Copenhagen"),
                 tags$div(style = "height: 10px;"), 
                 p("Find wheelchair accessible places in these cities by using the map or the search tools below"),
                 hr(),
                 
                 # dropdown menu for choosing a category
                 p("Find accessible places in this category", style = "font-weight: bold;"),
                 selectInput("amenity", label = NULL, choices = cat_all),
                 hr(),
                 
                 # search bar for finding specific places
                 p("Search for the name or address of a place to see if it is accessible", style = "font-weight: bold;"),
                 div(style = "display: flex; justify-content: center; gap: 10px;",
                 textInput("place", label = NULL, placeholder = "Write here"),
                 tags$style(HTML("#action { height: 38px; }")),
                 actionButton("action", "Search")
            )
          )
        )
      )
    ),
    
    # create tab for comparing cities
    tabPanel("Compare", 
      fluidRow(
        # title and description
        h5("Choose cities to compare their accessibility", style = "font-weight: bold; text-align:center;"),
        p("Here you can use our comparison tool, which compares cities based on three metrics: average distance to nearest accessible location, accessible locations pr. km² og the share of accessible locations.", style = "text-align:center;"),
        
        # create six buttons in one centered row
        div(style = "display: flex; justify-content: center; gap: 10px;",
          actionButton("aar2", "Aarhus"),
          actionButton("cph2", "Copenhagen"),
          actionButton("ber", "Berlin"),
          actionButton("bre", "Bremen"),
          actionButton("ham", "Hamburg"),
          actionButton("clear", "Clear all")
        ),
        tags$div(style = "height: 20px;"), 
        hr()
      ),
      # create new section with 3 columns with their own plot
      fluidRow(
        
        # column for average distance to the nearest accessible place
        column(4,
               p("Average distance", style = "text-align:center;"),
               # plot bi-directional arrows icon
               HTML("<i class='bi bi-arrow-left-right' style='font-size: 50px; color: #009CDE; text-align: center; display: block;'></i>"),
               # show plot with average distance
               plotOutput("distance")
        ),
        # column for share of accessible places
        column(4,
               p("Share of accessible places", style = "text-align:center;"),
               # pie chart icon
               HTML("<i class='bi bi-pie-chart-fill' style='font-size: 50px; color: #009CDE; text-align: center; display: block;'></i>"),
               # show plot with ratio
               plotOutput("ratio")
        ),
        # column with density
        column(4,
               p("Accessible places pr. km²", style = "text-align:center;"),
               # plot three pin icons
               div(style = "text-align:center;",
                   HTML("<i class='bi bi-geo-fill' style='font-size: 50px; color: #009CDE;'></i>"),
                   HTML("<i class='bi bi-geo-fill' style='font-size: 50px; color: #009CDE;'></i>"),
                   HTML("<i class='bi bi-geo-fill' style='font-size: 50px; color: #009CDE;'></i>")
                   ),
               # show plot with density
               plotOutput("density")
        )
      )
    )
  )
)

# create back end
server <- function(input, output, session) {
  # create a reactive value for the data that is displayed - default is all data
  wheels <- reactiveValues(data = wheels_all)
  
  # if the "Aarhus" button is clicked only display Aarhus data
  observeEvent(input$aar, {wheels$data <- wheels_all %>% dplyr::filter(city=="Aarhus")})
  
  # if the "København" button is clicked only display Copenhagen
  observeEvent(input$cph, {wheels$data <- wheels_all %>% dplyr::filter(city=="Copenhagen")})
  
  
  
  # create output map  
  output$map <- renderLeaflet({
    # if no category is chosen show all data
      if (input$amenity=="Choose/show all"){wheels_data <- wheels$data} 
    # if a category is chosen, only display accessible locations in that category
      else {
        wheels_data <- wheels$data
        wheels_data_filtered <- dplyr::filter(wheels_data, whelchr=="yes" & cat_en==input$amenity)
        # if there are no accessible locations in that category then inform the user about this
        if (nrow(wheels_data_filtered)==0){
          shinyalert("No places found", "There are no accessible locations in this category for this area", type = "info")
        }
        # if there are accessible locations in this category then display that data
        if (nrow(wheels_data_filtered)>0) {wheels_data <- wheels_data_filtered}
    }  
    
    # use icons with a wheelchair symbol
    icons <- awesomeIcons(
      icon = 'fa-solid fa-wheelchair',
      library = 'fa',
      markerColor = wheels_data$color,
      )
    # create leaflet map with a base group and a marker group
    leaflet() %>%
      addTiles(group="Base") %>% addProviderTiles("CartoDB.Voyager",group="Base")%>%
      addAwesomeMarkers(data=wheels_data, lat=~lat, lng=~lng,
                 icon=icons, clusterOptions = markerClusterOptions(spiderfyOnMaxZoom=FALSE, disableClusteringAtZoom=17), group="All_markers",
                 popup=~paste("<div style='text-align:center;'><b>",name,"</b> <br>",oc_frmt,"</div>")) %>%
      addLegend(colors = unique(wheels_data$color), labels=unique(wheels_data$label_n), title="Wheelchair accessibilty <br> for the location", opacity=1, group="Base")
  })
  
  # create reactive event for searching for specific places once the search button is pressed
  selected_place <- eventReactive(input$action, {
    # if there is input then find any cell where name or address contains the string
    req(input$place)
    wheels_data <- wheels$data
    wheels_data %>% dplyr::filter(str_detect(name, regex(input$place, ignore_case = TRUE)) |
                                    str_detect(oc_frmt, regex(input$place, ignore_case = TRUE))) 
  })
  observe({
    sel_place <- selected_place()
    
    # if there is one cell matching the search then zoom in on that one point
    if (nrow(sel_place) ==1) {
      leafletProxy("map") %>%
        setView(lng = sel_place$lng, lat = sel_place$lat, zoom = 70)
    }
    
    # if there is more than one place matching the search then display only these places and display info message
    if (nrow(sel_place) > 1) {
      # display info message
      shinyalert("More than one match","Several places match your search. The map displays all matches.", type="info")
      
      # create icons
      iconss <- awesomeIcons(
        icon = 'fa-solid fa-wheelchair',
        library = 'fa',
        markerColor = sel_place$color,
      )
      # clear all markers and markers from previous searches from the map and add only markers that match this search
      leafletProxy("map") %>% clearGroup("Search_markers")%>% hideGroup("All_markers") %>% addAwesomeMarkers(data=sel_place, lat=~lat, lng=~lng,
                                                                             icon=iconss, group="Search_markers",
                                                                             popup=~paste("<div style='text-align:center;'><b>",name,"</b> <br>",oc_frmt,"</div>"))
    }
    
    # if no cells match the search then inform the user of this
    if (nrow(sel_place) == 0) {
      shinyalert("No place found", "There is no place with the entered name/address in our database.", type = "info")
    }
  })
  
  # create reactive value for data displayed on comparison tap
  sel_metrics <- reactiveValues( data=c())
  # if a button for a city is pressed then add their data a list that will be used to filter the data
  observeEvent(input$aar2, {
    sel_metrics$data <- c(sel_metrics$data, "Aarhus")
  })
  observeEvent(input$cph2, {
    sel_metrics$data <- c(sel_metrics$data, "Copenhagen")
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
  # if the "Clear" button is pressed them clear the list of data for the graphs
  observeEvent(input$clear, {
    sel_metrics$data <- c()
  })
  # filter data so only the selected cities are displayed
  plot_data<- reactive({ dplyr::filter(metrics, X %in% sel_metrics$data)})
  
  # plot average distance to nearest accessible place for each city
  output$distance <- renderPlot({
    # if data has been selected then plot it
    if (nrow(plot_data()) > 0) {
      ggplot(data = plot_data(), aes(x = X, y = average_distance)) +
        geom_bar(stat = "identity", fill = "#009CDE") +
        ylab("Average distance to nearest accessible location in m") +
        xlab("Area") +
      ylim(0, max(metrics$average_distance)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } 
    # if no data has been selected then display nothing
    else {
      ggplot() + 
        theme_void()
    }
    
  })
  
  # plot ratio of accessible locations
  output$ratio <- renderPlot({
    # if cities have been selected then plot the data for those cities
    if (nrow(plot_data()) > 0) {
      ggplot(data = plot_data(), aes(x = X, y = ratio)) +
        geom_bar(stat = "identity", fill = "#009CDE") +
        ylab("Share accessible places") +
        xlab("Area")+
        ylim(0, max(metrics$ratio)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } 
    # if no city has been selected, display nothing
    else {
      ggplot() + 
        theme_void()
    }
    
  })
  
  # plot density 
  output$density <- renderPlot({
    # if one or more cities have been chosen then display their data
    if (nrow(plot_data()) > 0) {
      ggplot(data = plot_data(), aes(x = X, y = density)) +
        geom_bar(stat = "identity", fill = "#009CDE") +
        ylab("Accessible places pr. km²") +
        xlab("Area") +
        ylim(0, max(metrics$density)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } 
    # if not, display nothing
    else {
      ggplot() + 
        theme_void()
    }
    
  })
  
}
#run app
shinyApp(ui, server)

