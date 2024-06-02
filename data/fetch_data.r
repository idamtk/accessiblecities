# load required packages
library(raster)
library(sf)
library(osmdata)
library(mapview)
library(tidyverse)

# load polygons for muncipalities in DK
municipalities_dk <- getData("GADM", country = "DNK", level = 2)
## if this doesn't work -> load the same file locally
## source: https://gadm.org/download_country36.html
municipalities_dk <- readr::read_rds("gadm36_DNK_2_sp.rds")

# convert to spatial dataframe
municipalities_dk <- as(municipalities_dk, "sf")

# function that fetches the data for a city and cleans it
get_kommune <- function(polygons,city_name){
  # filter for the chosen city
  city <- polygons %>% dplyr::filter(NAME_2==city_name)
  # set the right CRS
  city_4326 <- city%>% st_transform(4326)
  # save polygon for use in the analysis script.
  st_write(city_4326, paste0("polygons/",city_name,"_poly.shp"),fileEncoding="WINDOWS-1252")
  
  # set bounding box for osm features
  bb<- city_4326 %>% st_bbox()
  osmboundingbox <- opq(bbox = bb,timeout = 180)
  # add locations that where wheelchair accessibility has been tagged
  yes  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "yes")
  no  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "no")
  limited <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "limited")
  # combine into one object
  wheel <- c(osmdata_sf(yes),osmdata_sf(no),osmdata_sf(limited))
  # filter out duplicates
  wheel_unique <- unique_osmdata(wheel)
  # keep only the points where both a name and wheelchair tag is available
  wheel_points <- wheel_unique$osm_points %>% 
    filter(!is.na(wheelchair)) %>% filter (!is.na(name)) %>%
    # only keep the following three columns
    dplyr::select(name,amenity,wheelchair) 
  # set right crs for points
  wheel_points_4326 <- wheel_points %>% st_transform(4326)
  # make sure all the points are within the polygon
  wheels <- st_intersection(wheel_points_4326, city_4326 %>% st_geometry() %>% st_union())
  # extract the coordinates from the geometry column and save in two separate columns
  wheels_unpacked<-st_coordinates(wheels)
  wheels$lng<-wheels_unpacked[,1]
  wheels$lat<-wheels_unpacked[,2]
  # save the object for use in the app
  st_write(wheels, paste0("points/",city_name,"_wheels.shp"),fileEncoding="WINDOWS-1252")
}
# get data for Aarhus
get_kommune(municipalities_dk,"Århus")
# get data for Copenhagen
get_kommune(municipalities_dk,"København")

#download bundesländer data
bundesland_deu <- getData("GADM", country = "DEU", level = 1)

## if this doesn't work -> load the same file locally
## source: https://gadm.org/download_country36.html
bundesland_deu <- readr::read_rds("gadm36_DEU_1_sp.rds")
bundesland_deu <- as(bundesland_deu, "sf")

# i create a function almost identical to the one above, designed for the german data
# function that fetches the data for a city and cleans it
get_bundesland <- function(polygons,bl_name){
  # filter for the chosen city
  bundesland <- polygons %>% dplyr::filter(NAME_1==bl_name)
  # set the right CRS
  bundesland_4326 <- bundesland%>% st_transform(4326)
  # save polygon for use in the analysis script.
  st_write(bundesland_4326, paste0("polygons/",bl_name,"_poly.shp"))
  
  # set bounding box for osm features
  bb<- bundesland_4326 %>% st_bbox()
  osmboundingbox <- opq(bbox = bb,timeout = 180)
  # add locations that where wheelchair accessibility has been tagged
  yes  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "yes")
  no  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "no")
  limited <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "limited")
  # combine into one object
  wheel <- c(osmdata_sf(yes),osmdata_sf(no),osmdata_sf(limited))
  # filter out duplicates
  wheel_unique <- unique_osmdata(wheel)
  # keep only the points where both a name and wheelchair tag is available
  wheel_points <- wheel_unique$osm_points %>% 
    filter(!is.na(wheelchair)) %>% filter (!is.na(name)) %>%
    # only keep the following three columns
    dplyr::select(name,amenity,wheelchair) 
  # set right crs for points
  wheel_points_4326 <- wheel_points %>% st_transform(4326)
  # make sure all the points are within the polygon
  wheels <- st_intersection(wheel_points_4326, bundesland_4326 %>% st_geometry() %>% st_union())
  # extract the coordinates from the geometry column and save in two separate columns
  wheels_unpacked<-st_coordinates(wheels)
  wheels$lng<-wheels_unpacked[,1]
  wheels$lat<-wheels_unpacked[,2]
  # save the object for use in the app
  st_write(wheels, paste0("points/",bl_name,"_wheels.shp"),fileEncoding="WINDOWS-1252")
}

# get the data for Bremen, Berlin and Hamburg
get_bundesland(bundesland_deu,"Bremen")
get_bundesland(bundesland_deu,"Berlin")
get_bundesland(bundesland_deu,"Hamburg")

# load data for all postal codes in denmark
url <- "https://api.dataforsyningen.dk/postnumre?format=geojson"
geofile<- tempfile()
download.file(url,geofile)
postal_areas<-st_read(geofile)
# filter for Aarhus C
aar_c<-postal_areas%>%dplyr::filter(nr==8000)
# change CRS
aar_c_4326 <- aar_c%>% st_transform(4326)
# save polygon for later use
st_write(aar_c_4326,"polygons/aarhus_c_poly.shp")
# read in all the point data from all of Aarhus kommune
aar_points<- st_read("points/Århus_wheels.shp")
# find only the points within Århus C
aar_c_points <-st_intersection(aar_points, aar_c_4326 %>% st_geometry() %>% st_union())
# save file
st_write(aar_c_points,"points/aar_c_wheels.shp",fileEncoding="WINDOWS-1252")
