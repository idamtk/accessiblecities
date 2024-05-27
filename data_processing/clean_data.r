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
municipalities_dk <- readr::read_rds("../data/gadm36_DNK_2_sp.rds")

# convert to spatial dataframe
municipalities_dk <- as(municipalities_dk, "sf")

# function that fetches the data for a city and cleans it
get_city <- function(polygons,city_name){
  # filter for the chosen city
  city <- polygons %>% dplyr::filter(NAME_2==city_name)
  # set the right CRS
  city_4326 <- city%>% st_transform(4326)
  # save polygon for use in the analysis script.
  saveRDS(city_4326, paste0(city_name,"_poly.rds"))
  
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
  saveRDS(wheels, paste0(city_name,"_wheels.rds"))
}
# get data for Århus
get_city(municipalities_dk,"Århus")
# get data for Copenhagen
get_city(municipalities_dk,"København")
