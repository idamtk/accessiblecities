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

aar_c<-readr::read_rds("../data/aarhus_c.rds")

# convert to spatial dataframe
municipalities_dk <- as(municipalities_dk, "sf")

#download bundesländer data
bundesland_deu <- getData("GADM", country = "DEU", level = 1)

## if this doesn't work -> load the same file locally
## source: https://gadm.org/download_country36.html
bundesland_deu <- readr::read_rds("../data/gadm36_DEU_1_sp.rds")
bundesland_deu <- as(bundesland_deu, "sf")

# function that fetches the data for a city and cleans it
get_kommune <- function(polygons,city_name){
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
  # I saved the unprocessed object for reproducibility
  #saveRDS(wheel,paste0(city_name,".rds"))
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
# get data for ?rhus
get_city(municipalities_dk,"Århus")
# get data for Copenhagen
get_city(municipalities_dk,"København")
# make sure file is saved with windows-1252 encoding

aar_points<- readr::read_rds("aarhus_map_data.rds")
aar_c_points <-st_intersection(aar_points, aar_c %>% st_geometry() %>% st_union())
saveRDS(aar_c_points,"aar_c_data.rds")

# icreate a function almost identical to the one above, designed for the german data
# function that fetches the data for a city and cleans it
get_stadtstaat <- function(polygons,stadt_name){
  # filter for the chosen city
  city <- polygons %>% dplyr::filter(NAME_1==stadt_name)
  # set the right CRS
  city_4326 <- city%>% st_transform(4326)
  # save polygon for use in the analysis script.
  saveRDS(city_4326, paste0(stadt_name,"_poly.rds"))
  
  # set bounding box for osm features
  bb<- city_4326 %>% st_bbox()
  osmboundingbox <- opq(bbox = bb,timeout = 180)
  # add locations that where wheelchair accessibility has been tagged
  yes  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "yes")
  no  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "no")
  limited <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = "limited")
  # combine into one object
  wheel <- c(osmdata_sf(yes),osmdata_sf(no),osmdata_sf(limited))
  # I saved the unprocessed object for reproducibility
  #saveRDS(wheel,paste0(city_name,".rds"))
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
  saveRDS(wheels, paste0(stadt_name,"_wheels.rds"))
}
get_stadtstaat(bundesland_deu,"Bremen")
get_stadtstaat(bundesland_deu,"Berlin")
get_stadtstaat(bundesland_deu,"Hamburg")
