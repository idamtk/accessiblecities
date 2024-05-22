library(raster)
library(sf)
library(osmdata)
library(mapview)
library(tidyverse)
munic <- getData("GADM", country = "DNK", level = 2)
munic <- as(munic, "sf")
århus<-munic[31,]
save_århus <- århus %>% st_transform(4326)
saveRDS(save_århus, "aarhus_poly.rds")
bb  <- århus %>% st_transform(4326) %>% st_bbox()
plot(bb)
osmboundingbox <- opq(bbox = bb,timeout = 180)

plot_category <- function(type,colorx){
  feature  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = type)
  wheel <- c(osmdata_sf(feature))
  wheel_uniq <- unique_osmdata(wheel)
  rpoint <- wheel_uniq$osm_points %>% 
    filter(!is.na(wheelchair)) %>% 
    st_transform(32632) %>%
    dplyr::select(name,amenity,wheelchair) 
  rpoly  <- wheel_uniq$osm_polygons %>% 
    st_transform(32632) %>% 
    dplyr::select(name,amenity,wheelchair)  %>% st_centroid()
  wheel_osm <- rbind(rpoly,rpoint)
  
  wheel_osm$color <- colorx
  return(wheel_osm)

}

map_yes<-plot_category("yes","green")

map_no<-plot_category("no","red")
map_limited<-plot_category("limited","orange")

wheels<- rbind(map_yes,map_no,map_limited)

wheels <- st_intersection(wheels, st_transform(århus, 32632) %>% st_geometry() %>% st_union())
wheels_data<- wheels %>% st_transform(4326)
wheels_unpacked<-st_coordinates(wheels_data)
wheels_data$X<-wheels_unpacked[,1]
wheels_data$Y<-wheels_unpacked[,2]
saveRDS(wheels_data,"aarhus_wheels.rds")

library(mapview)

map_osm <-  mapview(wheels, map.types = "OpenStreetMap", 
                    col.regions = wheels$color, 
                    color = "white", legend = FALSE, layer.name = "Baths in OSM",
                    homebutton = FALSE, lwd = 0.5) 
map_osm
