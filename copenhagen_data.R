library(osmdata)
library(dplyr)
suburbs <- st_read("cds-spatial/data/bydel.shp", options = "ENCODING=WINDOWS-1252")
cph <- suburbs %>% st_transform(4326)
saveRDS(cph,"cph_poly.rds")
bb  <- suburbs %>% st_transform(4326) %>% st_bbox()
plot(bb)
osmboundingbox <- opq(bbox = bb,timeout = 180)

plot_category <- function(type,colorx){
  qa  <- add_osm_feature(osmboundingbox, key = 'wheelchair',value = type)
  wheel <- c(osmdata_sf(qa))
  wheel_uniq <- unique_osmdata(wheel)
  rpoint <- wheel_uniq$osm_points %>% 
    filter(!is.na(wheelchair)) %>% 
    st_transform(32632) %>%
    dplyr::select(name,amenity,wheelchair) 
  rpoly  <- wheel_uniq$osm_polygons %>% 
    st_transform(32632) %>% 
    dplyr::select(name,amenity,wheelchair)  %>% st_centroid()
  baths_osm <- rbind(rpoly,rpoint)
  baths_osm$color <- colorx
  return(baths_osm)

}

map_yes=plot_category("yes","green")

map_no=plot_category("no","red")
map_limited=plot_category("limited","orange")

wheels<- rbind(map_yes,map_no,map_limited)

wheels <- st_intersection(wheels, st_transform(suburbs, 32632) %>% st_geometry() %>% st_union())
wheels_data<- wheels %>% st_transform(4326)
wheels_unpacked<-st_coordinates(wheels_data)

wheels_data$X<-wheels_unpacked[,1]
wheels_data$Y<-wheels_unpacked[,2]
saveRDS(wheels_data,"cph_wheels.rds")
library(leafsync)
sync(map_yes,map_no,map_limited)

qa  <- add_osm_feature(q, key = 'wheelchair',value = 'no')
qb  <- add_osm_feature(q, key = 'wheelchair',value = 'limited')
qc  <- add_osm_feature(q, key = 'wheelchair',value = 'yes')
library(osmdata)
wheel <- c(osmdata_sf(qa),
                 osmdata_sf(qb),
                 osmdata_sf(qc))
wheel_uniq <- unique_osmdata(wheel)
rpoint <- wheel_uniq$osm_points %>% 
  filter(!is.na(amenity)) %>% 
  st_transform(32632) %>%
  dplyr::select(name) 
rpoly  <- wheel_uniq$osm_polygons %>% 
  st_transform(32632) %>% 
  dplyr::select(name)  %>% st_centroid()

baths_osm <- rbind(rpoly,rpoint) 
baths_osm <- st_intersection(baths_osm, st_transform(suburbs, 32632) %>% st_geometry() %>% st_union())
library(mapview)
# library(leafsync)
# library(leaflet)
map_osm <-  mapview(wheels, map.types = "OpenStreetMap", 
                    col.regions = wheels$color, 
                    label = as.character(suburbs$name), 
                    color = "white", legend = FALSE, layer.name = "Baths in OSM",
                    homebutton = FALSE, lwd = 0.5) 
map_osm
