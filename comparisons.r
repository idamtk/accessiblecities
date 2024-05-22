wheels_data_cph <- readr::read_rds("cph_wheels.rds")
wheels_data_aar <- readr::read_rds
aar_poly <- readr::read_rds("aarhus_poly.rds")
cph_poly <- readr::read_rds("cph_poly.rds")
library(tidyverse)
aar<- wheels_data_aar%>%distinct(geometry, .keep_all=TRUE)
munic <- getData("GADM", country = "DNK", level = 2)
munic <- as(munic, "sf")
cph<-munic[25,]
cph_poly<- cph %>% st_transform(4326)
library(sf)
st_area(aar_poly)/1e+06

density_calc <- function (data,poly){
  area<-st_area(poly)/1*1e+06
  data_yes <- dplyr::filter(data, wheelchair=="yes")
  amount_acc_places<- nrow(data_yes)
  density <- amount_acc_places/area
  print("There are the following number of accessible locations per km2")
  print(density)
}
density_calc(wheels_data_aar,aar_poly)
density_calc(wheels_data_cph,cph_poly)

library(raster)
library(spdep)
aar_df<- data.frame(geometry=aar$geometry,
                    nn=near$nn)
near<-knearneigh(aar, k = 1)
aar_df$nn_geo <- aar_df[aar_df$nn,"geometry"]
st_distance(aar_df[50,"geometry"],aar_df[])
aar_df$distance <- diag(st_distance(aar_df$geometry,aar_df$nn_geo))
aar[59,"geometry"]
aar_df[50,"distance"]
mean(aar_df$distance)

avg_distance <- function(data){
  data_df <- dplyr::filter(data, wheelchair=="yes")
  data_df <- data_df %>% distinct(geometry, .keep_all=TRUE)
  near_data <- knearneigh(data_df, k = 1)
  near_df<- data.frame(geometry=data_df$geometry,
                      nn=near_data$nn)
  near_df$nn_geo <- near_df[near_df$nn,"geometry"]
  near_df$distance <- diag(st_distance(near_df$geometry,near_df$nn_geo))
  avg_dist<-mean(near_df$distance)
  print(avg_dist)
  
}
avg_distance(wheels_data_cph)
avg_distance(wheels_data_aar)
