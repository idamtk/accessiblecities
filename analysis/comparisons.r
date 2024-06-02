# load packages
library(tidyverse)
library(sf)
library(raster)
library(spdep)

# load point data
wheels_data_cph <- st_read("../data/points/København_wheels.shp")
wheels_data_aar <- st_read("../data/points/Århus_wheels.shp")
wheels_data_ber <- st_read("../data/points//Berlin_wheels.shp")
wheels_data_ham <- st_read("../data/points/Hamburg_wheels.shp")
wheels_data_bre <- st_read("../data/points/Bremen_wheels.shp")
wheels_data_aar_c <- st_read("../data/points/aar_c_wheels.shp")
# load polygon data
aar_poly <- st_read("../data/polygons/Århus_poly.shp")
aar_c_poly <- st_read("../data/polygons/aarhus_c_poly.shp")
cph_poly <- st_read("../data/polygons/København_poly.shp")
ber_poly <- st_read("../data/polygons/Hamburg_poly.shp")
bre_poly <- st_read("../data/polygons/Bremen_poly.shp")

# create function for calculating accessible points pr. km^2 
density_calc <- function (data,poly){
  # calculate area of polygon and convert to square km
  area<-st_area(poly)/1*1e+06
  # only select accessible points
  data_yes <- dplyr::filter(data, wheelchair=="yes")
  # get amount of accessible points
  amount_acc_places<- nrow(data_yes)
  # divide amount with area to get density
  density <- amount_acc_places/area
}

# function for calculating average distance between nearest accessible points
avg_distance <- function(data){
  # only use accessible locations
  data_yes <- dplyr::filter(data, wheelchair=="yes")
  # find the nearest neighbour for each line and add to a new dataframe
  near_data <- knearneigh(data_yes, k = 1)
  near_df<- data.frame(geometry=data_yes$geometry,
                      nn=near_data$nn)
  # add the geometry for the nearest neighbor
  near_df$nn_geo <- near_df[near_df$nn,"geometry"]
  # calculate the distance for the nearest neighbor for each point
  near_df$distance <- diag(st_distance(near_df$geometry,near_df$nn_geo))
  # calculate the mean to get the average
  avg_dist<-mean(near_df$distance)
}

# calculate ratio of accessible points
ratio_cal <- function(data){
  # get the total amount of points
  total <- nrow(data)
  # get the amount of accessible points
  accessible <- data%>%dplyr::filter(wheelchair=="yes")%>%nrow()
  # find the percentage of accessible points
  ratio <- accessible/total*100
}

# create function that includes the above functions
get_metrics <- function(data,polygon){
  density <- density_calc(data,polygon)
  ratio <- ratio_cal(data)
  mean <- avg_distance(data)
  results <- data.frame(density=density,
                        ratio=ratio,
                        average_distance=mean)
}
# get the results for each city
aar_results<-get_metrics(wheels_data_aar,aar_poly)
aar_c_results<-get_metrics(wheels_data_aar_c,aar_c_poly)
cph_results <- get_metrics(wheels_data_cph,cph_poly)
ber_results<-get_metrics(wheels_data_ber,ber_poly)
bre_results<-get_metrics(wheels_data_bre,bre_poly)
ham_results<-get_metrics(wheels_data_ham,ham_poly)
# add rownames
rownames(aar_results) <- "Aarhus"
rownames(aar_c_results) <- "Aarhus C"
rownames(cph_results) <- "København"
rownames(ber_results) <- "Berlin"
rownames(bre_results) <- "Bremen"
rownames(ham_results) <- "Hamburg"
# bind all results together in one dataframe
all_results<-rbind(aar_results,aar_c_results,ber_results,bre_results,cph_results,ham_results)
# save as a csv
write.csv(all_results,"metrics.csv",fileEncoding = "WINDOWS-1252")
