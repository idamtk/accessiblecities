
wheels_data_cph <- readr::read_rds("../data_processing/København_wheels.rds")
wheels_data_aar <- readr::read_rds("../data_processing/Århus_wheels.rds")
wheels_data_ber <- readr::read_rds("../data_processing/Berlin_wheels.rds")
wheels_data_ham <- readr::read_rds("../data_processing/Hamburg_wheels.rds")
wheels_data_bre <- readr::read_rds("../data_processing/Bremen_wheels.rds")
wheels_data_aar_c <- readr::read_rds("../data_processing/aar_c_data.rds")
aar_poly <- readr::read_rds("../data_processing/Århus_poly.rds")
aar_c_poly <- readr::read_rds("../data/aarhus_c.rds")
cph_poly <- readr::read_rds("../data_processing/København_poly.rds")
ber_poly <- readr::read_rds("../data_processing/Berlin_poly.rds")
ham_poly <- readr::read_rds("../data_processing/Hamburg_poly.rds")
bre_poly <- readr::read_rds("../data_processing/Bremen_poly.rds")
library(tidyverse)
aar<- wheels_data_aar%>%distinct(geometry, .keep_all=TRUE)
munic <- getData("GADM", country = "DNK", level = 2)
munic <- readr::read_rds("gadm36_DNK_2_sp.rds")
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
library(dplyr)
ratio_cal <- function(data){
  total <- nrow(data)
  accessible <- data%>%dplyr::filter(wheelchair=="yes")%>%nrow()
  ratio <- accessible/total*100
  print(ratio)
}
ratio_cal(wheels_data_cph)
ratio_cal(wheels_data_aar)


get_metrics <- function(data,polygon){
  density <- density_calc(data,polygon)
  ratio <- ratio_cal(data)
  mean <- avg_distance(data)
  results <- data.frame(density=density,
                        ratio=ratio,
                        average_distance=mean)
}
aar_results<-get_metrics(wheels_data_aar,aar_poly)
aar_c_results<-get_metrics(wheels_data_aar_c,aar_c_poly)
cph_results <- get_metrics(wheels_data_cph,cph_poly)

ber_results<-get_metrics(wheels_data_ber,ber_poly)

bre_results<-get_metrics(wheels_data_bre,bre_poly)
ham_results<-get_metrics(wheels_data_ham,ham_poly)
rownames(aar_results) <- "Aarhus"
rownames(aar_c_results) <- "Aarhus C"
rownames(cph_results) <- "København"
rownames(ber_results) <- "Berlin"
rownames(bre_results) <- "Bremen"
rownames(ham_results) <- "Hamburg"
all_results<-rbind(aar_results,aar_c_results,ber_results,bre_results,cph_results,ham_results)
write.csv(all_results,"metrics.csv",fileEncoding = "WINDOWS-1252")
