wheels_data_cph <- readr::read_rds("cph_wheels.rds")
wheels_data_aar <- readr::read_rds
aar_poly <- readr::read_rds("aarhus_poly.rds")
cph_poly <- readr::read_rds("cph_poly.rds")

munic <- getData("GADM", country = "DNK", level = 2)
munic <- as(munic, "sf")
cph<-munic[25,]
cph_poly<- cph %>% st_transform(4326)
library(sf)
st_area(aar_poly)/1e+06

density_calc <- function (data,poly){
  area<-st_area(poly)/1*1e+06
  amount_acc_places<- nrow(data)
  density <- amount_acc_places/area
  print("There are the following number of accessible locations per km2")
  print(density)
}
density_calc(wheels_data_aar,aar_poly)
density_calc(wheels_data_cph,cph_poly)

