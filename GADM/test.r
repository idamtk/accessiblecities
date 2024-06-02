library(tidyverse)
url <- "https://api.dataforsyningen.dk/postnumre?format=geojson"
geofile<- tempfile()
download.file(url,geofile)
library(sf)
geodata<-st_read(geofile)
aar_c<-geodata%>%dplyr::filter(nr==8000)
aar_c_4326 <- aar_c%>% st_transform(4326)
saveRDS(aar_c_4326,"aarhus_c.rds")
