library(tidyverse)
library(sf)
# read in files
århus <- readr::read_rds("../data/århus_wheels.rds")
københavn <- readr::read_rds("../data/København_wheels.rds")
# create function that adds a colors column which we will use for the markers on the map
add_colors <- function(data){
  
  data_colored <- data  %>%
    mutate(color = case_when(
      wheelchair == "yes" ~ "green",
      wheelchair == "limited" ~ "orange",
      wheelchair == "no" ~ "red"
    ))
}

# before we apply the function we geocode the addresses based on coordinates
library(opencage)

# set your opencage key
# set key in the environment
Sys.setenv(OPENCAGE_KEY = "db63550694224928890faabc2417f082")

# set the key interactively if it is missing.
oc_config(key=Sys.getenv("db63550694224928890faabc2417f082"))

# the observation in row 255 of århuscauses some trouble if run
# with all other observations but works on its own, so I split the dataset into 
# two parts
first_part<-århus[-255,]
second_part<-århus[255,]

# get the addresses for all entries based on their coordinates
first<- oc_reverse_df(first_part,latitude=lat, longitude=lng, no_annotations = TRUE)
two<- oc_reverse_df(second_part,latitude=lat, longitude=lng, no_annotations = TRUE)
# combine dataframes with coordinates
combined <- rbind(first, second)
# add colors column
århus_comb<- add_colors(combined)
# save as rds
saveRDS(århus_comb,"aarhus_map_data.rds")
#now we do the same with the copenhagen dataset
# k?benhavn needs to be split into several parts because:
# 1) the dataset has more than 2500 observations which is the daily limit for geocoding with opencage
# 2) because observation 2398 causes some issues if run with the rest of the dataset
cph_part_1<-københavn[1:2397,] 
cph_1<- oc_reverse_df(cph_part_1,latitude=lat, longitude=lng, no_annotations = TRUE)
cph_part_2<- københavn[2398,]
cph_2<- oc_reverse_df(cph_part_2,latitude=lat, longitude=lng, no_annotations = TRUE)
cph_part_3<-københavn[2399:2535,]
cph_3<- oc_reverse_df(cph_part_3,latitude=lat, longitude=lng, no_annotations = TRUE)
# create one dataframe with all coords
cph_coords<-rbind(cph_1, cph_2, cph_3)
# add color column
cph_comb<- add_colors(cph_coords)
# save for later use
saveRDS(cph_comb,"cph_map_data.rds")
