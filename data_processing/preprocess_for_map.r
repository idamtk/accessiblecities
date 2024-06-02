library(tidyverse)
library(sf)
# read in files
aarhus <- st_read("../data/points/Århus_wheels.shp")
copenhagen <- st_read("../data/points/København_wheels.shp")
# add column with city names
aarhus$city <- "Aarhus"
copenhagen$city <- "Copenhagen"
# combine into one dataframe
all <- rbind(arhus,copenhagen)

# add column with colors - this will be used for the markers for the map
all <- all  %>%
  mutate(color = case_when(
    wheelchair == "yes" ~ "green",
    wheelchair == "limited" ~ "orange",
    wheelchair == "no" ~ "purple"))


# using opencage to geocode addresses based on coordinates
library(opencage)

# set opencage
Sys.setenv(OPENCAGE_KEY = "INSERT YOUR KEY HERE")

# set the key interactively if it is missing.
oc_config(key=Sys.getenv("INSERT YOUR KEY HERE"))

# opencage allows 2500 geocodings a day, so the data needs to be split and run
# over two days 
# additionally a few of the observations need to be run on their own to be geocoded successfully
first_part<-all[1:254,]
second_part<-all[255,]
third_part<-all[256:2500,]
# these parts should be run the following day
fourth_part<- all[2501:2784,]
fifth_part <- all[2785,]
sixth_part <- all[2786:2922,]

# get the addresses for all entries based on their coordinates
one <- oc_reverse_df(first_part,latitude=lat, longitude=lng, no_annotations = TRUE)
two <- oc_reverse_df(second_part,latitude=lat, longitude=lng, no_annotations = TRUE)
three <- oc_reverse_df(third_part,latitude=lat, longitude=lng, no_annotations = TRUE)

# save the first batch so it can be loaded in the next day
prelim <-rbind(one,two,three)
saveRDS(prelim,"prelim.RDS")
# read it in again
prelim<-readr::read_rds("prelim.RDS")
# geocode the last 422 observations
four<- oc_reverse_df(fourth_part,latitude=lat, longitude=lng, no_annotations = TRUE)
five <- oc_reverse_df(fifth_part,latitude=lat, longitude=lng, no_annotations = TRUE)
six <-oc_reverse_df(sixth_part,latitude=lat, longitude=lng, no_annotations = TRUE)
# combine all observations into one dataframe
combined <- rbind(prelim,four,five,six)

# create function that removes overlap with name column from address column
remove_name_from_address <- function(name, address) {
  # create a regex pattern from the name
  pattern <- paste0("\\b", str_replace_all(name, "([\\W])", "\\\\\\1"), "\\b(,\\s*)?")
  # remove the name from the address
  str_trim(str_replace(address, pattern, ""))
  
}
# remove name from address to avoid redundancy on labels
combined<- combined %>%
  mutate(oc_formatted = mapply(remove_name_from_address, name, oc_formatted))

# add labels for data in Danish
combined_da<- combined %>%
  mutate(label_da = case_when(
    wheelchair == "yes" ~ "Tilgængelig",
    wheelchair == "limited" ~ "Begrænset tilgængelighed",
    wheelchair == "no" ~ "Utilgængelig"
  ))

# add labels for data in English
combined_en<- combined_en %>%
  mutate(label_en = case_when(
    whelchr == "yes" ~ "Accessible",
    whelchr == "limited" ~ "Limited Accessibility",
    whelchr == "no" ~ "Inaccessible"
  ))

# get list of categories where there are accessible places
combined_yes<-dplyr::filter(combined,wheelchair=="yes")
cats <- sort(unique(combined_yes$amenity))

# add column with clean category names that can be displayed in a menu
combined_en <- combined_en %>%
  mutate(cat_en = case_when(
    amenity == "bank" ~ "Banks",
    amenity == "bar" ~ "Bars and Pubs",
    amenity == "bicycle_rental" ~ "Bicycle Rentals",
    amenity == "bureau_de_change" ~ "Currency Exchanges",
    amenity == "bus_station" ~ "Bus Stations",
    amenity == "cafe" ~ "Cafes",
    amenity == "car_rental" ~ "Car Rentals",
    amenity == "cinema" ~ "Cinemas",
    amenity == "clinic" ~ "Clinics",
    amenity == "community_centre" ~ "Community Centers",
    amenity == "dentist" ~ "Dentists",
    amenity == "doctors" ~ "Doctors",
    amenity == "driving_school" ~ "Driving Schools",
    amenity == "fast_food" ~ "Fast Food Places",
    amenity == "ferry_terminal" ~ "Ferry Terminals",
    amenity == "fuel" ~ "Gas Stations",
    amenity == "hookah_lounge" ~ "Hookah Lounges",
    amenity == "ice_cream" ~ "Ice Cream Cafes",
    amenity == "library" ~ "Libraries",
    amenity == "music_venue" ~ "Music Venues",
    amenity == "nightclub" ~ "Nightclub",
    amenity == "parcel_locker" ~ "Parcel Lockers",
    amenity == "parking_entrance" ~ "Parking",
    amenity == "pharmacy" ~ "Pharmacies",
    amenity == "police" ~ "Police Stations",
    amenity == "post_office" ~ "Post Offices",
    amenity == "pub" ~ "Bars and Pubs",
    amenity == "recycling" ~ "Recycling Stations",
    amenity == "restaurant" ~ "Restaurants",
    amenity == "school" ~ "Schools",
    amenity == "social_facility" ~ "Social Facilities",
    amenity == "theatre" ~ "Theatres",
    amenity == "toilets" ~ "Toilets",
    amenity == "veterinary" ~ "Veterinarians",
  ))

# add column with clean category names in Danish that can be displayed in a menu
combined_da <- combined_da %>%
  mutate(cat_da = case_when(
    amenity == "bank" ~ "Banker",
    amenity == "bar" ~ "Barer",
    amenity == "bicycle_rental" ~ "Cykeludlejning",
    amenity == "bureau_de_change" ~ "Valutaveksling",
    amenity == "bus_station" ~ "Busstationer",
    amenity == "cafe" ~ "Cafeer",
    amenity == "car_rental" ~ "Biludlejning",
    amenity == "cinema" ~ "Biografer",
    amenity == "clinic" ~ "Klinikker",
    amenity == "community_centre" ~ "Kulturhuse",
    amenity == "dentist" ~ "Tandlæger",
    amenity == "doctors" ~ "Læger",
    amenity == "driving_school" ~ "Køreskoler",
    amenity == "fast_food" ~ "Fast Food Restauranter",
    amenity == "ferry_terminal" ~ "Færgeterminaler",
    amenity == "fuel" ~ "Tankstationer",
    amenity == "hookah_lounge" ~ "Vandpibecafeer",
    amenity == "ice_cream" ~ "Iskiosker",
    amenity == "library" ~ "Biblioteker",
    amenity == "music_venue" ~ "Koncertsteder",
    amenity == "nightclub" ~ "Natklubber",
    amenity == "parcel_locker" ~ "Pakkebokse",
    amenity == "parking_entrance" ~ "Parkering",
    amenity == "pharmacy" ~ "Apoteker",
    amenity == "police" ~ "Politistationer",
    amenity == "post_office" ~ "Posthus",
    amenity == "pub" ~ "Barer",
    amenity == "recycling" ~ "Genbrugsstationer",
    amenity == "restaurant" ~ "Restauranter",
    amenity == "school" ~ "Skoler",
    amenity == "social_facility" ~ "Botilbud og plejehem",
    amenity == "theatre" ~ "Teatre",
    amenity == "toilets" ~ "Toiletter",
    amenity == "veterinary" ~ "Dyrlæger",
  ))
# save as shapefiles
st_write(combined_en, "map_data_en.shp", fileEncoding="WINDOWS-1252")
st_write(combined_da, "map_data_da.shp", fileEncoding="WINDOWS-1252")
