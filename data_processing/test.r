library(httr)
library(sf)
library(xml2)

# Define the URL with your token
url <- "https://api.dataforsyningen.dk/DAGI_10MULTIGEOM_GMLSFP_DAF?service=WFS&request=GetCapabilities&token=8fdc7553a1e8adc188ecd830dfb7f7c4"

# Make a GET request
response <- GET(url, query=list(request="POSTNUMMERINDDELING"))

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- content(response, "parsed")
  
  # Now you can work with the data
  print(data)
} else {
  # Print error message if request failed
  print("Error: Failed to retrieve data")
}
library(xml2)

# Parse the XML document
doc <- xml2::read_xml(response)

# Extract the data you need
# For example, to extract the Title from ServiceIdentification
all <- xml2::xml_text(xml2::xml_find_fir(doc))

# Print the extracted data
print(all)

library(XML)

data <- xmlParse(response)

xml_data <- xmlToList(data)
print(xml_data)
