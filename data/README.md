# AccessibleCities data
This folder contains the data used for the AccessibleCities app.
All of the data should be loaded through the code.
The data consists of:
- Polygons for municipalities in Denmark and federal states in Germany.    The data for these is from GADM (Database of Global Administrative Areas). The data for this should be fetched automatically by the code. However, as the GADM server does not always respond, the data can also be downloaded from the following links:
    - German data - Click the link for R (sp), level1
    - Danish data - Click the link for R (sp), level2
From this data only the data for Århus Kommune, København Kommune as well as the German Bundesländer Bremen, Berlin and Hamburg is used for the app.
- Polygons for postal codes in Denmark. This data is from Kortforsyningen and will be downloaded through the code. Only the polygon from Århus C is used for the app.

- This code also loads all points with an OpenStreetMap tag for wheelchair accessibility within the polygons above. OpenStreetMap for 
This data has been loaded on the 1st of June. If this script is rerun at a later data, the data might be slightly different if tags have been added/removed since the 1st of June.


## How to run
Requirements:
  -
1. Clone repo
2.

## License
## Contact

