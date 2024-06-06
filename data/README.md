# AccessibleCities data
This folder contains the data used for the AccessibleCities app.
All of the data should be loaded through the ```fetch_data.r``` script.
The data consists of:
- Polygons for municipalities in Denmark and federal states in Germany. The data for these is from GADM (Database of Global Administrative Areas). The data for this should be fetched automatically by the code. However, as the GADM server does not always respond, the data can also be downloaded from the following links and then be loaded locally:
    - German data - Click the link for R (sp), level1
    - Danish data - Click the link for R (sp), level2
      Both files are included in this folder as ```gadm36_DEU_1_sp.rds``` and ```gadm36_DNK_2_sp.rds```
      GADM's data is "freely available for academic use and other non-commercial use".
      From this data only the data for Aarhus Kommune, København Kommune as well as the German Bundesländer Bremen, Berlin
      and Hamburg is used for the app. These are saved in the polygons folder.
- Polygon for the 8000 postal code in Denmark (Aarhus C). This data is from Styrelsen for Dataforsyning og Infrastruktur (Agency for Data Supply and Infrastructure) and is licensed under CC BY 4.0. This is saved in the polygon folder.

- This code also loads all points with an OpenStreetMap tag for wheelchair accessibility within the polygons above. © OpenStreetMap contributers. Open Data Commons Open Database License (ODbL) by the OpenStreetMap Foundation (OSMF).
This data has been loaded on the 27th of May. If this script is rerun at a later data, the data might be slightly different if tags have been added/removed since the 27th of May. TO ensure complete reproducibility, any rerun will receive the ```_rerun``` suffix to ensure it does not overwrite the original data. 
