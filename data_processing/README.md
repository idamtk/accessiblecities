# Data processing
This folder contains the script for preprocessing the data for the map, ```preprocess_for_map.r``` and the files produced by this script. The produced files are shapefiles, which consists of a package of four files (.dbf, .prj,.shp and .shx). There is one group of files for the Danish version of the app and one for the English version of the app.

## The script does the following:
- Adds a color column which will be used define the color for each marker on the map
- Reverse geocodes all points to fetch their addresses
- Removes overlap between names and addresses to remove redundancy from the labels which will be displayed on the map.
- Adds columns with presentable versions of the labels for wheelchair accessibility and category of location in Danish or English.

The script might need to be run over two days, as it requires geocoding 2922 points and OpenCage only allows free geocoding of up to 2500 points per day. Alternatively the code can be run with only the first 2500 observations. Instructions for each approach follow below:
## Geocoding all points
1. Run lines 1-47 in the script.
2. Close your RStudio.
3. Open RStudio again the following day.
4. Run lines 49-168 in the script.

## Geocoding only the first 2500 points
1. Run lines 1-43 in the script.
2. Change line 46 from this:
   ```
   prelim <-rbind(one,two,three)
   ```
   to the following:
   ```
   combined <-rbind(one,two,three)
   ```
3. Run lines 58-168 in the script.
