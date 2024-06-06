# Accessible Cities
This repository contains the scripts and data used for the Shiny app Accessible Cities. The app is available at this [link](https://accessiblecities.shinyapps.io/deploy/).
This app lets you find locations in Aarhus and Copenhagen which are accessible for wheelchair users. It also contains a tool for comparing the accessibility of Aarhus, Copenhagen, Bremen, Berlin and Hamburg.
This app was created as part of my exam for Spatial Analytics at AU.

## How to run
This section contains instructions for how to run this code on your machine. This repository has been developed and tested on a Windows 10 operating system with R 4.3.0 and RStudio 2023.06.0+421.

### Clone this repository

```
git clone https://github.com/idamtk/accessiblecities.git
cd accessiblecities
```

## Running the app
If you wish to simply run the app follow these steps:
1. Go to the ```app``` folder
2. Open ```app_en.r``` in RStudio for the English version of the app. Open ```app_da.r``` for the Danish version
3. Install any requirements not already on your machine.
4. Press "Run app".

## Running the entire pipeline
If you wish to rerun the entire pipeline, follow these steps. All scripts will assume that the working directory is the directory they are located in.
1. Run ```fetch_data.r```. It is located in the ```data``` folder.
2. Run ```comparisons.r```. It is located in the ```analysis``` folder.
3. Run ```preprocess_for_map.r```. It is located in the ```data_preprocessing``` folder. This script might need to be run over two days as it relies on OpenCage geocoding, which only allows for 2500 free requests per day. The dataset that is geocoded contains 2922 entries. The script is built to consider this and will save a preliminary file with the first 2500 entries and then add the 422 entries once the following lines are run. However, it can also be run with the first 2500 entries, if one does not have the time to wait until the next day. 
5. Run ```app_en.r``` or ```app_da.r```
## License
This code is licensed under the MIT license.
## Contact
For any questions, please contact ik@cc.au.dk. 
