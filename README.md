# Dengue maps
Global mapping of dengue transmission intensity

This repository contains the data and code to make global predictions of dengue (DENV) transmission intensity (both R0 and Force of Infection) based on the environmental suitability of DENV transmission. Predictions are made using: 

- a global dataset of FoI estimates (level 1 administrative unit)
- a dataset of bioclimatic, demographic and socioeconomic variables driving suitability of DENV tramsmission (1 km resolution)
- a Random Forest model

## FoI dataset
381 estimates of FoI (i.e. the per capita rate at which susceptible individuals acquire infections) were sourced from age-stratified seroprevalence and case notification data carried out in a variety of settings globally.

## Dataset of covariates 
This represents a set of environmental and socio-economic variables which facilitate dengue transmission and for which data were available at the global scale. We considered 5 environmental variables, which were available at high resolutions (1-10 km pixel), measuring: (1) precipitation, (2) diurnal temperature, (3) nocturnal temperature, (4) Enhanced Vegetation Index, (5) Middle Infrared Reflectance, (6) altitude, (7) urban accessibility and (8) population density. These variables were subsequently used as explanatory variables when fitting the geospatial model to predict average force of infection or R0 (the response variables).

## Random Forest


## Uncertainty 
Predictions are the average of across 200 random forest models fitted to different bootstrap samples of the original dataset. 
