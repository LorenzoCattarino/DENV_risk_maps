# Dengue maps
Global mapping of dengue transmission intensity

This repository contains the data and code to make global predictions of dengue (DENV) transmission intensity (both R0 and Force of Infection) based on the environmental suitability of DENV transmission. Predictions are made using: 

- a global dataset of FoI estimates (level 1 administrative unit)
- a dataset of bioclimatic, demographic and socioeconomic variables driving suitability of DENV tramsmission (1 km resolution)
- a Random Forest model

Predictions are the results of RF models fitted to 200 bootstrap samples of the original dataset. 
