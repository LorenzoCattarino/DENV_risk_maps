# Dengue maps
Global mapping of dengue transmission intensity

This repository contains the data and code to make global predictions of dengue (DENV) transmission intensity (both R_0_ and Force of Infection) based on the environmental suitability of DENV transmission. Predictions are made using: 

- a global dataset of FoI estimates (level 1 administrative unit)
- a dataset of bioclimatic, demographic and socioeconomic variables driving suitability of DENV tramsmission (1 km resolution)
- a Random Forest model

R<sub>0</sub> predictions are then used to quantify the global impact of two main DENV control strategies: 
1) release of *Wolbachia*-infected mosquitos,
2) childhood vaccination with the Sanofi Pasteur dengue vaccine.

## FoI dataset
381 estimates of FoI (i.e. the per capita rate at which susceptible individuals acquire infections) were sourced from age-stratified seroprevalence and case notification data carried out in a variety of settings globally.

## Dataset of covariates 
This represents a set of environmental and socio-economic variables which facilitate dengue transmission and for which data were available at the global scale. We considered 5 environmental variables, which were available at high resolutions (1-10 km pixel), measuring: (1) precipitation, (2) diurnal temperature, (3) nocturnal temperature, (4) Enhanced Vegetation Index, (5) Middle Infrared Reflectance, (6) altitude, (7) urban accessibility and (8) population density. These variables were subsequently used as explanatory variables when fitting the geospatial model to predict average force of infection or R<sub>0</sub> (the response variables).

## Random Forest
To make predictions at a finer resolution than the data, spatial disaggregation and Expectation Maximization were employed. First, a bootstrap sample of the original data was created. This was achieved using a spatially explicit approach which allowed to spatially segregate the points inside the bootstrap samples from the points outside the bootstrap sample. Then the admin unit level covariates of the bootstrap sample were spatially disaggregated to 20 km resolution (arbitrary choice of resolution, representing a trade-off between increasing resolution and reducing computational time). A RF model was then trained to the 20 km resolution bootstrap samples (training set) and validated against the data points not present in the bootstrap samples (validation set).      

## Uncertainty 
Predictions are the average across 200 random forest models fitted to different bootstrap samples of the original data. 
