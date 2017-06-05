rm(list =ls())
   
# load packages
library(maptools)
library(fields)

# load function 
source(file.path("R", "random_forest", "map_fn.R"))

# load data
geocon_data <- read.csv(file.path("data", "Gecon40_post_final.csv")) 
adm1_shp <- readShapePoly(file.path("data", "shapefiles", "gadm28_levels.shp", "gadm28_adm1.shp"))

print.map (x = geocon_data$PPP2005_40, 
           coord_lon = geocon_data$LONGITUDE, 
           coord_lat = geocon_data$LAT, 
           delta.deg = 1, 
           title = "title", 
           filename = "G-Econ_data.png", 
           lower = 0, 
           upper = 10, 
           shp = adm1_shp) 
