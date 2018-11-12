library(rgdal)
library(raster)

lakes <- readOGR(dsn = file.path("output", "shapefiles"),
                 layer = "lakes_diss")
  
countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                     layer = "gadm28_adm0")

countries_no_lakes <- erase(countries, lakes)

writeOGR(countries_no_lakes, ".", "test", driver = "ESRI Shapefile")
