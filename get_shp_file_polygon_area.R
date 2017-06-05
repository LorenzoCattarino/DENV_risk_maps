library(rgdal)

shp <- readOGR(file.path("data", "shapefiles", "THA_adm_shp"), "THA_adm1")
  
shp_pr <- spTransform(shp, CRS("+init=epsg:24047"))

all_areas <- sapply(shp_pr@polygons, function(x) x@area)

#from sq m to sq km
all_areas/1000000
