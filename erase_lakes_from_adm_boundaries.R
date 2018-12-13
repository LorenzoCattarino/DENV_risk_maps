library(rgdal)
library(raster)
require(rgeos)

lakes <- readOGR(dsn = file.path("output", "shapefiles"),
                 layer = "lakes_diss")
  
adm0_erase_arcGIS <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                             layer = "gadm28_adm0_eras")

countries <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), 
                     layer = "gadm28_adm0")

countries_no_lakes <- erase(countries, lakes)

writeOGR(obj = countries_no_lakes, 
         dsn = file.path("output", "shapefiles"), 
         layer = "gadm28_adm0_eras", 
         driver = "ESRI Shapefile")


# subset ----------------------------------------------------------------------


canada <- countries[countries@data$NAME_ENGLI == "Canada", ]
  
canada_no_lakes <- erase(canada, lakes_diss)

canada_uni_lakes <- gUnion(canada, lakes, byid = TRUE)

canada_inters_lakes <- intersect(canada, lakes)

canada_inters_lakes <- gIntersection(canada, lakes, byid = TRUE)

canada_no_lakes <- gDifference(canada, lakes, byid = TRUE)

lakes_diss <- gUnaryUnion(lakes)
  
  
# plot ------------------------------------------------------------------------


countries_eras <- readOGR(dsn = file.path("output", "shapefiles"), 
                          layer = "gadm28_adm0_eras")

shp_fl <- adm0_erase_arcGIS

dir.create("figures", FALSE, TRUE)

png(file.path("figures", "adm0_eras_arcGIS.png"),
    width = 16,
    height = 8,
    units = "cm",
    pointsize = 12,
    res = 300)
par(mar = c(0.5,0.5,0.5,0.5), oma = c(0,0,0,0))
plot(shp_fl, col = "red", border = "green", lwd = 0.5)
#plot(lakes, col = "blue", border = "black", lwd = 0.5, add = TRUE)
dev.off()
