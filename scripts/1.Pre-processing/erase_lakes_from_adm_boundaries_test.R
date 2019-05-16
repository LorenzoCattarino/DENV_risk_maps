library(rgdal)
# library(raster)
require(rgeos)

xy <- list(cbind(x = c(0, 1, 1, 0), 
                 y = c(0, 0, 1, 1)),
           cbind(x = c(1, 2, 2, 1),
                 y = c(1, 1, 2, 2)),
           cbind(x = c(0, 1, 1, 0),
                 y = c(1, 1, 2, 2)))

ps <- sapply(xy, Polygon, hole = FALSE)

# add id variable 
p1 <- Polygons(ps, ID = 1) 

my_spatial_polys_1 <- SpatialPolygons(
  list(p1), 
  proj4string = CRS("+proj=longlat +datum=WGS84"))


# -------------------------------------------------------------------


xy <- list(cbind(x = c(0.5, 1, 1, 0.5), 
                 y = c(1, 1, 1.5, 1.5)))

ps <- sapply(xy, Polygon, hole = TRUE)

# id variable 
p1 <- Polygons(ps, ID = 2) 

my_spatial_polys_2 <- SpatialPolygons(
  list(p1), 
  proj4string = CRS("+proj=longlat +datum=WGS84"))

plot(my_spatial_polys_1, col = "red")
plot(my_spatial_polys_2, col = NA, add = TRUE)

# er <- erase(my_spatial_polys_1, my_spatial_polys_2)
er <- gDifference(my_spatial_polys_1, my_spatial_polys_2)
      
plot(er, col = "red")

un <- gUnion(my_spatial_polys_1, my_spatial_polys_2)
plot(un, col = "red")


# --------------------------------------------------------------------


combined_poly <- rbind(my_spatial_polys_1, my_spatial_polys_2)

plot(combined_poly, col = "red")
combined_poly@polygons[[1]]
  
mine@polygons[[1]]@Polygons[[2]]@coords = mine@polygons[[1]]@Polygons[[2]]@coords[5:1,]


