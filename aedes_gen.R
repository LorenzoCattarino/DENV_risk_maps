rm(list = ls())

# load packages 
library(raster)
library(maptools)
library(fields)

# load data 
Global.mod.results2005to2010 <- raster(file.path("data", 
                                                 "aedes_generations", 
                                                 "eggsgen_ck_2005-2010_Global_arg.grd"))
country_border_shp <- readShapePoly(file.path("data", 
                                              "shapefiles", 
                                              "gadm28_levels.shp", 
                                              "gadm28_adm0.shp"))
admin_shp <- readShapePoly(file.path("data", 
                                     "shapefiles", 
                                     "gadm28_levels.shp", 
                                     "gadm28_adm1.shp"))  

# define projection
geograhic_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

proj4string(country_border_shp) <- geograhic_CRS

?projectRaster

# ---------------------------------------- plot original raster


map_colours <- terrain.colors(50)

lon <- seq(150, 150, by = 50)
lat <- seq(-60, 60, by = 20)

tiff(file.path("figures", "dengue_dataset", 
              "aedes_gen_original_res.tiff"), 
    width = 10, height = 6, units = "in", 
    compression = "lzw",
    res = 300)

print(plot(
  Global.mod.results2005to2010,
  ext = extent(-180, 180, -80, 80),
  axes = TRUE, 
  main = "number of aedes generations at 25 x 25 pixel res", 
  xlab = "Longitude",
  ylab = "Latitude",
  useRaster = FALSE)) 

axis(1, at = seq(-150, 150, 50), lab = seq(-150, 150, 50))

axis(2, at = seq(-60, 60, 20), lab = seq(-60, 60, 20))

box()

plot(country_border_shp, lwd = 0.5, border = "gray30", add = TRUE)

dev.off()


# ---------------------------------------- aggregate raster and attach to admin unit shp 


generation_numbers <- extract(Global.mod.results2005to2010, admin_shp, fun = mean, na.rm = TRUE, df = TRUE)

admin_shp_2 <- merge(admin_shp, generation_numbers, by.x = "OBJECTID", by.y = "ID")

write.table(admin_shp_2@data, 
            file.path("output", 
                      "datasets", 
                      "aedes_generations.csv"), 
            row.names = FALSE, sep = ",")


# ---------------------------------------- plot shp file with aggregated raster value 


tiff(file.path("figures", "dengue_dataset", 
               "aedes_gen_adm1_res.tiff"), 
     width = 10, height = 6, units = "in", 
     compression = "lzw",
     res = 300)

# Create list object for country borders
country_border_shp_list <- list("sp.polygons", 
                                country_border_shp, 
                                lwd = 0.2, 
                                col = "gray30", 
                                first = FALSE)

print(spplot(
  admin_shp_2, 
  zcol = "layer",
  col = NA,
  scales = list(x = list(draw = TRUE, 
                         at = seq(-150, 150, 50)), 
                y = list(draw = TRUE,
                         at = seq(-60, 60, 20))),
  xlab = "Longitude",
  ylab = "Latitude", 
  main = list(label = "number of aedes generations at admin unit 1 res", cex = 1.3),
  col.regions = rev(map_colours),
  ylim = c(-80, 90),
  sp.layout = list(country_border_shp_list)))

dev.off()
