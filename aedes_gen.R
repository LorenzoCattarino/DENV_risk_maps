library(raster)
library(maptools)
library(fields)

# load data 
aedes_gen <- raster(file.path("data", 
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

lands_pop <- raster(file.path("data",
                              "Landscan_2015",
                              "lspop2015.flt"))


# ---------------------------------------- plot original raster


map_colours <- terrain.colors(50)

png(file.path("figures", 
              "aedes_gen_original_res.png"), 
    width = 14, 
    height = 6, 
    units = "in", 
    res = 300)

plot(aedes_gen)

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
