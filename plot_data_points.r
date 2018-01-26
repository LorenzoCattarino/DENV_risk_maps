# Creates a map of the dataset point and dengue presence-absence mask 

# load packages
library(rgdal) 
library(ggplot2)


# load data ------------------------------------------------------------------- 


All_FOI_estimates <- read.table(
  file.path("output", 
            "foi", 
            "All_FOI_estimates_linear.txt"), 
  header = TRUE, 
  sep = ",")

pseudoAbsences <- read.csv(
  file.path("output", 
            "datasets", 
            "pseudo_absence_points_2.csv"), 
  header = TRUE)

world_shp_admin_1_dengue <- readOGR(dsn = file.path("output", "shapefiles"), 
                                    layer = "gadm28_adm0_eras")

bra <- readOGR(dsn = file.path("data", "shapefiles", "BRA_adm_shp"), 
               layer = "BRA_adm1")

col <- readOGR(dsn = file.path("data", "shapefiles", "COL_adm_shp"), 
               layer = "COL_adm1")

ven <- readOGR(dsn = file.path("data", "shapefiles", "VEN_adm_shp"), 
               layer = "VEN_adm1")

mex <- readOGR(dsn = file.path("data", "shapefiles", "MEX_adm_shp"), 
               layer = "MEX_adm1")

ind <- readOGR(dsn = file.path("data", "shapefiles", "IND_adm_shp"), 
               layer = "IND_adm1")

aus <- readOGR(dsn = file.path("data", "shapefiles", "AUS_adm_shp"), 
               layer = "AUS_adm1")


# pre processing -------------------------------------------------------------- 


fort_shp <- fortify(world_shp_admin_1_dengue)

fort_bra <- fortify(bra)
fort_col <- fortify(col)
fort_ven <- fortify(ven)
fort_mex <- fortify(mex)
fort_ind <- fortify(ind)
fort_aus <- fortify(aus)


# plot ------------------------------------------------------------------------ 


png(file.path("figures", "dengue_points.png"), 
    width = 18, 
    height = 10, 
    units = "in", 
    pointsize = 12,
    res = 200)

p <- ggplot() +
  geom_polygon(data = fort_shp, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_polygon(data = fort_bra, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_polygon(data = fort_ind, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_polygon(data = fort_mex, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_polygon(data = fort_col, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_polygon(data = fort_ven, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_polygon(data = fort_aus, 
               aes(x = long, y = lat, group = group),
               colour = "black",
               fill = "gray80",
               size = 0.2) + 
  geom_point(data = All_FOI_estimates, aes(x = longitude, y = latitude)) +
  coord_equal() +
  scale_y_continuous(labels = NULL, limits = c(-60, 60)) +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(p)

dev.off()
