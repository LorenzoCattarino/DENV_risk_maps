# Creates a map of the dataset point and dengue presence-absence mask 

# load packages
library(rgdal) 
library(dplyr)
library(colorRamps)


# ---------------------------------------- load data


All_FOI_estimates <- read.table(
  file.path("output", 
            "foi", 
            "All_FOI_estimates_linear.txt"), 
  header = TRUE, 
  sep = ",")

pseudoAbsences <- read.csv(
  file.path("output", 
            "datasets", 
            "pseudo_absence_points_NUM_CODES_sub.csv"), 
  header = TRUE)

world_shp_admin_1_dengue <- readOGR(dsn = file.path("data", "shapefiles", "gadm28_levels.shp"), layer = "gadm28_adm1_dengue")


# ---------------------------------------- pre processing


data_points <- SpatialPoints(All_FOI_estimates[, c("longitude", "latitude")])
pseudoAbsence_points <- SpatialPoints(pseudoAbsences[, c("longitude","latitude")])

data_points_list <- list(
  "sp.points",
  data_points,
  pch = 21, fill = "black", col = NA, cex = 1)

pseudoAbsence_points_list <- list(
  "sp.points", 
  pseudoAbsence_points,
  pch = 21, fill = "yellow", col = NA, cex = 1)


# ---------------------------------------- plot


png(file.path("figures", "dengue_points_and_absence_mask.png"), 
     width = 18, 
    height = 10, 
    units = "in", 
    pointsize = 12,
    bg = "white", 
    res = 200)

spplot(world_shp_admin_1_dengue, "dengue", lwd = 0.5,
       scales = list(x = list(draw = TRUE, 
                              at = seq(-150, 150, 50)), 
                     y = list(draw = TRUE)),
       xlab = "Longitude",
       ylab = "Latitude", 
       col.regions = c("palegreen3","red2"),
       colorkey = FALSE,
       sp.layout = list(data_points_list,
                        pseudoAbsence_points_list))

dev.off()
