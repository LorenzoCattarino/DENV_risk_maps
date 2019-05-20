# Creates a map of the dataset point and dengue presence-absence mask 

# load packages
library(rgdal) 
library(dplyr)
library(colorRamps)

source(file.path("R", "utility_functions.R"))


# load data ------------------------------------------------------------------- 


All_FOI_estimates <- read.csv(file.path("output", 
                                        "foi", 
                                        "FOI_estimates_lon_lat_gadm_R0.csv"), 
                              header = TRUE)

pseudoAbsences <- read.csv(file.path("output", 
                                     "datasets", 
                                     "pseudo_absence_points_2.csv"), 
                           header = TRUE)

world_shp_admin_1_dengue <- readOGR(dsn = file.path("output", "shapefiles"), 
                                    layer = "gadm28_adm1_dengue")


# pre processing -------------------------------------------------------------- 


data_points <- SpatialPoints(All_FOI_estimates[, c("longitude", "latitude")])
pseudoAbsence_points <- SpatialPoints(pseudoAbsences[, c("longitude","latitude")])

data_points_list <- list(
  "sp.points",
  data_points,
  pch = 21, fill = "dodgerblue", col = NA, cex = 0.5)

pseudoAbsence_points_list <- list(
  "sp.points", 
  pseudoAbsence_points,
  pch = 21, fill = "yellow", col = NA, cex = 0.5)


# plot ------------------------------------------------------------------------ 


p <- spplot(world_shp_admin_1_dengue, "dengue", lwd = 0.5,
            scales = list(x = list(draw = TRUE, 
                                   at = seq(-150, 150, 50)), 
                          y = list(draw = TRUE)),
            xlab = "Longitude",
            ylab = "Latitude", 
            col.regions = c("palegreen3","red2"),
            colorkey = FALSE,
            sp.layout = list(data_points_list,
                             pseudoAbsence_points_list))

save_plot(plot_obj = p, 
          out_pth = file.path("figures", "data"), 
          out_fl_nm = "dengue_points_and_absence_mask", 
          wdt = 18, 
          hgt = 10) 
