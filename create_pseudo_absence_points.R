rm(list=ls())

# load packages
library(maptools) 
library(raster)
library(rgeos)

# load data 
All_FOI_estimates <- read.table (file.path("output", 
                                           "datasets",
                                           "foi", 
                                           "All_FOI_estimates_linear.txt"), 
                                 header = TRUE, sep = ",")

world_shp_admin_1_dengue <- readShapePoly(file.path("data","shapefiles", "gadm28_levels.shp", "gadm28_adm1_dengue"))
  
no_data_points <- nrow(All_FOI_estimates) 

pseudo_absence_proportion <- 1
  
no_pseudo_absence_points <- floor(no_data_points * pseudo_absence_proportion)  

world_shp_admin_1_dengue_cropped <- crop(
  world_shp_admin_1_dengue, 
  extent(bbox(world_shp_admin_1_dengue)[1,1], 
         bbox(world_shp_admin_1_dengue)[1,2], 
         bbox(world_shp_admin_1_dengue)[2,1], 60))

# Sample pseudo absences 
pseudo_absence_points <- spsample(
  subset(world_shp_admin_1_dengue_cropped, world_shp_admin_1_dengue_cropped@data$dengue == 0), 
  n = no_pseudo_absence_points, 
  type = "random")

# Get admin 1 names for pseudo absence points  
overlay <- over(
  pseudo_absence_points, 
  world_shp_admin_1_dengue)

# Create df
pseudo_absence_points_df <- data.frame(type = "pseudoAbsence",
                                       ID_0 = overlay$ID_0,
                                       country = overlay$NAME_0, 
                                       country_code = overlay$ISO, 
                                       ID_1 = overlay$ID_1,
                                       adm1 = overlay$NAME_1, 
                                       latitude = pseudo_absence_points@coords[,"y"],
                                       longitude = pseudo_absence_points@coords[,"x"])

write.table(pseudo_absence_points_df, 
            file.path("output", 
                      "datasets", 
                      "pseudo_absence_points.csv"), 
            row.names = FALSE, 
            sep = ",")
