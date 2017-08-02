# load packages
library(maptools) 
library(raster)
library(rgeos)


# ---------------------------------------- define paramaters 


pseudo_absence_proportion <- 1

out_pt <- file.path("output", "datasets")

out_nm <- "pseudo_absence_points.csv"


# ---------------------------------------- load data


All_FOI_estimates <- read.table(
  file.path("output", 
            "foi", 
            "All_FOI_estimates_linear.txt"), 
  header = TRUE, 
  sep = ",")

world_shp_admin_1_dengue <- readShapePoly(
  file.path("data",
            "shapefiles", 
            "gadm28_levels.shp", 
            "gadm28_adm1_dengue"))

  
# ---------------------------------------- pre processing


no_data_points <- nrow(All_FOI_estimates) 
  
no_pseudo_absence_points <- floor(no_data_points * pseudo_absence_proportion)  


# ---------------------------------------- run


world_shp_admin_1_dengue_cropped <- crop(
  world_shp_admin_1_dengue, 
  extent(bbox(world_shp_admin_1_dengue)[1,1], 
         bbox(world_shp_admin_1_dengue)[1,2], 
         bbox(world_shp_admin_1_dengue)[2,1], 60))

sub_world <- subset(world_shp_admin_1_dengue_cropped, world_shp_admin_1_dengue_cropped@data$dengue == 0)

# Sample pseudo absences 
pseudo_absence_points <- spsample(sub_world, 
                                  n = no_pseudo_absence_points, 
                                  type = "random")

# Get admin 1 names for pseudo absence points  
overlay_1 <- over(pseudo_absence_points, world_shp_admin_1_dengue)

# Create df
psAbs_df1 <- data.frame(type = "pseudoAbsence",
                        ID_0 = overlay_1$ID_0,
                        country = overlay_1$NAME_0, 
                        country_code = overlay_1$ISO, 
                        ID_1 = overlay_1$ID_1,
                        adm1 = overlay_1$NAME_1, 
                        latitude = pseudo_absence_points@coords[,"y"],
                        longitude = pseudo_absence_points@coords[,"x"])


# ---------------------------------------- customize the pseudo absence set


# subset Ireland from the world shp 
all_adm1_IRL <- world_shp_admin_1_dengue_cropped[world_shp_admin_1_dengue_cropped@data$NAME_0 == "Ireland",]

# sample adm 1 in Ireland 
IRL_adm_ids <- sample(1:nrow(all_adm1_IRL@data), 2, replace = FALSE)

# get the Irish polygons
IRL_adm <- all_adm1_IRL[IRL_adm_ids, ]

# transform the polygons to spatial points (using polygon centroids) 
irish_point <- SpatialPoints(coordinates(IRL_adm))

# sample one point in Scotland 
scottish_point <- spsample(
  subset(world_shp_admin_1_dengue_cropped, world_shp_admin_1_dengue_cropped@data$NAME_1 == "Scotland"), 
  n = 2, 
  type = "random")

overlay_2 <- over(irish_point, world_shp_admin_1_dengue)

psAbs_df2 <- data.frame(type = "pseudoAbsence",
                        ID_0 = overlay_2$ID_0,
                        country = overlay_2$NAME_0, 
                        country_code = overlay_2$ISO, 
                        ID_1 = overlay_2$ID_1,
                        adm1 = overlay_2$NAME_1, 
                        latitude = irish_point@coords[, 2],
                        longitude = irish_point@coords[, 1])

overlay_3 <- over(scottish_point, world_shp_admin_1_dengue)

psAbs_df3 <- data.frame(type = "pseudoAbsence",
                        ID_0 = overlay_3$ID_0,
                        country = overlay_3$NAME_0, 
                        country_code = overlay_3$ISO, 
                        ID_1 = overlay_3$ID_1,
                        adm1 = overlay_3$NAME_1, 
                        latitude = scottish_point@coords[,"y"],
                        longitude = scottish_point@coords[,"x"])


# ---------------------------------------- rbind all pseudoabsences


all_psAbs <- rbind(psAbs_df1, psAbs_df2, psAbs_df3)


# ---------------------------------------- save


write.csv(all_psAbs, 
          file.path(out_pt, out_nm), 
          row.names = FALSE)
