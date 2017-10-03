# Locates pseudo absence points

# load packages
library(maptools) 
library(raster)
library(rgeos)
library(ggplot2)
library(dplyr)

source(file.path("R", "prepare_datasets", "functions_for_locating_pseudo_absences.r"))


# ---------------------------------------- define paramaters 


pseudo_absence_proportion <- 2

shp_fl_pth <- file.path(
  "output",
  "shapefiles", 
  "gadm28_adm1_dengue_cropped.shp")

out_pt <- file.path("output", "datasets")

var_1 <- c("ID_0", "NAME_0", "ISO", "ID_1", "NAME_1")

var_2 <- c("y", "x")

col_nms <- c("type", "ID_0", "country", "ISO", "ID_1", "adm1", "latitude", "longitude")

custom_pap_countries <- list("Algeria",
                             "Australia",
                             "China",
                             "Ireland",
                             "Libya", 
                             "Mongolia", 
                             "United Kingdom", 
                             "Western Sahara")


# ---------------------------------------- load data


All_FOI_estimates <- read.table(
  file.path("output", 
            "foi", 
            "All_FOI_estimates_linear.txt"), 
  header = TRUE, 
  sep = ",")

adm_shp_file <- readShapePoly(
  file.path("output",
            "shapefiles", 
            "gadm28_adm1_dengue"))

  
# ---------------------------------------- define variables 


no_data_points <- nrow(All_FOI_estimates) 
  
no_pseudo_absence_points <- floor(no_data_points * pseudo_absence_proportion)  


# ---------------------------------------- subset to -60 +60 latitude AND dengue


fl_ex <- file.exists(shp_fl_pth)

if(fl_ex){
  
  adm_shp_file_cropped <- readShapePoly(shp_fl_pth)

} else {
  
  adm_shp_file_cropped <- crop(
  adm_shp_file, 
  extent(bbox(adm_shp_file)[1,1], 
         bbox(adm_shp_file)[1,2], 
         bbox(adm_shp_file)[2,1], 60))
  
  writeSpatialShape(adm_shp_file_cropped, shp_fl_pth)
  
}

sub_world <- subset(adm_shp_file_cropped, adm_shp_file_cropped@data$dengue == 0)


# ---------------------------------------- sample at random pseudo absences


pseudo_absence_points <- spsample(sub_world, n = no_pseudo_absence_points, type = "random")
  
  
# ----------------------------------------


# Get admin 1 names for pseudo absence points  
overlay_1 <- over(pseudo_absence_points, adm_shp_file)

# Create df
psAbs_df1 <- setNames(data.frame("pseudoAbsence",
                                 overlay_1[, var_1],
                                 pseudo_absence_points@coords[, var_2]),
                      nm = col_nms)


# ---------------------------------------- remove adm 1 duplicates


dup <- duplicated(psAbs_df1[, c("ID_0", "ID_1")])
psAbs_df1 <- psAbs_df1[!dup, ]


# ---------------------------------------- save


write.csv(psAbs_df1, 
          file.path(out_pt, "pseudo_absence_points_1.csv"), 
          row.names = FALSE)


# ===================================================================
#
#                        STOP - customize the pseudo absence data set
#
# ===================================================================


ID_1_maps <- lapply(custom_pap_countries, 
                    map_ID_1,
                    adm_shp_file)

custom_adm1_codes <- list(
  alg = c(22, 41, 3, 44, 6),
  aus = c(5, 6, 8, 9, 10, 11),
  chi = 19,
  ire = c(16, 4, 19),
  lib = c(6, 16, 14, 21, 18, 22, 5),
  mon = c(1, 9, 18, 8),
  uk = c(3, 2),
  wsa = c(1, 4))


# ===================================================================
#
#                             
#
# ===================================================================


# ---------------------------------------- create df of adm 1 NOT included in the first sampling round


my_c_logic <- adm_shp_file@data$NAME_0 %in% custom_pap_countries
adm0_codes_vec <- unique(adm_shp_file@data[my_c_logic, "ID_0"])

custom_adm0_codes <- lapply(
  seq_along(adm0_codes_vec), 
  rep_num,
  adm0_codes_vec, 
  custom_adm1_codes)

custom_adm0_adm1_ls <- mapply(cbind, custom_adm0_codes, custom_adm1_codes)

custom_adm0_adm1 <- do.call("rbind", custom_adm0_adm1_ls)

custom_adm0_adm1 <- as.data.frame(custom_adm0_adm1)

names(custom_adm0_adm1) <- c("ID_0", "ID_1")

regions_to_sample <- anti_join(custom_adm0_adm1, psAbs_df1)


# ---------------------------------------- create a spatial polygon object with the polygons not sampled yet


# get polygon ids 
sub_world_data_subset <- inner_join(sub_world@data, regions_to_sample)
ids <- sub_world_data_subset$OBJECTID

# subset the -60/+60 shp file 
sub_world_copy <- sub_world[sub_world$OBJECTID %in% ids, ]


# ---------------------------------------- sample one point in each adm 1


more_pap_list <- sapply(slot(sub_world_copy, "polygons"), function(i) spsample(i, n = 1, type = "random"))

pseudo_absence_points_2 <- do.call("rbind", more_pap_list)

overlay_2 <- over(pseudo_absence_points_2, adm_shp_file)

psAbs_df2 <- setNames(data.frame("pseudoAbsence",
                                 overlay_2[, var_1],
                                 pseudo_absence_points_2@coords[, var_2]),
                      nm = col_nms)


# ---------------------------------------- rbind all pseudoabsences


all_psAbs <- rbind(psAbs_df1, psAbs_df2)


# ---------------------------------------- save


write.csv(all_psAbs, 
          file.path(out_pt, "pseudo_absence_points_2.csv"), 
          row.names = FALSE)
