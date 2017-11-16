# Attach to the foi dataset a column with the area (km2) of each admin unit  

library(rgdal)
library(dplyr)


# ---------------------------------------- define parameters


winkel_tripel_crs <- CRS("+proj=wintri")

foi_out_pt <- file.path("output", "foi")

foi_out_nm <- "All_FOI_estimates_linear_env_var_area.csv"


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 

shp <- readOGR(file.path("data", "shapefiles", "gadm28_levels.shp"), "gadm28_adm1")


# ---------------------------------------- start


shp_pr <- spTransform(shp, winkel_tripel_crs)

all_areas <- sapply(shp_pr@polygons, function(x) x@area)

# from sq m to sq km
shp_pr@data$Shape_Area <- all_areas/1000000 

shp_pr@data$ID_0 <- as.numeric(as.character(shp_pr@data$ID_0))
shp_pr@data$ID_1 <- as.numeric(as.character(shp_pr@data$ID_1))

shp_pr@data <- shp_pr@data[!duplicated(shp_pr@data[, c("ID_0", "ID_1")]), ]

foi_data_2 <- left_join(foi_data, shp_pr@data[, c("ID_0", "ID_1", "Shape_Area")], by = c("ID_0", "ID_1"))


# ---------------------------------------- save 


write.csv(foi_data_2, 
          file.path(foi_out_pt, foi_out_nm), 
          row.names = FALSE)
