
library(dplyr)
library(rgdal)

source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------  


map_proj <- "+proj=moll"

out_fl_nm <- "all_adm1_env_var.csv"

out_pt <- file.path("output", "env_variables")


# load data -------------------------------------------------------------------


adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        stringsAsFactors = FALSE)
  
shp_fl_adm1 <- readOGR(file.path("data", "shapefiles", "gadm28_levels.shp"), "gadm28_adm1")


# calculate population density ------------------------------------------------


shp_pr <- spTransform(shp_fl_adm1, map_proj)

all_areas <- sapply(shp_pr@polygons, function(x) x@area)

# from sq m to sq km
shp_pr@data$Shape_Area <- all_areas / 1000000 

shp_pr@data$ID_0 <- as.numeric(as.character(shp_pr@data$ID_0))
shp_pr@data$ID_1 <- as.numeric(as.character(shp_pr@data$ID_1))

shp_pr@data <- shp_pr@data[!duplicated(shp_pr@data[, c("ID_0", "ID_1")]), ]

ret <- inner_join(adm_dataset, shp_pr@data[, c("ID_0", "ID_1", "Shape_Area")], by = c("ID_0", "ID_1"))

ret$log_pop_den <- log(1 + (ret$population / ret$Shape_Area))  


# save ------------------------------------------------------------------------


write_out_csv(ret, out_pt, out_fl_nm)
