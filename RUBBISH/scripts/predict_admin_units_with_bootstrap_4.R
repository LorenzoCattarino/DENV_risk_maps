# Makes a map of the administrative unit predictions

source(file.path("R", "plotting", "quick_polygon_map.R"))

library(rgdal)
library(colorRamps)
library(lattice)
library(grid)


# define parameters -----------------------------------------------------------  


parameters <- list(id = 4) 

vars_to_average <- "p16"

statistic <- "mean"


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")

out_pth <- file.path("figures", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")


# load data ------------------------------------------------------------------- 


# country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
#                        layer = "gadm28_adm0_eras",
#                        stringsAsFactors = FALSE)

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm1_eras"),
                   stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_col <- matlab.like(100)

mean_pred_fl_nm <- paste0(vars_to_average, "_mean", ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")

adm_shp_pred <- merge(adm_shp, 
                      df_long[, c("ID_0", "ID_1", statistic)], 
                      by = c("ID_0", "ID_1"), 
                      all.x = TRUE)


# plot ------------------------------------------------------------------------


quick_polygon_map(adm_shp_pred, my_col, statistic, out_pth, out_fl_nm)
