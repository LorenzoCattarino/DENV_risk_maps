# Makes a map of the admin unit level predictions

source(file.path("R", "create_parameter_list.R"))
source(file.path("R", "plotting", "quick_polygon_map.R"))
source(file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))
       
library(sf)
library(ggplot2)
library(shades)


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 2,
                   statistic = "mean")


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

vars_to_average <- c("response_endemic", 
                     "p9", 
                     "p16")

statistic <- parameters$statistic

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


national_borders <- st_read(dsn = file.path("output", "shapefiles"),
                            layer = "gadm28_adm0_eras",
                            stringsAsFactors = FALSE)
  
adm_shp <- st_read(dsn = file.path("output", "shapefiles"), 
                   layer = "gadm28_adm1_eras",
                   stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


# my_col <- colorRamps::matlab.like(100)
my_col <- creating_color_palette(100)


# plot ------------------------------------------------------------------------


for (i in seq_along(vars_to_average)){
  
  var_to_average <- vars_to_average[i]
  
  message(var_to_average)  
  
  mean_pred_fl_nm <- paste0(var_to_average, "_", statistic, ".rds")
  
  df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))
  
  out_fl_nm <- paste0(var_to_average, "_", statistic, ".png")
  
  adm_shp_pred <- merge(adm_shp, 
                        df_long[, c("ID_0", "ID_1", statistic)], 
                        by = c("ID_0", "ID_1"), 
                        all.x = TRUE)
  
  quick_polygon_map(adm_shp_pred, my_col, statistic, out_pth, out_fl_nm,
                    country_borders = national_borders)

}
