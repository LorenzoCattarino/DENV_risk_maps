# Makes a map of the square predictions and save the raster

source(file.path("R", "plotting", "functions_for_plotting_raster_maps.r"))
source(file.path("R", "utility_functions.r"))

library(fields)
library(colorRamps)


# define parameters -----------------------------------------------------------  


parameters <- list(id = 4,
                   dependent_variable = "FOI",
                   z_range = list(FOI = c(0, 0.06),
                                  R0_1 = c(0, 8),
                                  R0_2 = c(0, 4),
                                  R0_3 = c(0, 5))) 

vars_to_average <- "response"

statistic <- "mean"


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

z_range <- parameters$z_range[[var_to_fit]]

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)
  
out_path <- file.path("figures", 
                      "predictions_world",
                      "bootstrap_models",
                      model_type)


# pre processing -------------------------------------------------------------- 


my_col <- matlab.like(100)

mean_pred_fl_nm <- paste0(vars_to_average, "_mean", ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")


# plot ------------------------------------------------------------------------ 


quick_raster_map(pred_df = df_long, 
                 variable = vars_to_average, 
                 statistic = statistic, 
                 my_col = my_col, 
                 out_pt = out_path, 
                 out_name = out_fl_nm,
                 z_range = z_range)


# save the raster -------------------------------------------------------------


my_ras <- raster::rasterFromXYZ(df_long[, c("longitude","latitude", statistic)])

# check 
# raster::plot(my_ras, zlim=c(0,0.06), col = my_col)

# save 
raster::writeRaster(my_ras, filename = file.path(in_path, "foi_map.tif"), format = "GTiff", overwrite = TRUE)

# test
# my_ras <- raster::raster(file.path("output", 
#                                    "predictions_world",
#                                    "bootstrap_models",
#                                    "model_21", 
#                                    "foi_map.tif"))
