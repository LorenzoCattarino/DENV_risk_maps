# Makes a map of the square predictions and save the raster

source(file.path("R", "plotting", "functions_for_plotting_raster_maps.r"))

library(fields)
library(colorRamps)


# define parameters -----------------------------------------------------------  


parameters <- list(id = 2,
                   vars_to_plot = c("FOI", "R0_1","R0_2"),
                   z_range = list(FOI = c(0, 0.06),
                                  R0_1 = c(1, 7),
                                  R0_2 = c(1, 4),
                                  R0_3 = c(0, 5)),
                   save_raster = TRUE,
                   statistic = "mean",
                   vars_to_average = c("response", 
                                       "transformed_1_wolbachia_4",
                                       "transformed_2_wolbachia_4"))


# define variables ------------------------------------------------------------


vars_to_plot <- parameters$vars_to_plot

vars_to_average <- parameters$vars_to_average
  
statistic <- parameters$statistic

z_ranges <- parameters$z_range

save_raster <- parameters$save_raster 

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)
  
out_path <- file.path("figures", 
                      "predictions_world",
                      "bootstrap_models",
                      model_type)

my_col <- matlab.like(100)


# run ------------------------------------------------------------------------- 


for (i in seq_along(vars_to_plot)) {
  
  var_to_plot <- vars_to_plot[i]
  
  var_to_average <- vars_to_average[i]
  
  message(var_to_average)
  
  z_range <- z_ranges[[var_to_plot]]
  
  mean_pred_fl_nm <- paste0(var_to_average, "_mean", ".rds")
  
  df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))
  
  out_fl_nm <- paste0(var_to_average, "_", statistic, ".png")
  
  
  # plot ------------------------------------------------------------------------ 
  
  
  quick_raster_map(pred_df = df_long, 
                   variable = var_to_average, 
                   statistic = statistic, 
                   my_col = my_col, 
                   out_pt = out_path, 
                   out_name = out_fl_nm,
                   z_range = z_range)
  
  
  # save the raster -------------------------------------------------------------
  
  
  if (save_raster) {
    
    my_ras <- raster::rasterFromXYZ(df_long[, c("longitude","latitude", statistic)])
    
    # check 
    # raster::plot(my_ras, zlim=c(0,0.06), col = my_col)
    
    # save 
    raster::writeRaster(my_ras, 
                        filename = file.path(out_path, 
                                             paste0(var_to_plot, "_map.tif")), 
                        format = "GTiff", 
                        overwrite = TRUE)
    
    # test
    # my_ras <- raster::raster(file.path("figures",
    #                                    "predictions_world",
    #                                    "bootstrap_models",
    #                                    "model_2",
    #                                    "R0_1_map.tif"))
    
  }
  
}
