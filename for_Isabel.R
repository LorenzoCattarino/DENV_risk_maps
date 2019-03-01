library(fields)

source(file.path("R", "prepare_datasets", "calculate_mean_across_fits.R"))
source(file.path("R", "plotting", "functions_for_plotting_raster_maps.r"))

in_path <- file.path("output", 
                     "predictions_world", 
                     "bootstrap_models", 
                     "model_24")

out_fig_path <- file.path("figures", 
                          "predictions_world", 
                          "bootstrap_models", 
                          "model_24")

preds <- readRDS(file.path(in_path, "transformed_r_wolbachia_3.rds"))

statistic <- "mean"

my_col <- colorRamps::matlab.like(100)

var_to_sum <- as.character(seq_len(200))

mean_preds <- average_boot_samples_dim2(preds[, var_to_sum])

mean_preds <- cbind(preds[, c("cell", "latitude", "longitude", "population", "ID_0", "ID_1", "ID_2")], mean_preds)

quick_raster_map(pred_df = mean_preds, 
                 statistic = statistic, 
                 my_col = my_col, 
                 out_pt = out_fig_path, 
                 out_name = "foi_from_predicted_R0_3.png",
                 z_range = c(0, 0.08))

my_ras <- raster::rasterFromXYZ(mean_preds[, c("longitude","latitude", statistic)])

# check 
# raster::plot(my_ras, zlim=c(0,0.06), col = my_col)

# save 
raster::writeRaster(my_ras, 
                    filename = file.path(in_path, "foi_from_predicted_R0_3.tif"), 
                    format = "GTiff", 
                    overwrite = TRUE)

# test
# my_ras <- raster::raster(file.path("output", 
#                                    "predictions_world",
#                                    "bootstrap_models",
#                                    "model_21", 
#                                    "foi_map.tif"))
