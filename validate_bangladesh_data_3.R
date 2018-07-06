
# plot our cropped global prediction map
# extract and plot foi at the sero points coordinates
# calculate correlation with our 20 km predictions

library(rgdal)
library(raster)
library(colorRamps)
library(ggplot2)
library(viridis)

source(file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))
source(file.path("R", "utility_functions.R"))


# define parameters -----------------------------------------------------------


parameters <- list(resample_grid_size = 20)

adm0 <- 20

my_col <- matlab.like(10)

dts_out_pt <- file.path("output", "seroprevalence", "salje")

sctplot_out_pt <- file.path("figures", "data", "salje")

model_tp <- "FOI_best_model_2"

sctplot_out_nm <- "correlation_20km_pred_vs_observations_2.png"

pred_map_out_nm <- "predicted_FOI_map_2.png"

pred_points_out_nm <- "salje_bangl_points_20km_foi_2.png"


# load data -------------------------------------------------------------------


pred <- readRDS(file.path("output",
                          "predictions_world", 
                          "best_fit_models",
                          model_tp,
                          "response.rds"))

salje_data <- read.csv(file.path("output", 
                                 "seroprevalence",
                                 "salje",
                                 "observations_20km.csv"),
                       stringsAsFactors = FALSE)

shp <- readOGR(file.path("data", "shapefiles", "BGD_adm_shp"), "BGD_adm1",
               stringsAsFactors = FALSE,
               integer64 = "allow.loss")


# pre processing --------------------------------------------------------------


shp_fort <- fortify(shp)

gr_size <- parameters$resample_grid_size

res <- (1 / 120) * gr_size
lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

names(pred)[names(pred) == "latitude"] <- "lat.grid"
names(pred)[names(pred) == "longitude"] <- "long.grid"


# crop the global prediction map ----------------------------------------------


pred_mat <- prediction_df_to_matrix(lats, lons, pred, "best")

pred_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred_r_mat <- raster(pred_mat_ls)

pred_r_mat_crp <- raster::crop(pred_r_mat, shp)

pred_r_mat_msk <- raster::mask(pred_r_mat_crp, shp)

pred_r_spdf <- as(pred_r_mat_msk, "SpatialPixelsDataFrame")

pred_r_df <- as.data.frame(pred_r_spdf)


# plot the cropped global prediction map --------------------------------------


png(file.path(sctplot_out_pt, pred_map_out_nm),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_tile(data = pred_r_df, aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = my_col, 
                       guide = guide_colourbar(title = "predicted FOI")) +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  scale_x_continuous("long") +
  scale_y_continuous("lat") +
  coord_equal() +
  theme_minimal()

print(p)

dev.off()


# extract foi at the sero points coordinates ----------------------------------


my_points <- as.matrix(salje_data[,c("lon", "lat")])

cell_ids_in_raster <- cellFromXY(pred_r_mat, my_points)

raster_values <- pred_r_mat[cell_ids_in_raster]

salje_data$foi_sqr <- raster_values


# plot the 20 km foi at the sero points ---------------------------------------


png(file.path(sctplot_out_pt, pred_points_out_nm),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_point(data = salje_data, aes(x = lon, y = lat, colour = foi_sqr), size = 2) +
  coord_equal() + 
  scale_color_viridis("20 km FOI") +
  theme_minimal()

print(p)

dev.off()


# plot the scatter plot -------------------------------------------------------


corr_coeff <- round(cor(salje_data$FOI, salje_data$foi_sqr), 3)

dir.create(sctplot_out_pt, FALSE, TRUE)

png(file.path(sctplot_out_pt, sctplot_out_nm),
    width = 15,
    height = 8,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_point(aes(x = FOI, y = foi_sqr, colour = "red"), data = salje_data, size = 1) +
  scale_colour_identity(name = "", guide = "legend", labels = "salje") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_text(aes(x = 0.03, y = 0.005, label = paste0("r = ", corr_coeff))) +
  scale_x_continuous("observed 20 km FOI", limits = c(0, 0.045)) +
  scale_y_continuous("predicted 20 km FOI", limits = c(0, 0.045))

print(p)

dev.off()


# save ------------------------------------------------------------------------


write_out_csv(salje_data, dts_out_pt, "predictions_20km.csv")

