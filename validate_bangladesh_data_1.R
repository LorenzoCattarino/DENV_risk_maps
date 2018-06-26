
# plot the Salje's seroprevalence points
# plot our cropped global prediction map
# extract and plot foi at the sero points coordinates
# find the admin unit 1 for each of sero points

library(ggplot2)
library(rgdal)
library(h2o)
library(raster)
library(dplyr)
library(colorRamps)
library(viridis)
library(rgeos)

source(file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))
source(file.path("R", "utility_functions.R"))

my_h2o_ver <- "3.16.0.2"

if(packageVersion("h2o") != my_h2o_ver) install.packages(file.path("R_sources", "h2o_3.16.0.2.tar.gz"), repos = NULL, type = "source")
                 
                 
# define parameters -----------------------------------------------------------


parameters <- list(resample_grid_size = 20)

alpha_iso_code <- "BGD"
adm0 <- 20

map_out_pt <- file.path("figures", "data", "salje")

dts_out_pt <- file.path("output", "seroprevalence", "salje") 
  
dts_out_nm <- "ProportionPositive_bangladesh_salje_env_var.csv"
  
my_col <- matlab.like(10)


# load data -------------------------------------------------------------------
  
  
salje_data <- read.csv(file.path("data",
                                 "seroprevalence",
                                 "ProportionPositive_bangladesh_salje.csv"))

shp <- readOGR(file.path("data", "shapefiles", "BGD_adm_shp"), "BGD_adm1",
               stringsAsFactors = FALSE,
               integer64 = "allow.loss")

pred <- readRDS(file.path("output",
                          "predictions_world", 
                          "best_fit_models",
                          "FOI_best_model",
                          "response.rds"))


# pre processing --------------------------------------------------------------


gr_size <- parameters$resample_grid_size

res <- (1 / 120) * gr_size
lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

shp_fort <- fortify(shp)

salje_data$id_point <- seq_len(nrow(salje_data))

salje_data$o_j <- salje_data$nPos / salje_data$nAll

salje_data$ISO <- alpha_iso_code


# plot the original seroprevalence points -------------------------------------


dir.create(map_out_pt, FALSE, TRUE)

png(file.path(map_out_pt, "salje_bangl_points_serop.png"),
    width = 12,
    height = 10,
    units = "cm",
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_path(data = shp_fort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_point(data = salje_data, aes(x = lon, y = lat, colour = o_j), size = 2) +
  coord_equal() + 
  scale_color_viridis("seroprevalence") +
  theme_minimal()

print(p)

dev.off()

# old style!
#
# par(mar = c(0,0,0,0), oma = c(0,0,0,0))
# plot(shp)
# points(xy_spdf, pch = 21, cex = 0.6, bg = "red")
# text(location_xy$lon, 
#      location_xy$lat, 
#      labels = salje_data$id_point, 
#      pos = 3, 
#      cex = 0.5)


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


png(file.path(map_out_pt, "predicted_FOI_map.png"),
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


png(file.path(map_out_pt, "salje_bangl_points_20km_foi.png"),
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


# find admin unit 1 -----------------------------------------------------------


location_xy <- salje_data[, c("lon", "lat")]
xy_spdf <- SpatialPoints(location_xy, proj4string = shp@proj4string)

overlay <- over(xy_spdf, shp)
country_numeric_code <- overlay$ID_0
adm_numeric_code <- overlay$ID_1
adm1_name <- overlay$NAME_1
country_name <- overlay$NAME_0
  
salje_data$country <- country_name
salje_data$ID_0 <- country_numeric_code
salje_data$ID_1 <- adm_numeric_code
salje_data$adm1 <- adm1_name

salje_data <- subset(salje_data, !is.na(ID_1))


# add adm unit 1 centroid coordinates -----------------------------------------


centroid_objs <- gCentroid(shp,byid=TRUE)

centroid_xy <- centroid_objs@coords

centroid_xy <- cbind(seq(nrow(centroid_xy)), centroid_xy)

colnames(centroid_xy) <- c("ID_1", "longitude", "latitude")

centroid_xy <- as.data.frame(centroid_xy)

salje_data <- left_join(salje_data, centroid_xy)


# save ------------------------------------------------------------------------


write_out_csv(salje_data, 
              dts_out_pt, 
              "predictions_20km.csv")
