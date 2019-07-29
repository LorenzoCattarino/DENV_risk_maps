
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "plotting", "nice_raster_map.R"))

my_pkgs <- c("colorRamps", "rgdal", "ggplot2", "raster", "grid", "gridExtra")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(id = 4,
                   resample_grid_size = 20,
                   plot_wdt = 17,
                   plot_hgt = 11, 
                   barwdt = 1.5,
                   barhgt = 5,
                   pol_brd_sz = 0.1,
                   leg_pos_x = 0.07,
                   leg_pos_y = 0.35,
                   leg_txt_sz = 10,
                   leg_ttl_sz = 12,
                   map_proj = "+proj=moll")


# define variables ------------------------------------------------------------  


model_type <- paste0("model_", parameters$id)

gr_size <- parameters$resample_grid_size

map_proj <- parameters$map_proj

out_file_name <- "pred_and_sd_map.png"
  
out_path <- file.path("figures", "predictions_world")


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


pred <- readRDS(file.path("output",
                          "predictions_world",
                          "bootstrap_models",
                          model_type,
                          "response_mean.rds"))

sd <- readRDS(file.path("output",
                        "predictions_world",
                        "bootstrap_models",
                        "grid_size_interpolated",
                        "response_mean.rds"))

countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

bbox <- readOGR(dsn = file.path("data", "shapefiles", "ne_50m_graticules_all"), 
                layer = "ne_50m_wgs84_bounding_box") 


# pre processing -------------------------------------------------------------- 


my_ext <- extent(-130, 180, -60, 38)

res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]

fl_ex <- file.exists(file.path("output", "shapefiles", "gadm28_adm0_eras_cropped.shp"))

if(fl_ex){
  
  countries_cropped <- readOGR(dsn = file.path("output", "shapefiles"),
                               layer = "gadm28_adm0_eras_cropped",
                               stringsAsFactors = FALSE,
                               integer64 = "allow.loss")
  
} else {
  
  countries_cropped <- crop(countries, my_ext)
  
  writeOGR(countries_cropped, 
           dsn = file.path("output", "shapefiles"), 
           layer = "gadm28_adm0_eras_cropped", 
           driver = "ESRI Shapefile")
  
}


# countries <- spTransform(countries_cropped, CRS(map_proj))

countries_df <- fortify(countries_cropped)

bbox <- crop(bbox, my_ext)

# bbox <- spTransform(bbox, CRS(map_proj))
  
bbox_df <- fortify(bbox)

pred_mat <- prediction_df_to_matrix(lats, lons, pred, "mean")

pred_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred_r_mat <- raster(pred_mat_ls)

pred_r_mat <- crop(pred_r_mat, my_ext)

# pred_r_mat <- projectRaster(pred_r_mat, crs = map_proj)

pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")

pred_r_df <- as.data.frame(pred_r_spdf)

# pred_r_df <- subset(pred_r_df, layer >= 1)


# -----------------------------------------------------------------------------
# 
# cap the sd 
# 
# -----------------------------------------------------------------------------


# sd$sd <- ifelse(sd$sd > 1, 1, sd$sd)


# -----------------------------------------------------------------------------


sd_mat <- prediction_df_to_matrix(lats, lons, sd, "sd")

sd_mat_ls <- list(x = lons,
                  y = lats,
                  z = sd_mat)

sd_r_mat <- raster(sd_mat_ls)

sd_r_mat <- crop(sd_r_mat, my_ext)

# sd_r_mat <- projectRaster(sd_r_mat, crs = map_proj)

sd_r_spdf <- as(sd_r_mat, "SpatialPixelsDataFrame")

sd_r_df <- as.data.frame(sd_r_spdf)


# submit ----------------------------------------------------------------------


if (CLUSTER){
  
  t <- obj$enqueue(make_nice_map(parameters,
                                 countries_df,
                                 bbox_df,
                                 pred_r_df,
                                 sd_r_df,
                                 out_path,
                                 out_file_name))
  
} else {
  
  make_nice_map(parameters,
                countries_df,
                bbox_df,
                pred_r_df,
                sd_r_df,
                out_path,
                out_file_name)
  
}
