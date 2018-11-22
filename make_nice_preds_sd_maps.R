
options(didehpc.cluster = "fi--dideclusthn")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "plotting", "nice_raster_map.R"))

my_pkgs <- c("colorRamps", "rgdal", "ggplot2", "raster", "grid", "gridExtra")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  resample_grid_size = 20,
  plot_wdt = 17,
  plot_hgt = 17, 
  barwdt = 1.5,
  barhgt = 6,
  pol_brd_sz = 0.1,
  leg_pos_x = 0.10,
  leg_pos_y = 0.3,
  leg_txt_sz = 10,
  leg_ttl_sz = 12,
  map_proj = "+proj=moll")


# define variables ------------------------------------------------------------  


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
                          "model_16",
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


res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]

countries <- spTransform(countries, CRS(map_proj))

countries_df <- fortify(countries)

bbox <- spTransform(bbox, CRS(map_proj))

bbox_df<- fortify(bbox)

pred_mat <- prediction_df_to_matrix(lats, lons, pred, "mean")

pred_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred_r_mat <- raster(pred_mat_ls)

pred_r_mat <- projectRaster(pred_r_mat, crs = map_proj)

pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")

pred_r_df <- as.data.frame(pred_r_spdf)

pred_r_df <- subset(pred_r_df, layer >= 1)


# -----------------------------------------------------------------------------
# 
# cap the sd 
# 
# -----------------------------------------------------------------------------


sd$sd <- ifelse(sd$sd > 1, 1, sd$sd)


# -----------------------------------------------------------------------------


sd_mat <- prediction_df_to_matrix(lats, lons, sd, "sd")

sd_mat_ls <- list(x = lons,
                  y = lats,
                  z = sd_mat)

sd_r_mat <- raster(sd_mat_ls)

sd_r_mat <- projectRaster(sd_r_mat, crs = map_proj)

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
