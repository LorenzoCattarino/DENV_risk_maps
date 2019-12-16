
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "create_parameter_list.R"),
  file.path("R", "plotting", "make_nice_map.R"),
  file.path("R", "plotting", "make_nice_2_stacked_maps_figure.R"),
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"))

my_pkgs <- c("colorRamps", "sf", "raster", "ggplot2", "grid", "gridExtra", "dplyr", "shades")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 2,
                   plot_wdt = 18.4,
                   plot_hgt = 12.2, 
                   barwdt = 1.5,
                   barhgt = 4.5,
                   pol_brd_sz = 0.1,
                   leg_pos_x = 0,
                   leg_pos_y = 0,
                   leg_txt_sz = 10,
                   leg_ttl_sz = 12,
                   map_proj = "+proj=moll",
                   coord_limits = c(-130, 180, -60, 38),
                   ID_0_to_remove = c(1, 69, 171, 122, 200, 224, 226, 235, 236, 244, 246))

# my_col <- colorRamps::matlab.like(100)
# my_col <- colorspace::diverge_hcl(11,
#                                   h = c(250, 10),
#                                   c = 100,
#                                   l = c(37, 88),
#                                   power = c(0.7, 1.7))
# my_col <- rev(brewer.pal(n = 11, name = "RdBu"))
# my_col <- diverge_hcl(100, palette = "Blue-Red")
# my_col <- diverge_hcl(100, palette = "Blue-Red 2")
# my_col <- diverge_hcl(100, palette = "Blue-Red 3")
# my_col <- divergingx_hcl(n = 100, palette = "RdYlBu")

my_col <- creating_color_palette(100)


# define variables ------------------------------------------------------------  


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

gr_size <- parameters$resample_grid_size

out_file_name <- "pred_and_sd_map_hcl_col_brew_sat4.png"
  
out_path <- file.path("figures", "predictions_world")

coord_limits <- parameters$coord_limits

ID_0_to_remove <- parameters$ID_0_to_remove


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

countries <- st_read(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

endemic_ID_0_ID_1 <- read.csv(file.path("output", 
                                "datasets", 
                                "dengue_endemic_ID_0_ID_1.csv"),
                      stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


res <- (1 / 120) * gr_size

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

endemic_ID_0_ID_1 <- endemic_ID_0_ID_1[!endemic_ID_0_ID_1$ID_0 %in% ID_0_to_remove,]
  
countries <- countries[!countries$NAME_ENGLI == "Caspian Sea", ]

# remove pixels outside of endemic ID_0 and ID_1 
pred2 <- dplyr::inner_join(pred, endemic_ID_0_ID_1)

pred_mat <- prediction_df_to_matrix(lats, lons, pred2, "mean")

pred_mat_ls <- list(x = lons,
                    y = lats,
                    z = pred_mat)

pred_r_mat <- raster(pred_mat_ls)

pred_r_spdf <- as(pred_r_mat, "SpatialPixelsDataFrame")

pred_r_df <- as.data.frame(pred_r_spdf)

# remove pixels outside of endemic ID_0 and ID_1 
sd2 <- dplyr::inner_join(sd, endemic_ID_0_ID_1)

sd_mat <- prediction_df_to_matrix(lats, lons, sd2, "sd")

sd_mat_ls <- list(x = lons,
                  y = lats,
                  z = sd_mat)

sd_r_mat <- raster(sd_mat_ls)

sd_r_spdf <- as(sd_r_mat, "SpatialPixelsDataFrame")

sd_r_df <- as.data.frame(sd_r_spdf)


# submit ----------------------------------------------------------------------


if (CLUSTER){
  
  t <- obj$enqueue(make_nice_2_stacked_maps_figure(parameters,
                                                   countries,
                                                   bbox,
                                                   pred_r_df,
                                                   sd_r_df,
                                                   out_path,
                                                   out_file_name,
                                                   my_col))
  
} else {
  
  make_nice_2_stacked_maps_figure(parameters,
                                  countries,
                                  bbox,
                                  pred_r_df,
                                  sd_r_df,
                                  out_path,
                                  out_file_name,
                                  my_col)
  
}
