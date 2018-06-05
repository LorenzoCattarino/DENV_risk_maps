
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))

my_pkgs <- c("colorRamps", "rgdal", "ggplot2", "raster", "grid", "gridExtra")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  resample_grid_size = 20)   

map_proj <- "+proj=moll"


# define variables ------------------------------------------------------------  


out_file_name <- "pred_and_sd_map.png"
  
out_path <- file.path("figures", 
                      "predictions_world")


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


pred <- readRDS(file.path("output",
                           "predictions_world",
                           "best_fit_models",
                           "R0_3_best_model",
                           "response.rds"))

sd <- readRDS(file.path("output",
                         "predictions_world",
                         "bootstrap_models",
                         "grid_size_interpolated",
                         "R0_3_boot_model",
                         "response_mean.rds"))

countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

bbox <- readOGR(dsn = file.path("data", "shapefiles", "ne_50m_graticules_all"), 
                layer = "ne_50m_wgs84_bounding_box") 


# submit ----------------------------------------------------------------------


t <- obj$enqueue(make_nice_map(parameters,
                               map_proj,
                               countries,
                               bbox,
                               pred,
                               sd,
                               out_path,
                               out_file_name))
