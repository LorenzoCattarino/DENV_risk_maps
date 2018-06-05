
options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.R"))

my_pkgs <- c("colorRamps", "rgdal", "ggplot2", "raster")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "R0_3",
  resample_grid_size = 20,
  na_cutoff = 0)   

# statsc <- "best"
statsc <- "sd"

# ttl <- expression("R"[0])
ttl <- "SD"

# mean_pred_fl_nm <- "response.rds"
mean_pred_fl_nm <- "response_mean.rds"

map_projs <- c(NA, "+proj=robin", "+proj=moll")

out_file_names <- c("nice_map.png", "nice_map_robin.png", "nice_map_moll.png")


# define variables ------------------------------------------------------------  


# model_type <- paste0(parameters$dependent_variable, "_best_model")
model_type <- paste0(parameters$dependent_variable, "_boot_model")

# out_path <- file.path("figures", 
#                       "predictions_world", 
#                       "best_fit_models", 
#                       model_type)
out_path <- file.path("figures", 
                      "predictions_world", 
                      "bootstrap_models", 
                      "grid_size_interpolated",
                      model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  #config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


# df_long <- readRDS(file.path("output",
#                              "predictions_world",
#                              "best_fit_models",
#                              model_type,
#                              mean_pred_fl_nm))
df_long <- readRDS(file.path("output",
                             "predictions_world",
                             "bootstrap_models",
                             "grid_size_interpolated",
                             model_type,
                             mean_pred_fl_nm))

countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

bbox <- readOGR(dsn = file.path("data", "shapefiles", "ne_50m_graticules_all"), 
                layer = "ne_50m_wgs84_bounding_box") 


# remove the Caspian Sea ------------------------------------------------------ 


countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]


# submit one job --------------------------------------------------------------


names(df_long)[names(df_long) == "latitude"] <- "lat.grid"
names(df_long)[names(df_long) == "longitude"] <- "long.grid"

v_nice_map <- obj$enqueue(
  make_nice_map(seq_along(map_projs)[3],
                parameters,
                map_projs,
                countries,
                bbox,
                df_long,
                statsc,
                ttl,
                out_path,
                out_file_names))


# submit bundle ---------------------------------------------------------------


# if(CLUSTER){
# 
#   v_nice_map <- queuer::qlapply(
#     seq_along(map_projs),
#     make_nice_map,
#     obj,
#     parameters
#     map_projs,
#     countries,
#     bbox,
#     df_long,
#     statsc,
#     ttl,
#     out_path,
#     out_file_names)
# 
# } else {
# 
#   v_nice_map <- make_nice_map(
#     seq_along(map_projs)[3],
#     parameters,
#     map_projs,
#     countries,
#     bbox,
#     df_long,
#     statsc,
#     ttl,
#     out_path,
#     out_file_names)
#   
# }
