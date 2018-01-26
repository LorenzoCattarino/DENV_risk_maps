options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.r"))

my_pkgs <- c("colorRamps", "rgdal", "ggplot2", "raster")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}else{
  
  context::context_load(ctx)
  
}


# define parameters ----------------------------------------------------------- 


model_tp <- "R0_3_boot_model"

mean_pred_fl_nm <- "R0_r_mean_all_squares_3.rds"

map_projs <- c(NA, "+proj=robin", "+proj=moll")

out_file_names <- c("nice_map.png", "nice_map_robin.png", "nice_map_moll.png")

my_col <- matlab.like(10)

statsc <- "median"

out_path <- file.path("figures", "predictions_world", model_tp)

ttl <- expression('R'[0])

na_cutoff <- 1


# load data ------------------------------------------------------------------- 


df_long <- readRDS(
  file.path(
    "output",
    "predictions_world",
    model_tp,
    mean_pred_fl_nm))

countries <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm0_eras")

bbox <- readOGR(dsn = file.path("data", "shapefiles", "ne_50m_graticules_all"), 
                layer = "ne_50m_wgs84_bounding_box") 


# remove the Caspian Sea ------------------------------------------------------ 


countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]


# submit one job --------------------------------------------------------------


v_nice_map <- obj$enqueue(
    make_very_nice_map(seq_along(map_projs)[3],
                       map_projs,
                       countries,
                       bbox,
                       df_long,
                       statsc,
                       na_cutoff,
                       out_path,
                       out_file_names))

  
# submit bundle ---------------------------------------------------------------


# if(CLUSTER){
# 
#   v_nice_map <- queuer::qlapply(
#     seq_along(map_projs),
#     make_very_nice_map,
#     obj,
#     map_projs,
#     countries,
#     bbox,
#     df_long,
#     statsc,
#     na_cutoff,
#     out_path,
#     out_file_names)
#   
# } else {
#   
#   v_nice_map <- make_very_nice_map(seq_along(map_projs)[1],
#                                    map_projs,
#                                    countries,
#                                    bbox,
#                                    df_long,
#                                    statsc,
#                                    na_cutoff,
#                                    out_path,
#                                    out_file_names)
#   
# }
