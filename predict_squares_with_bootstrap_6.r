# Makes a map of the square predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.r"))

my_pkgs <- c("data.table", "ggplot2", "fields", "rgdal", "scales", "RColorBrewer", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------  


parameters <- list(
  grid_size = 1,
  resample_grid_size = 20,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 9)   

var_to_fit <- "FOI"

vars_to_average <- "response"

statistics <- "CV"

map_size <- "small"

in_dts_tag <- "CV"


# define variables ------------------------------------------------------------


model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

in_path <- file.path("output", 
                     "predictions_world", 
                     "bootstrap_models")#,
                     # my_dir,
                     # model_type)
  
out_path <- file.path("figures", 
                      "predictions_world",
                      "bootstrap_models")#,
                      # my_dir,
                      # model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


# countries <- readOGR(dsn = file.path("output", "shapefiles"), 
#                      layer = "gadm28_adm0_eras")


# pre processing -------------------------------------------------------------- 


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  colorRamps::matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10))

# countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]


# create combination of factors -----------------------------------------------

# 
# fact_comb_FOI <- expand.grid(vars_to_average = vars_to_average[vars_to_average == "response"], 
#                              scenario_id = 1, 
#                              statistics = statistics, 
#                              stringsAsFactors = FALSE)
# 
# fact_comb_no_FOI <- expand.grid(vars_to_average = vars_to_average[vars_to_average != "response"], 
#                                 scenario_id = scenario_id, 
#                                 statistics = statistics, 
#                                 stringsAsFactors = FALSE)
# 
# fact_comb <- rbind(fact_comb_FOI, fact_comb_no_FOI)
# 
# fact_comb_ls <- df_to_list(fact_comb, use_names = TRUE)
  
  
# submit one job --------------------------------------------------------------


# t <- obj$enqueue(
#   wrapper_to_square_map(
#     fact_comb_ls[[1]],
#     my_colors = col_ls,
#     model_tp = model_type,
#     shp_fl = countries,
#     out_path = out_path,
#     map_size = map_size,
#     in_dts_tag = in_dts_tag,
#     in_path = in_path))


# submit all jobs ------------------------------------------------------------- 


# if (CLUSTER) {
# 
#   maps <- queuer::qlapply(
#     fact_comb_ls,
#     wrapper_to_square_map,
#     obj,
#     my_colors = col_ls,
#     model_tp = model_type,
#     shp_fl = countries,
#     out_path = out_path,
#     map_size = map_size,
#     in_dts_tag = in_dts_tag,
#     in_path = in_path)
# 
# } else {
# 
#   maps <- loop(
#     fact_comb_ls[1],
#     wrapper_to_square_map,
#     my_colors = col_ls,
#     model_tp = model_type,
#     shp_fl = countries,
#     out_path = out_path,
#     map_size = map_size,
#     in_dts_tag = in_dts_tag,
#     in_path = in_path,
#     parallel = FALSE)
# 
# }
# 
# if(!CLUSTER){
#   context::parallel_cluster_stop()
# }


# =============================================================================
#
# new script for quick mapping 
#
# =============================================================================


mean_pred_fl_nm <- paste0(vars_to_average, "_", in_dts_tag, ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistics, ".png")

names(df_long)[names(df_long) == "lat.grid"] <- "latitude"
names(df_long)[names(df_long) == "long.grid"] <- "longitude"

quick_raster_map(df_long, statistics, out_path, out_fl_nm)
