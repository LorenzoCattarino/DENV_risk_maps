# Makes a map of the square predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_raster_maps.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("data.table", "ggplot2", "fields", "rgdal", "scales", "RColorBrewer", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------  


parameters <- list(
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

vars_to_average <- "response"

statistic <- "median"

n_col <- 100


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type)
  
out_path <- file.path("figures", 
                      "predictions_world",
                      "bootstrap_models",
                      model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# pre processing -------------------------------------------------------------- 


my_col <- matlab.like(n_col)

mean_pred_fl_nm <- paste0(vars_to_average, "_mean", ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")


# plot ------------------------------------------------------------------------ 


quick_raster_map(df_long, vars_to_average, statistic, my_col, out_path, out_fl_nm, c(0, 0.06))
