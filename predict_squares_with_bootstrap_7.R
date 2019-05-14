# Makes a map of any output of the burden analysis

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
  id = 21,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  grid_size = 1 / 120,
  no_predictors = 26,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

vars_to_average <- "transformed_3_r_wolbachia_4"

statistic <- "mean"

n_col <- 100

FOI_z_range <- c(0, 0.06)
R0_1_z_range <- c(0, 8)
R0_2_z_range <- c(0, 4)
R0_3_z_range <- c(0, 5)

z_range <- R0_3_z_range

base_info <- c("cell",
               "latitude",
               "longitude",
               "population",
               "ID_0",
               "ID_1",
               "ID_2")


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


# load data -------------------------------------------------------------------


df_long_with_base_info <- readRDS(file.path(in_path, paste0(vars_to_average, ".rds")))


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

df_long_2 <- cbind(df_long_with_base_info[, base_info], df_long)

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")


# plot ------------------------------------------------------------------------ 


quick_raster_map(pred_df = df_long_2, 
                 variable = vars_to_average, 
                 statistic = statistic, 
                 my_col = my_col, 
                 out_pt = out_path, 
                 out_name = out_fl_nm,
                 z_range = z_range)
