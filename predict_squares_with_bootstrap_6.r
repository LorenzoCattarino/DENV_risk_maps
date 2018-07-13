# Makes a map of the square predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("data.table", "ggplot2", "fields", "rgdal", "scales", "RColorBrewer", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------  


parameters <- list(
  dependent_variable = "FOI",
  grid_size = 5)   

model_type_tag <- "_boot_model"

vars_to_average <- "response"

statistic <- "mean"


# define variables ------------------------------------------------------------


model_type <- paste0(parameters$dependent_variable, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     my_dir,
                     model_type)
  
out_path <- file.path("figures", 
                      "predictions_world",
                      "bootstrap_models",
                      my_dir,
                      model_type)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# pre processing -------------------------------------------------------------- 


mean_pred_fl_nm <- paste0(vars_to_average, "_mean", ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")

#names(df_long)[names(df_long) == "lat.grid"] <- "latitude"
#names(df_long)[names(df_long) == "long.grid"] <- "longitude"


# plot ------------------------------------------------------------------------ 


quick_raster_map(df_long, vars_to_average, statistic, out_path, out_fl_nm)
