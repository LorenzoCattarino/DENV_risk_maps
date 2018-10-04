# Makes a map of the administrative unit predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "plotting", "quick_polygon_map.R"))

my_pkgs <- c("rgdal", "colorRamps", "lattice", "grid")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters -----------------------------------------------------------  


parameters <- list(
  id = 14,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "R0_1",
  pseudoAbs_value = 0.5,
  grid_size = 5,
  no_predictors = 23,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

vars_to_average <- "response"

statistic <- "mean"

n_col <- 100

adm_level <- 1


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")

out_pth <- file.path("figures", 
                     "predictions_world",
                     "bootstrap_models",
                     model_type,
                     "adm_1")


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras",
                       stringsAsFactors = FALSE)

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm", adm_level, "_eras"),
                   stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_col <- matlab.like(n_col)

mean_pred_fl_nm <- paste0(vars_to_average, "_mean", ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_average, "_", statistic, ".png")

#df_long <- df_long[!duplicated(df_long[, c("ID_0", "ID_1")]), ]

adm_shp_pred <- merge(adm_shp, 
                      df_long[, c("ID_0", "ID_1", statistic)], 
                      by = c("ID_0", "ID_1"), 
                      all.x = TRUE)


# plot ------------------------------------------------------------------------


quick_polygon_map(adm_shp_pred, country_shp, my_col, statistic, out_pth, out_fl_nm)
