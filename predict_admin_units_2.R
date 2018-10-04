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


var_to_fit <- "FOI"

adm_level <- 1

stat <- "adm"
#stat <- "aver_sqr"

my_prj <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs"


# define variables ------------------------------------------------------------


var_to_plot <- paste0("response_adm", adm_level)

model_type <- paste0(var_to_fit, "_best_model")

out_pth <- file.path("figures", 
                     "predictions_world",
                     "best_fit_models",
                     model_type)

out_nm <- paste0(var_to_plot, "_", stat, ".png")
  
pred_fl_name <- paste0(var_to_plot, ".rds")


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


prediction_dat <- readRDS(file.path("output", 
                                    "predictions_world", 
                                    "best_fit_models",
                                    model_type, 
                                    pred_fl_name))

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras")

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = paste0("gadm28_adm", adm_level, "_eras"))


# pre processing -------------------------------------------------------------- 


#adm_shp_prj <- spTransform(adm_shp, CRS(my_prj))

#country_shp_prj <- spTransform(country_shp, CRS(my_prj))

adm_shp_pred <- merge(adm_shp, 
                      prediction_dat[, c("OBJECTID", stat)], 
                      by = "OBJECTID", 
                      all.x = TRUE)


# plot ------------------------------------------------------------------------


quick_polygon_map(adm_shp_pred, country_shp, stat, out_pth, out_nm)
