# Makes a pretty map of the square predictions  

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "plot_map_pixel_level_ggplot.r"))
  
my_pkgs <- c("data.table", "ggplot2", "colorRamps", "raster", "rgdal", "scales")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_cw"

gr_size <- 20

res <- (1 / 120) * gr_size

cut_off <- 0

lats <- seq(-90, 90, by = res)
lons <- seq(-180, 180, by = res)

x <- file.path(
  "output",
  "predictions_world",
  model_tp,
  "pred_0_1667_deg_long.rds")

out_pt <- file.path(
  "figures", 
  "predictions_world",
  model_tp)

out_fl_nm <- "pred_0_1667_deg.png"


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}else{
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data 


all_preds <- readRDS(x)

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# ---------------------------------------- create matrix of foi values 


all_preds$lat.int=floor(all_preds$lat.grid*6+0.5)
all_preds$long.int=floor(all_preds$long.grid*6+0.5)
all_preds$foi=ifelse(all_preds$mean_pred < cut_off, 0,all_preds$mean_pred)

lats.int=lats*6
lons.int=lons*6

mat <- matrix(0, nrow = length(lons), ncol = length(lats))

i.lat <- findInterval(all_preds$lat.int, lats.int)
i.lon <- findInterval(all_preds$long.int, lons.int)

mat[cbind(i.lon, i.lat)] <- all_preds$foi


# ---------------------------------------- convert matrix to raster object


mat_ls <- list(x = lons,
               y = lats,
               z = mat)

r_mat <- raster(mat_ls)


#----------------------------------------- get raster extent 


my_ext <- matrix(r_mat@extent[], nrow = 2, byrow = TRUE) 


# ---------------------------------------- apply same extent to the shape file 


country_shp@bbox <- my_ext


# ---------------------------------------- mask the raster to the shape file


r_mat_msk <- mask(r_mat, country_shp)


# ---------------------------------------- convert to ggplot-friendly objects 


r_spdf <- as(r_mat_msk, "SpatialPixelsDataFrame")

r_df <- as.data.frame(r_spdf)

shp_fort <- fortify(country_shp)


# ---------------------------------------- submit job


if (CLUSTER) {
  
  foi_map <- obj$enqueue(map_data_pixel_ggplot(df = r_df, 
                                               shp = shp_fort, 
                                               out_path = out_pt, 
                                               out_file_name = out_fl_nm))
}  else {
  
  foi_map <- map_data_pixel_ggplot(df = r_df, 
                                   shp = shp_fort, 
                                   out_path = out_pt, 
                                   out_file_name = out_fl_nm)
  
}
