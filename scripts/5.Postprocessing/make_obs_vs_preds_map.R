options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "plotting", "functions_for_plotting_admin_level_maps.r"))
  
my_pkgs <- c("data.table", "rgdal", "dplyr", "colorRamps", "ggplot2", "tidyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters -----------------------------------------------------------  


model_tp <- "R0_3_boot_model"

base_info <- c("ADM_0", "ADM_1", "o_j")

in_pt <- file.path(
  "output", 
  "EM_algorithm",
  model_tp,
  "predictions_data")

out_pth <- file.path(
  "figures", 
  "EM_algorithm",
  model_tp,
  "maps")

out_file_name <- "obs_vs_preds_map.png"

ttl <- expression('R'[0])
map_size <- "small"


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# load data -------------------------------------------------------------------


country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras")

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                     layer = "gadm28_adm1_eras",
                     stringsAsFactors = FALSE)

#adm_shp <- subset(adm_shp_1, NAME_0 == "Colombia")


# load prediction tables of each boot sample ----------------------------------


fi <- list.files(in_pt, 
                 pattern = ".",
                 full.names = TRUE)

all_samples <- loop(
  fi, 
  readRDS,
  parallel = FALSE)


# reshape ---------------------------------------------------------------------


all_samples_2 <- lapply(all_samples, average_data_points)

adm_preds <- lapply(all_samples_2, "[[", "admin")

adm_preds <- do.call("cbind", adm_preds)

sqr_preds <- lapply(all_samples_2, "[[", "square")

sqr_preds <- do.call("cbind", sqr_preds)

all_av_preds <- bind_cols(all_samples_2[[1]][, base_info], 
                          adm = rowMeans(adm_preds), 
                          sqr = rowMeans(sqr_preds))

# remove pseudo absences 
all_av_preds <- all_av_preds %>% 
  filter(o_j != 0.5)


# fortify shapefiles ----------------------------------------------------------


countries <- fortify(country_shp)

adm_shp_fort <- fortify(adm_shp, region = "OBJECTID") %>% 
  mutate(id = as.numeric(id))


# join fields to admin unit shp file ------------------------------------------


adm_shp@data$OBJECTID <- as.numeric(adm_shp@data$OBJECTID)
adm_shp@data$ID_0 <- as.numeric(adm_shp@data$ID_0)
adm_shp@data$ID_1 <- as.numeric(adm_shp@data$ID_1)

adm_shp_fort_2 <- left_join(adm_shp_fort, 
                            adm_shp@data, 
                            by = c("id" = "OBJECTID"))

names(adm_shp_fort_2)[names(adm_shp_fort_2) == "ID_0"] <- "ADM_0"

names(adm_shp_fort_2)[names(adm_shp_fort_2) == "ID_1"] <- "ADM_1"


# join predictions to admin unit shp file -------------------------------------


joint_dts <- inner_join(adm_shp_fort_2, all_av_preds, by = c("ADM_0", "ADM_1"))

df_to_plot <- melt(
  joint_dts,
  id.vars = c("long", "lat", "id", "group", "ADM_0", "ADM_1"),
  measure.vars = c("o_j", "adm", "sqr"))

levels(df_to_plot$variable) <- c("Observations", 
                                 "Predictions (Admin unit 1)", 
                                 "Predictions (20 km pixels)")


# submit job ------------------------------------------------------------------ 


my_col <- matlab.like(10)

if(CLUSTER){
  
  obs_vs_preds_map <- obj$enqueue(
    map_obs_and_preds(df = df_to_plot,
                      shp = countries,
                      var_to_plot = "value",
                      out_path = out_pth, 
                      out_file_name = out_file_name, 
                      my_col = my_col, 
                      ttl = ttl, 
                      map_size = map_size))

} else {

  obs_vs_preds_map <- map_obs_and_preds(df = df_to_plot,
                                        shp = countries,
                                        var_to_plot = "value",
                                        out_path = out_pth, 
                                        out_file_name = out_file_name, 
                                        my_col = my_col, 
                                        ttl = ttl, 
                                        map_size = map_size)
  
}
