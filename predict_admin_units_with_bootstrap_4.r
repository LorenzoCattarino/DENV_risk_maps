# Makes a pretty map of the administrative unit predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "plotting", "functions_for_plotting_admin_level_maps.r"))

my_pkgs <- c("rgdal", "dplyr", "colorRamps", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# define parameters -----------------------------------------------------------  


model_tp <- "R0_3_boot_model"

vars <- "R0_r"

scenario_id <- 3

#pred_fl_name <- "R0_r_mean_all_adm1_3.rds"
pred_fl_name <- "R0_r_mean_all_squares_3.rds"

statistics <- c("median", "interv")

map_size <- "small"

out_pth <- file.path(
  "figures", 
  "predictions_world",
  model_tp)


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# create combination of factors ----------------------------------------------- 


fact_comb_FOI <- expand.grid(var = vars[vars == "FOI"], 
                             scenario_id = 1, 
                             statistic = statistics, 
                             stringsAsFactors = FALSE)

fact_comb_no_FOI <- expand.grid(var = vars[vars != "FOI"], 
                                scenario_id = scenario_id, 
                                statistic = statistics, 
                                stringsAsFactors = FALSE)

fact_comb <- rbind(fact_comb_FOI, fact_comb_no_FOI)


# load data ------------------------------------------------------------------- 


prediction_dat <- readRDS(
  file.path(
    "output", 
    "predictions_world", 
    model_tp, 
    pred_fl_name))

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras")

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = "gadm28_adm1_eras")


# pre processing -------------------------------------------------------------- 


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10),
  rev(heat.colors(10)))

adm_shp_fort <- fortify(adm_shp, region = "OBJECTID") %>% 
  mutate(id = as.numeric(id))

df_to_plot <- left_join(adm_shp_fort, prediction_dat, by = c("id" = "OBJECTID"))

fact_comb_ls <- df_to_list(fact_comb, use_names = TRUE)


# submit one job -------------------------------------------------------------- 


# t <- obj$enqueue(
#   wrapper_to_admin_map(
#     fact_comb_ls[1],
#     my_colors = col_ls, 
#     df_long = df_to_plot, 
#     country_shp = country_shp, 
#     out_path = out_pth, 
#     map_size= map_size))
    

# submit all jobs ------------------------------------------------------------- 


if(CLUSTER){
  
  admin_maps <- queuer::qlapply(
    fact_comb_ls,
    wrapper_to_admin_map,
    obj,
    my_colors = col_ls, 
    df_long = df_to_plot, 
    country_shp = country_shp, 
    out_path = out_pth, 
    map_size= map_size)
  
} else {
  
  admin_maps <- loop(
    fact_comb_ls[1],
    wrapper_to_admin_map,
    my_colors = col_ls, 
    df_long = df_to_plot, 
    country_shp = country_shp, 
    out_path = out_pth, 
    map_size= map_size,
    parallel = FALSE)

}
