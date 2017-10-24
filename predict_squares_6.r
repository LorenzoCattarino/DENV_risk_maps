# Makes a pretty map of the square predictions  

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.r"))

my_pkgs <- c("data.table", "ggplot2", "colorRamps", "raster", "rgdal", "scales", "RColorBrewer")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_3"

base_info <- c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2") 


# ---------------------------------------- define variables


vars <- "FOI_r"
  
scenario <- 1
  
statistics <-  c("mean", "sd", "interv", "lCI", "uCI")

all_titles <- c("mean", "SD", "quantile_diff", "2.5_quantile", "97.5_quantile")

do.p9.logic <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

plot_wdt <- 28 #or: 7
plot_hgt <- 12 #or: 3

out_pt <- file.path(
  "figures", 
  "predictions_world",
  model_tp)
  
  
# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  context::parallel_cluster_start(3, ctx)
}


# ---------------------------------------- create color palette


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10))


# ---------------------------------------- load data 


if(vars == "FOI"){
  
  mean_pred_fl_nm <- paste0(vars, "_mean_all_squares_0_1667_deg.rds")
  
} else {
  
  mean_pred_fl_nm <- paste0(vars, "_mean_all_squares_0_1667_deg_", scenario, ".rds")
    
}

mean_preds <- readRDS(
  file.path(
    "output",
    "predictions_world",
    model_tp,
    "means",
    mean_pred_fl_nm))

all_sqr_covariates <- readRDS(
  file.path(
    "output", 
    "env_variables", 
    "all_squares_env_var_0_1667_deg.rds"))

if(vars == "FOI"){
  
  data_all <- cbind(all_sqr_covariates[, base_info], mean_preds)

} else {
  
  data_all <- mean_preds
  
}

data_all$interv <- data_all$uCI - data_all$lCI 

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# ---------------------------------------- convert to ggplot-friendly object 


shp_fort <- fortify(country_shp)


# ------------------------------------------ submit one job 


# t <- obj$enqueue(
#   wrapper_to_ggplot_map(
#     seq_along(statistics)[1],
#     vars = vars,
#     statistics = statistics,
#     my_colors = col_ls,
#     titles_vec = all_titles,
#     df_long = data_all,
#     country_shp = country_shp,
#     shp_fort = shp_fort,
#     out_path = out_pt,
#     do.p9.logic = do.p9.logic))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  maps <- queuer::qlapply(
    seq_along(statistics),
    wrapper_to_ggplot_map,
    obj,
    vars = vars,
    statistics = statistics,
    my_colors = col_ls,
    titles_vec = all_titles,
    df_long = data_all,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    do.p9.logic = do.p9.logic,
    plot_wdt = plot_wdt,
    plot_hgt = plot_hgt)

} else {

  maps <- loop(
    seq_along(statistics)[1],
    wrapper_to_ggplot_map,
    vars = vars,
    statistics = statistics,
    my_colors = col_ls,
    titles_vec = all_titles,
    df_long = data_all,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    do.p9.logic = do.p9.logic,
    plot_wdt = plot_wdt,
    plot_hgt = plot_hgt,
    parallel = TRUE)

}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
