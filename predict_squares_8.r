# Makes a pretty map of the square predictions  

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "plotting", "functions_for_plotting_square_level_maps_ggplot.r"))

my_pkgs <- c("data.table", "ggplot2", "colorRamps", "raster", "rgdal", "scales", "RColorBrewer")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_2"

base_info <- c("cell", "lat.grid", "long.grid", "population", "ADM_0", "ADM_1", "ADM_2") 


# ---------------------------------------- define variables


out_pt <- file.path(
  "figures", 
  "predictions_world",
  model_tp)

vars <-  c("mean", "sd", "interv", "lCI", "uCI")

all_titles <- c("FOI", "SD", "quantile_diff", "2.5_quantile", "97.5_quantile")

do.p9.logic <- c(FALSE, FALSE, FALSE, FALSE, FALSE)


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


mean_FOI <- readRDS(
  file.path(
    "output",
    "predictions_world",
    model_tp,
    "means",
    "FOI_mean_all_squares_0_1667_deg.rds"))

all_sqr_covariates <- readRDS(
  file.path(
    "output", 
    "env_variables", 
    "all_squares_env_var_0_1667_deg.rds"))

all_preds <- cbind(all_sqr_covariates[, base_info], mean_FOI)

all_preds$interv <- all_preds$uCI - all_preds$lCI 

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# ---------------------------------------- convert to ggplot-friendly object 


shp_fort <- fortify(country_shp)


# ------------------------------------------ submit one job 


# t <- obj$enqueue(
#   wrapper_to_ggplot_map(
#     seq_along(vars)[1],
#     vars = vars,
#     my_colors = col_ls,
#     titles_vec = all_titles,
#     df_long = all_preds,
#     country_shp = country_shp,
#     shp_fort = shp_fort,
#     out_path = out_pt,
#     do.p9.logic = do.p9.logic))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  maps <- queuer::qlapply(
    seq_along(vars),
    wrapper_to_ggplot_map,
    obj,
    vars = vars,
    my_colors = col_ls,
    titles_vec = all_titles,
    df_long = all_preds,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    do.p9.logic = do.p9.logic)
  
} else {
  
  maps <- loop(
    seq_along(vars)[3:5],
    wrapper_to_ggplot_map,
    vars = vars,
    my_colors = col_ls,
    titles_vec = all_titles,
    df_long = all_preds,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    do.p9.logic = do.p9.logic,
    parallel = TRUE)
  
}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
