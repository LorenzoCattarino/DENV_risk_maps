# Makes a pretty map of the square predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.r"))

my_pkgs <- c("data.table", "ggplot2", "colorRamps", "raster", "rgdal", "scales", "RColorBrewer")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_2"


# ---------------------------------------- define variables


vars <- "I_inc"

scenario_id <- 4:9
  
statistics <- "mean"
#statistics <- c("mean", "sd", "interv", "lCI", "uCI")

fact_comb <- expand.grid(vars = vars, scenario_id = scenario_id, statistics = statistics, stringsAsFactors = FALSE)

plot_wdt <- 12 # or: 8 # or: 28
plot_hgt <- 6 # or: 4 # or: 12

out_pt <- file.path(
  "figures", 
  "predictions_world",
  model_tp)
  
  
# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  #config <- didehpc::didehpc_config(template = "12Core")
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  context::parallel_cluster_start(6, ctx)
}


# ---------------------------------------- create color palette


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10))


# ---------------------------------------- load data 


country_shp <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# ---------------------------------------- convert to ggplot-friendly object 


shp_fort <- fortify(country_shp)


# ----------------------------------------


fact_comb_ls <- df_to_list(fact_comb, use_names = TRUE)
  
  
# ------------------------------------------ submit one job 


# t1 <- obj$enqueue(
#   wrapper_to_ggplot_map(
#     fact_comb_ls[1],
#     my_colors = col_ls,
#     model_tp = model_tp,
#     country_shp = country_shp,
#     shp_fort = shp_fort,
#     out_path = out_pt,
#     plot_wdt = plot_wdt,
#     plot_hgt = plot_hgt))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  maps <- queuer::qlapply(
    fact_comb_ls,
    wrapper_to_ggplot_map,
    obj,
    my_colors = col_ls,
    model_tp = model_tp,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    plot_wdt = plot_wdt,
    plot_hgt = plot_hgt)

} else {

  map_1 <- loop(
    fact_comb_ls,
    wrapper_to_ggplot_map,
    my_colors = col_ls,
    model_tp = model_tp,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    plot_wdt = plot_wdt,
    plot_hgt = plot_hgt,
    parallel = TRUE)

}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
