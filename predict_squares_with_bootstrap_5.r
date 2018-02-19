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


model_tp <- "R0_3_boot_model"

vars <- c("FOI")

scenario_id <- NA
  
statistics <- c("median", "interv")
#statistics <- c("mean", "median, "sd", "interv", "lCI", "uCI")

map_size <- "small"

out_pt <- file.path(
  "figures", 
  "predictions_world",
  model_tp)
  
in_dts_tag <- "mean_all_squares"


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  #context::parallel_cluster_start(4, ctx)
}


# ---------------------------------------- create combination of factors


fact_comb_FOI <- expand.grid(vars = vars[vars == "FOI"], 
                             scenario_id = 1, 
                             statistics = statistics, 
                             stringsAsFactors = FALSE)

fact_comb_no_FOI <- expand.grid(vars = vars[vars != "FOI"], 
                                scenario_id = scenario_id, 
                                statistics = statistics, 
                                stringsAsFactors = FALSE)

fact_comb <- rbind(fact_comb_FOI, fact_comb_no_FOI)


# ---------------------------------------- create color palette


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10))


# ---------------------------------------- load data 


countries <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


countries <- countries[!countries@data$NAME_ENGLI == "Caspian Sea", ]


# ----------------------------------------


fact_comb_ls <- df_to_list(fact_comb, use_names = TRUE)
  
  
# ------------------------------------------ submit one job 


# t <- obj$enqueue(
#   wrapper_to_square_map(
#     fact_comb_ls[[1]],
#     my_colors = col_ls,
#     model_tp = model_tp,
#     shp_fl = countries,
#     out_path = out_pt,
#     map_size = map_size,
#     in_dts_tag = in_dts_tag))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  maps <- queuer::qlapply(
    fact_comb_ls,
    wrapper_to_square_map,
    obj,
    my_colors = col_ls,
    model_tp = model_tp,
    shp_fl = countries,
    out_path = out_pt,
    map_size = map_size,
    in_dts_tag = in_dts_tag)

} else {

  maps <- loop(
    fact_comb_ls[1],
    wrapper_to_square_map,
    my_colors = col_ls,
    model_tp = model_tp,
    shp_fl = countries,
    out_path = out_pt,
    map_size = map_size,
    in_dts_tag = in_dts_tag,
    parallel = FALSE)

}

if(!CLUSTER){
  context::parallel_cluster_stop()
}
