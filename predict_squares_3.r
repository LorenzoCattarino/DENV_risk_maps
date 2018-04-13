# Makes a pretty map of the square predictions

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "functions_for_plotting_square_level_maps.r"))

my_pkgs <- c("data.table", "ggplot2", "fields", "rgdal", "scales", "RColorBrewer", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

vars_to_plot <- "response"
  
statistics <- "best"


# define variables ------------------------------------------------------------


model_type <- paste0(var_to_fit, "_best_model")

in_path <- file.path("output", 
                     "predictions_world", 
                     "best_fit_models",
                     model_type)

out_pt <- file.path("figures", 
                    "predictions_world",
                    "best_fit_models",
                    model_type)

in_dts_tag <- "best_all_squares"


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)

}


# load data ------------------------------------------------------------------- 


country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras")


# pre processing -------------------------------------------------------------- 


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10))

# country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# # ---------------------------------------- create combination of factors
# 
# 
# fact_comb_FOI <- expand.grid(vars = vars[vars == "FOI"], 
#                              scenario_id = 1, 
#                              statistics = statistics, 
#                              stringsAsFactors = FALSE)
# 
# fact_comb_no_FOI <- expand.grid(vars = vars[vars != "FOI"], 
#                                 scenario_id = scenario_id, 
#                                 statistics = statistics, 
#                                 stringsAsFactors = FALSE)
# 
# fact_comb <- rbind(fact_comb_FOI, fact_comb_no_FOI)
# 
# 
# # ---------------------------------------- convert to ggplot-friendly object 
# 
# 
# shp_fort <- fortify(country_shp)
# 
# 
# # ----------------------------------------
# 
# 
# fact_comb_ls <- df_to_list(fact_comb, use_names = TRUE)
#   
#   
# # ------------------------------------------ submit one job 
# 
# 
# # t1 <- obj$enqueue(
# #   wrapper_to_ggplot_map(
# #     fact_comb_ls[1],
# #     my_colors = col_ls,
# #     model_tp = model_tp,
# #     country_shp = country_shp,
# #     shp_fort = shp_fort,
# #     out_path = out_pt,
# #     map_size = map_size,
# #     in_dts_tag = in_dts_tag))
# 
# 
# # ---------------------------------------- submit all jobs
# 
# 
# if (CLUSTER) {
# 
#   maps <- queuer::qlapply(
#     fact_comb_ls,
#     wrapper_to_ggplot_map,
#     obj,
#     my_colors = col_ls,
#     model_tp = model_tp,
#     country_shp = country_shp,
#     shp_fort = shp_fort,
#     out_path = out_pt,
#     map_size = map_size,
#     in_dts_tag = in_dts_tag)
# 
# } else {
# 
#   maps <- loop(
#     fact_comb_ls[2],
#     wrapper_to_ggplot_map,
#     my_colors = col_ls,
#     model_tp = model_tp,
#     country_shp = country_shp,
#     shp_fort = shp_fort,
#     out_path = out_pt,
#     map_size = map_size,
#     in_dts_tag = in_dts_tag,
#     parallel = FALSE)
# 
# }
# 
# if(!CLUSTER){
#   context::parallel_cluster_stop()
# }


# =============================================================================
#
# new script for quick mapping 
#
# =============================================================================


mean_pred_fl_nm <- paste0(vars_to_plot, ".rds")

df_long <- readRDS(file.path(in_path, mean_pred_fl_nm))

out_fl_nm <- paste0(vars_to_plot, "_", statistics, ".png")

names(df_long)[names(df_long) == "lat.grid"] <- "latitude"
names(df_long)[names(df_long) == "long.grid"] <- "longitude"

quick_raster_map(df_long, statistics, out_pt, out_fl_nm)
