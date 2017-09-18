# Makes a pretty map of the square predictions  

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_square_level_maps_ggplot.r"))
  
my_pkgs <- c("data.table", "ggplot2", "colorRamps", "raster", "rgdal", "scales", "RColorBrewer")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_cw_2"

n_scenarios <- 6 
  
  
# ---------------------------------------- define variables


x <- file.path(
  "output",
  "predictions_world",
  model_tp,
  "R0_and_burden_all_combs.rds")

out_pt <- file.path(
  "figures", 
  "predictions_world",
  model_tp)

vars <-  c("p9", "FOI", 
           paste0("R0_", seq_len(n_scenarios)), 
           paste0("I_inc_", seq_len(n_scenarios)),
           paste0("C_inc_", seq_len(n_scenarios)))

all_titles <- c("p9", "FOI", 
                rep("R0", n_scenarios),
                rep("Infections", n_scenarios),
                rep("Cases", n_scenarios))

do.p9.logic <- c(TRUE, rep(FALSE, length(vars)-1))


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
}else{
  
  context::context_load(ctx)
  
}


# ---------------------------------------- create color palette


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10))


# ---------------------------------------- load data 


all_preds <- readRDS(x)
      
country_shp <- readOGR(dsn = file.path("output", "shapefiles"), layer = "gadm28_adm0_eras")


# ---------------------------------------- remove the Caspian Sea


country_shp <- country_shp[!country_shp@data$NAME_ENGLI == "Caspian Sea", ]


# ---------------------------------------- convert to ggplot-friendly object 


shp_fort <- fortify(country_shp)


# ------------------------------------------ submit one job 


# t <- obj$enqueue(
#   wrapper_to_ggplot_map(
#     seq_along(vars)[2],
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

  foi_map <- queuer::qlapply(
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

  foi_map <- lapply(
    seq_along(vars)[1],
    wrapper_to_ggplot_map,
    vars = vars,
    my_colors = col_ls,
    titles_vec = all_titles,
    df_long = all_preds,
    country_shp = country_shp,
    shp_fort = shp_fort,
    out_path = out_pt,
    do.p9.logic = do.p9.logic)

}
