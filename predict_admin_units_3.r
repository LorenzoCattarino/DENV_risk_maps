# Makes a pretty map of the administrative unit predictions

options(didehpc.cluster = "fi--didemrchnb")

my_resources <- c(
  file.path("R", "plotting", "functions_for_plotting_admin_level_maps.r"))

my_pkgs <- c("rgdal", "dplyr", "colorRamps", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)

CLUSTER <- TRUE


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_2"

vars <- "FOI"

statistics <- c("mean", "sd", "interv", "lCI", "uCI")

all_titles <- c("mean", "SD", "quantile_diff", "2.5_quantile", "97.5_quantile")

do.p9.logic <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

plot_wdt <- 28 #or: 7
plot_hgt <- 12 #or: 3

out_pth <- file.path(
  "figures", 
  "predictions_world",
  model_tp)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- create color palette


col_ls <- list(
  c("red3", "orange", "chartreuse4"),
  matlab.like(10),
  colorRampPalette(c("green4", "yellow", "red"))(10),
  rev(heat.colors(10)))


# ---------------------------------------- load data


prediction_dat <- readRDS(
  file.path(
    "output", 
    "predictions_world", 
    model_tp, 
    "means", 
    "FOI_mean_all_adm_1.rds"))

covariate_dat <- read.csv(
  file.path(
    "output", 
    "env_variables", 
    "All_adm1_env_var.csv"))

country_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                       layer = "gadm28_adm0_eras")

adm_shp <- readOGR(dsn = file.path("output", "shapefiles"), 
                   layer = "gadm28_adm1_eras")


# ---------------------------------------- pre processing


prediction_dat$interv <- prediction_dat$uCI - prediction_dat$lCI 

prediction_dat <- cbind(OBJECTID = covariate_dat$OBJECTID, prediction_dat) 

adm_shp_fort <- fortify(adm_shp, region = "OBJECTID") %>% 
  mutate(id = as.numeric(id))

df_to_plot <- left_join(adm_shp_fort, prediction_dat, by = c("id" = "OBJECTID"))

na_logic <- apply(as.matrix(df_to_plot[, statistics]), 1, anyNA)

df_to_plot[na_logic, statistics] <- rep(0, length(statistics))


# ---------------------------------------- start


if(CLUSTER){
  
  admin_maps <- queuer::qlapply(
    seq_along(statistics),
    wrapper_to_admin_map,
    obj,
    vars = vars, 
    statistics = statistics, 
    my_colors = col_ls, 
    titles_vec = all_titles, 
    df_long = df_to_plot, 
    country_shp = country_shp, 
    out_path = out_pth, 
    do.p9.logic = do.p9.logic,
    plot_wdt = plot_wdt, 
    plot_hgt = plot_hgt)
  
} else {
  
  
  
}
