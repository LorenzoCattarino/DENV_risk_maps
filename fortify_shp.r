options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "fortify_and_save_shp.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("rgdal", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


country_shp <- readOGR(
  file.path("data",
            "shapefiles",
            "gadm28_levels.shp"),
  "gadm28_adm0")


# ---------------------------------------- define parameters


world_CRS <- CRS("+init=EPSG:4760")

out_pt <- file.path("output", "datasets")
  
out_nm <- "country_shp_prj_fort.rds"
  
  
# ---------------------------------------- pre processing


country_shp_prj <- spTransform(country_shp, world_CRS)


# ---------------------------------------- submit the job


if (CLUSTER) {

  shp_f <- obj$enqueue(fortify_and_save(country_shp_prj, out_pt, out_nm))

} else {

  shp_f <- fortify(country_shp_prj, region = "ID_0") 
  
}
