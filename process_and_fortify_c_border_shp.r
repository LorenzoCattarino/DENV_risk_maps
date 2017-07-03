options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "process_and_fortify.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("rgdal", "ggplot2", "raster")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# ---------------------------------------- load data


country_shp <- readOGR(
  file.path("data",
            "shapefiles",
            "gadm28_levels.shp"),
  "gadm28_adm0")

lake_shp <- readOGR(
  file.path("output",
            "shapefiles"),
  "lakes_diss")


# ---------------------------------------- define parameters


out_pt <- file.path("output", "datasets")
  
out_nm <- "country_shp_fort.rds"
  

# ---------------------------------------- submit the job


if (CLUSTER) {

  shp_f <- obj$enqueue(process_and_fortify(country_shp, lake_shp, out_pt, out_nm))

} else {

  shp_f <- process_and_fortify(country_shp, lake_shp, out_pt, out_nm) 
  
}
