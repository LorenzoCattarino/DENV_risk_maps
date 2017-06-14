options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "filter_resample_and_combine.r"),
  file.path("R", "prepare_datasets", "filter_and_resample.r"),
  file.path("R", "prepare_datasets", "grid_up_foi_dataset.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
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


# ---------------------------------------- pre processing


country_shp_prj <- spTransform(country_shp, world_CRS)


# ---------------------------------------- submit the job


if (CLUSTER) {

  t <- obj$enqueue(
    fortify(country_shp_prj, region = "ID_0"))  

} else {

    
}
