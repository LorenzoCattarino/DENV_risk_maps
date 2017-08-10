# Binds all square tiles together 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "load_and_bind.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- "data.table"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


model_tp <- "boot_model_20km_cw" 

in_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp,
  "tile_sets_0_1667_deg")

out_pt <- file.path(
  "output", 
  "predictions_world",
  model_tp)

out_fl_nm <- "pred_0_1667_deg_long.rds"


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
}else{
  
  context::context_load(ctx)
  
}


# ---------------------------------------- pre processing


fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit job


if (CLUSTER) {
  
  all_tiles <- obj$enqueue(load_and_bind(fi, out_pt, out_fl_nm))

} else {
  
  all_tiles <- load_and_bind(fi, out_pt, out_fl_nm)

}
