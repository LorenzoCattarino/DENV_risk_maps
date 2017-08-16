# Takes the mean foi across models, for each tile 

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.r"))

my_pkgs <- NULL

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw"

no_fits <- 50

NA_tile_fl_name <- "NA_pixel_tiles_20km.txt"

in_pth <- file.path("output", "env_variables", "all_sets_0_1667_deg")

bs_inf <- c("cell", "lat.grid", "long.grid", "population")

in_foi_pth <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg")

var_names <- "foi"

out_names <- c("mean" , "l_CI", "u_CI")


# ---------------------------------------- define variables


out_pth_all <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg",
  "averages")


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::start_parallel_cluster(8, ctx)
  
}


# ---------------------------------------- load data


# tiles
tile_summary <- read.csv(
  file.path("data", 
            "env_variables", 
            "plus60minus60_tiles.csv"), 
  header = TRUE, 
  sep = ",", 
  stringsAsFactors = FALSE)

# NA pixel tiles 
NA_pixel_tiles <- read.table(
  file.path("output", 
            "datasets", 
            NA_tile_fl_name), 
  sep = ",", 
  header = TRUE)


# ---------------------------------------- pre processing


# get tile ids
tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   wrapper_to_load_tile_dataset(
#     seq_along(tile_ids_2)[1],
#     ids_vec = tile_ids_2,
#     in_path = in_pth,
#     predictors = best_predictors,
#     model_in_path = RF_obj_path,
#     parallel = FALSE,
#     no_fits = no_fits,
#     model_type = model_tp,
#     out_path = out_pth_all))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  pred_tiles <- queuer::qlapply(
    seq_along(tile_ids_2),
    wrapper_to_load_tile_dataset,
    obj,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    predictors = best_predictors,
    model_in_path = RF_obj_path,
    parallel = FALSE,
    no_fits = no_fits,
    model_type = model_tp,
    out_path = out_pth_all)

} else {

  pred_tiles <- lapply(
    seq_along(tile_ids_2)[1],
    wrapper_to_load_tile_dataset,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    predictors = best_predictors,
    model_in_path = RF_obj_path,
    parallel = FALSE,
    no_fits = no_fits,
    model_type = model_tp,
    out_path = out_pth_all)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
