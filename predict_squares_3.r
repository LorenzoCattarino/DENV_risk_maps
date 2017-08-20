# Makes foi predictions of all squares, for each tile and model fit

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_load_tile_dataset.r"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"))

my_pkgs <- c("h2o", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             packages = my_pkgs,
                             sources = my_resources)


# ---------------------------------------- define parameters


model_tp <- "boot_model_20km_cw"

no_fits <- 50

NA_tile_fl_name <- "NA_pixel_tiles_20km.txt"

in_pth <- file.path("output", "env_variables", "all_sets_0_1667_deg", "gadm")

var_name <- "foi"


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::start_parallel_cluster(8, ctx)
  
}


# ---------------------------------------- load data


# predicting variable rank
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

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


# get the vector of best predictors (from MH variable selection routine)
best_predictors <- predictor_rank$variable[1:9]

# get tile ids
tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

  
# ---------------------------------------- define variables


RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  model_tp,
  "optimized_model_objects")

out_pth_all <- file.path(
  "output", 
  "predictions_world", 
  model_tp,
  "tile_sets_0_1667_deg",
  paste0("tile_", tile_ids_2))


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   wrapper_to_load_tile_dataset(
#     seq_along(tile_ids_2)[185],
#     ids_vec = tile_ids_2,
#     in_path = in_pth,
#     no_fits = no_fits,
#     model_in_path = RF_obj_path,
#     predictors = best_predictors,
#     parallel = FALSE,
#     out_path = out_pth_all,
#     out_name = var_name))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  pred_tiles <- queuer::qlapply(
    seq_along(tile_ids_2),
    wrapper_to_load_tile_dataset,
    obj,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    no_fits = no_fits,
    model_in_path = RF_obj_path,
    predictors = best_predictors,
    parallel = FALSE,
    out_path = out_pth_all,
    out_name = var_name)

} else {

  pred_tiles <- lapply(
    seq_along(tile_ids_2)[1],
    wrapper_to_load_tile_dataset,
    ids_vec = tile_ids_2,
    in_path = in_pth,
    no_fits = no_fits,
    model_in_path = RF_obj_path,
    predictors = best_predictors,
    parallel = FALSE,
    out_path = out_pth_all,
    out_name = var_name)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
