options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_load_tile_dataset.R"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.R"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"))

my_pkgs <- c("h2o", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


cut_off <- 0

model_tp <- "boot_model_20km_cw"

gr_size <- 20 # km 

if (gr_size == 1) {
  bs_inf <- c("pixel_id", "latitude", "longitude", "population", "ADM_0", "ADM_1", "ADM_2")
  in_path <- file.path("data", "env_variables", "all_sets_gadm_codes")
  out_pth <- file.path("output", 
                       "predictions", 
                       model_tp,
                       "tile_sets_0_0083_deg")
} 

if (gr_size == 20) {
  bs_inf <- c("cell", "lat.grid", "long.grid", "population")
  in_path <- file.path("output", "env_variables", "all_sets_0_1667_deg")
  out_pth <- file.path("output", 
                       "predictions", 
                       model_tp,
                       "tile_sets_0_1667_deg")
}

# var_names <- "mean_pred"
var_names <- c("mean_pred" , "low_perc", "up_perc")

RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  "optimized_model_objects",
  "boot_samples")

no_fits <- 200


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
            "NA_pixel_tiles.txt"), 
  sep = ",", 
  header = TRUE)


# ---------------------------------------- pre processing


# get the vector of best predictors (from MH variable selection routine)
best_predictors <- predictor_rank$variable[1:9]

# get tile ids
tile_ids <- tile_summary$tile.id

if (gr_size == 1) {
  NA_pixel_tile_ids <- NA_pixel_tiles$tile_id
}

if (gr_size == 20){
  NA_pixel_tile_ids <- c(217, 254, 288, 321, 326, 402, 
                         442, 458, 485, 486, 492, 493, 
                         494, 519, 520, 523, 530, 539, 
                         284, 289, 290, 291, 406, 432, 
                         433, 529)
}

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   wrapper_to_load_tile_dataset(
#     seq_along(tile_ids_2)[1],
#     ids_vec = tile_ids_2,
#     sel_preds = best_predictors,
#     in_path = in_path,
#     model_in_path = RF_obj_path,
#     out_path = out_pth,
#     cut_off = cut_off,
#     var_names = var_names,
#     base_info = bs_inf,
#     parallel = FALSE,
#     no_fits = no_fits,
#     average = TRUE,
#     model_type = model_tp))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  all_tiles <- queuer::qlapply(
    seq_along(tile_ids_2), 
    wrapper_to_load_tile_dataset, 
    obj,
    ids_vec = tile_ids_2,
    sel_preds = best_predictors,
    in_path = in_path,
    model_in_path = RF_obj_path,
    out_path = out_pth,
    cut_off = cut_off,
    var_names = var_names,
    base_info = bs_inf,
    parallel = FALSE,
    no_fits = no_fits,
    average = TRUE,
    model_type = model_tp)
  
} else {
  
  all_tiles <- lapply(
    seq_along(tile_ids_2)[61],
    wrapper_to_load_tile_dataset,
    ids_vec = tile_ids_2,
    sel_preds = best_predictors,
    in_path = in_path,
    model_in_path = RF_obj_path,
    out_path = out_pth,
    cut_off = cut_off,
    var_names = var_names,
    base_info = bs_inf,
    parallel = FALSE,
    no_fits = no_fits,
    average = TRUE,
    model_type = model_tp)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}
