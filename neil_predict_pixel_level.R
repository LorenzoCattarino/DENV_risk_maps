options(didewin.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "wrapper_to_load_tile_dataset.R"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.R"),
  file.path("R", "random_forest", "make_RF_predictions.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"))

my_pkgs <- c("ranger", "data.table")

library(ranger,data_table)
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(ranger))

clusterEvalQ(cl, lapply(c(
  file.path("R", "utility_functions.R"),
  file.path("R", "random_forest", "wrapper_to_load_tile_dataset.R"),
  file.path("R", "random_forest", "wrapper_to_make_predictions.R"),
  file.path("R", "random_forest", "make_RF_predictions.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R")),source))

context::context_log_start()
ctx <- context::context_save(packages = my_pkgs,
                             sources = my_resources,
                             root = "context")


# ---------------------------------------- define parameters


cut_off <- 0

bs_inf <- c("cell", "lat.grid", "long.grid", "population") 
#c("pixel_id", "latitude", "longitude", "population", ADM_0", "ADM_1", "ADM_2")

var_names <- "mean_pred" #, "sd_pred", "low_perc", "up_perc")

RF_obj_path <- file.path(
  "output",
  "model_objects",
  "best_model_20km_cw.RDS")

in_path <- file.path(
  "output",
  "env_variables",
  "all_sets_0_1667_deg")

out_path <- file.path(
  "output", 
  "predictions", 
  "best_model_20km_cw",
  "tile_sets_0_1667_deg")


# ---------------------------------------- are you using the cluster?


if (CLUSTER) {
  
  #config <- didewin::didewin_config(template = "12and16Core")
  obj <- didewin::queue_didewin(ctx)

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

#NA_pixel_tile_ids <- NA_pixel_tiles$tile_id
NA_pixel_tile_ids <- c(217, 254, 288, 321, 326, 402, 442, 458, 485, 486, 492, 493, 494, 519, 520, 523, 530, 539, 284, 289, 290, 291, 406, 432, 433, 529)

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  


# ---------------------------------------- submit jobs


if (CLUSTER) {
  
  all_tiles <- queuer::qlapply(
    seq_along(tile_ids_2), 
    wrapper_to_load_tile_dataset, 
    obj,
    ids_vec = tile_ids_2,
    sel_preds = best_predictors,
    in_path = in_path,
    model_in_path = RF_obj_path,
    out_path = out_path,
    cut_off = cut_off,
    var_names = var_names,
    base_info = bs_inf,
    parallel = FALSE)
  
} else {
  
  all_tiles <- parLapply(cl,
    seq_along(tile_ids_2),
    wrapper_to_load_tile_dataset,
    ids_vec = tile_ids_2,
    sel_preds = best_predictors,
    in_path = in_path,
    model_in_path = RF_obj_path,
    out_path = out_path,
    cut_off = cut_off,
    var_names = var_names,
    base_info = bs_inf,
    parallel = FALSE)
  
}

if (!CLUSTER) {
  context:::stop_parallel_cluster()
}

# t <- obj$enqueue(
#   wrapper_to_load_tile_dataset(
#     seq_along(tile_ids_2)[1],
#     ids_vec = tile_ids_2,
#     sel_preds = best_predictors,
#     in_path = in_path,
#     model_in_path = RF_obj_path,
#     out_path = out_path,
#     cut_off = cut_off,
#     var_names = var_names,
#     base_info = bs_inf,
#     parallel = FALSE))
