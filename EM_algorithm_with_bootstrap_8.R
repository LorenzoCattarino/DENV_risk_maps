# For each bootstrap sample of the original dataset, it creates a data frame with:  
#
# 1) admin unit observation
# 2) admin unit prediction 
# 3) population weighted average of the 1 km pixel predictions, within the observation's admin unit
# 4) population weighted average of the square predictions, within the observation's admin unit

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.R"),
  file.path("R", "random_forest", "load_predict_filter.r"),
  file.path("R", "random_forest", "attach_predictions_diff_scales_to_data.r"))

my_pkgs <- c("h2o", "dplyr", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters 


no_fits <- 200

model_type <- "boot_model_20km_cw"

RF_obj_path <- file.path(
  "output",
  "EM_algorithm",
  "optimized_model_objects",
  "boot_samples")

sqr_dts_path <- file.path(
  "output", 
  "EM_algorithm", 
  "env_variables", 
  "boot_samples")  

bt_sqr_preds_path <- file.path(
  "output",
  "EM_algorithm",
  "square_predictions",
  "boot_samples")
  
tile_sets_path <- file.path(
  "data", 
  "env_variables", 
  "all_sets_gadm_codes")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
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

adm_dataset <- read.csv(  
  file.path("output",
            "env_variables",
            "All_adm1_env_var.csv"),
  header = TRUE,
  sep = ",", 
  stringsAsFactors = FALSE)

boot_samples <- readRDS(
  file.path("output",
            "EM_algorithm",
            "boot_samples",
            "bootstrap_samples.rds"))


# ---------------------------------------- pre process admin predictions


adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# ---------------------------------------- create some objects 


tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

best_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   attach_pred_different_scale_to_data(
#     seq_len(no_fits)[1],
#     model_path = RF_obj_path,
#     sqr_dts_path = sqr_dts_path,
#     bt_sqr_preds_path = bt_sqr_preds_path,
#     bt_samples = boot_samples,
#     adm_dts = adm_dataset,
#     predictors = best_predictors,
#     all_sqr_preds = all_square_preds,
#     tile_ids = tile_ids_2,
#     in_path = tile_sets_path))


# ---------------------------------------- submit all jobs


if (CLUSTER) {
  
  bsamples_preds <- queuer::qlapply(
    seq_len(no_fits),
    attach_pred_different_scale_to_data,
    obj,
    model_path = RF_obj_path, 
    sqr_dts_path = sqr_dts_path, 
    bt_sqr_preds_path = bt_sqr_preds_path,
    bt_samples = boot_samples,
    adm_dts = adm_dataset, 
    predictors = best_predictors, 
    all_sqr_preds = all_square_preds, 
    tile_ids = tile_ids_2, 
    in_path = tile_sets_path)
  
} else {
  
  bsamples_preds <- lapply(
    seq_len(no_fits)[1],
    attach_pred_different_scale_to_data,
    model_path = RF_obj_path, 
    sqr_dts_path = sqr_dts_path, 
    bt_sqr_preds_path = bt_sqr_preds_path,
    bt_samples = boot_samples,
    adm_dts = adm_dataset, 
    predictors = best_predictors, 
    all_sqr_preds = all_square_preds, 
    tile_ids = tile_ids_2, 
    in_path = tile_sets_path)
  
}
