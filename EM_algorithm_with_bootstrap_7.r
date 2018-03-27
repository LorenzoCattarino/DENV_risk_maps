# For each bootstrap sample of the original dataset, it creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"))

my_pkgs <- c("h2o", "dplyr", "data.table")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------  


var_to_fit <- "FOI"

grid_size <- 1

pseudoAbsence_value <- -0.02

no_fits <- 200

grp_flds <- c("ADM_0", "ADM_1", "data_id")


# define variables ------------------------------------------------------------ 


model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", grid_size)

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir,
                         model_type,
                         "optimized_model_objects")

out_pt <- file.path("output",
                    "EM_algorithm",
                    "bootstrap_models",
                    my_dir,
                    model_type,
                    "predictions_data")


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# load data -------------------------------------------------------------------  


foi_dataset <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var_area.csv"),
  stringsAsFactors = FALSE) 

boot_samples <- readRDS(file.path("output",
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))
  
sqr_dataset <- readRDS(file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 "env_variables",
                                 "env_vars_20km.rds"))

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "stepwise", 
                                     "predictor_rank.csv"),
                           stringsAsFactors = FALSE)

# tiles
tile_summary <- read.csv(file.path("data", 
                                   "env_variables", 
                                   "plus60minus60_tiles.csv"), 
                         header = TRUE, 
                         stringsAsFactors = FALSE)

# NA pixel tiles 
NA_pixel_tiles <- read.table(file.path("output", 
                                       "datasets", 
                                       "NA_pixel_tiles_20km.txt"), 
                             sep = ",",
                             header = TRUE)

all_sqr_predictions <- readRDS(file.path("output",
                                         "EM_algorithm",
                                         "bootstrap_models",
                                         my_dir,
                                         model_type,
                                         "square_predictions_all_data.rds"))


# process the original data ---------------------------------------------------


names(foi_dataset)[names(foi_dataset) == var_to_fit] <- "o_j"
names(foi_dataset)[names(foi_dataset) == "ID_0"] <- grp_flds[1]
names(foi_dataset)[names(foi_dataset) == "ID_1"] <- grp_flds[2]

foi_dataset[foi_dataset$type == "pseudoAbsence", "o_j"] <- pseudoAbsence_value


# pre process admin predictions ----------------------------------------------- 


adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# create some objects ---------------------------------------------------------  


tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

my_predictors <- predictor_rank$name[1:13]


# submit one job --------------------------------------------------------------


# t <- obj$enqueue(
#   attach_pred_different_scale_to_data(
#     seq_len(no_fits)[1],
#     model_path = RF_obj_path,
#     foi_data = foi_dataset,
#     adm_dts = adm_dataset,
#     predictors = my_predictors,
#     all_sqr_preds = all_sqr_predictions,
#     sqr_dts = sqr_dataset,
#     tile_ids = tile_ids_2,
#     bt_samples = boot_samples,
#     out_path = out_pt,
#     grp_fields = grp_flds,
#     start_h2o = TRUE,
#     shut_h2o = TRUE))


# submit all jobs ------------------------------------------------------------- 




if (CLUSTER) {

  bsamples_preds <- queuer::qlapply(
    seq_len(no_fits),
    attach_pred_different_scale_to_data,
    obj,
    model_path = RF_obj_path,
    foi_data = foi_dataset,
    adm_dts = adm_dataset,
    predictors = my_predictors,
    all_sqr_preds = all_sqr_predictions,
    sqr_dts = sqr_dataset,
    tile_ids = tile_ids_2,
    bt_samples = boot_samples,
    out_path = out_pt,
    grp_fields = grp_flds,
    start_h2o = TRUE,
    shut_h2o = TRUE)

} else {
  
  h2o.init()
  
  bsamples_preds <- lapply(
    seq_len(no_fits),
    attach_pred_different_scale_to_data,
    model_path = RF_obj_path,
    foi_data = foi_dataset,
    adm_dts = adm_dataset,
    predictors = my_predictors,
    all_sqr_preds = all_sqr_predictions,
    sqr_dts = sqr_dataset,
    tile_ids = tile_ids_2,
    bt_samples = boot_samples,
    out_path = out_pt,
    grp_fields = grp_flds,
    start_h2o = FALSE,
    shut_h2o = FALSE)
  
  h2o.shutdown(prompt = FALSE)
  
}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
