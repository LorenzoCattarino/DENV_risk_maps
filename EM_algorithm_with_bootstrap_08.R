# For each bootstrap sample of the original dataset, it creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "prepare_datasets", "remove_NA_rows.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------  


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  grid_size = 5,
  no_samples = 200,
  no_predictors = 23)   

model_type_tag <- "_boot_model_22"

grp_flds <- c("ID_0", "ID_1", "data_id")


# define variables ------------------------------------------------------------ 


var_to_fit <- parameters$dependent_variable

model_type <- paste0(var_to_fit, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

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

# obj$enqueue(install.packages(file.path("R_sources", "h2o_3.18.0.8.tar.gz"), repos=NULL, type="source"))$wait(Inf)


# load data -------------------------------------------------------------------  


foi_dataset <- read.csv(file.path("output", 
                                  "foi", 
                                  "All_FOI_estimates_and_predictors_2.csv"),
                        stringsAsFactors = FALSE) 

boot_samples <- readRDS(file.path("output",
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))
  
sqr_dataset <- readRDS(file.path("output",
                                 "EM_algorithm",
                                 "best_fit_models",
                                 "env_variables_FOI_fit",
                                 "covariates_and_foi_20km_2.rds"))

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

foi_dataset[foi_dataset$type == "pseudoAbsence", "o_j"] <- parameters$pseudoAbs_value


# pre process admin predictions ----------------------------------------------- 


adm_dataset <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# create some objects ---------------------------------------------------------  


tile_ids <- tile_summary$tile.id

NA_pixel_tile_ids <- NA_pixel_tiles$tile_id

tile_ids_2 <- tile_ids[!tile_ids %in% NA_pixel_tile_ids]  

my_predictors <- predictor_rank$name[1:parameters$no_predictors]

no_samples <- parameters$no_samples


# submit one job --------------------------------------------------------------


# t <- obj$enqueue(
#   attach_pred_different_scale_to_data(
#     seq_len(no_samples)[1],
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
#     parms = parameters))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  bsamples_preds <- queuer::qlapply(
    seq_len(no_samples),
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
    parms = parameters)

} else {

  bsamples_preds <- lapply(
    seq_len(no_samples)[1],
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
    parms = parameters)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
