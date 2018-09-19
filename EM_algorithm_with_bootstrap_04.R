# Runs the EM algorithm on each 20km square bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 

grp_flds <- c("ID_0", "ID_1", "unique_id")
  
  
# define variables ------------------------------------------------------------  


model_type <- paste0("model_", parameters$id)

no_samples <- parameters$no_samples

grid_size <- parameters$grid_size

my_dir <- paste0("grid_size_", grid_size)
  
RF_out_pth <- file.path("output", 
                        "EM_algorithm",
                        "bootstrap_models",
                        model_type,
                        "optimized_model_objects")

diag_t_pth <- file.path("output", 
                        "EM_algorithm",
                        "bootstrap_models",
                        model_type,
                        "diagnostics")

train_dts_pth <- file.path("output",
                           "EM_algorithm",
                           "bootstrap_models",
                           model_type,
                           "training_datasets")

map_pth <- file.path("figures", 
                     "EM_algorithm",
                     "bootstrap_models",
                     model_type, 
                     "maps")

sct_plt_pth <- file.path("figures", 
                         "EM_algorithm",
                         "bootstrap_models",
                         model_type,
                         "iteration_fits")

sqr_dts_pth <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
                         model_type, 
                         "env_variables_and_init_pred",
                         "boot_samples")

data_sqr_predictions_out_path <- file.path("output", 
                                           "EM_algorithm",
                                           "bootstrap_models", 
                                           model_type, 
                                           "data_square_predictions")

global_predictions_out_path <- file.path("output", 
                                         "predictions_world",
                                         "bootstrap_models",
                                         model_type,
                                         "boot_samples")


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "16Core")#GeneralNodes", wholenode = TRUE)
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE)

bt_samples <- readRDS(file.path("output", 
                                "EM_algorithm", 
                                "bootstrap_models", 
                                my_dir, 
                                "bootstrap_samples.rds"))

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

data_sqr_covariates <- readRDS(file.path("output", 
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "env_variables", 
                                         "env_vars_20km_2.rds"))


# pre process ----------------------------------------------------------------- 


number_of_predictors <- parameters$no_predictors
  
my_predictors <- predictor_rank$name[1:number_of_predictors]

adm_dts <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   exp_max_algorithm_boot(
#     seq_len(no_samples)[1],
#     parms = parameters,
#     boot_samples = bt_samples,
#     my_preds = my_predictors,
#     grp_flds = grp_flds,
#     RF_obj_path = RF_out_pth,
#     diagn_tab_path = diag_t_pth,
#     map_path = map_pth,
#     sct_plt_path = sct_plt_pth,
#     adm_dataset = adm_dts,
#     pxl_dts_pt = sqr_dts_pth,
#     train_dts_path = train_dts_pth,
#     data_squares = data_sqr_covariates,
#     all_squares = all_sqr_covariates,
#     data_sqr_predictions_out_path = data_sqr_predictions_out_path,
#     all_sqr_predictions_out_path = global_predictions_out_path))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  EM_alg_run_exp <- queuer::qlapply(
    seq_len(no_samples),
    exp_max_algorithm_boot,
    obj,
    parms = parameters,
    boot_samples = bt_samples,
    my_preds = my_predictors,
    grp_flds = grp_flds,
    RF_obj_path = RF_out_pth,
    diagn_tab_path = diag_t_pth,
    map_path = map_pth,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts,
    pxl_dts_pt = sqr_dts_pth,
    train_dts_path = train_dts_pth,
    data_squares = data_sqr_covariates,
    all_squares = all_sqr_covariates,
    data_sqr_predictions_out_path = data_sqr_predictions_out_path,
    all_sqr_predictions_out_path = global_predictions_out_path)

} else {

  EM_alg_run_exp <- lapply(
    seq_len(no_samples)[1],
    exp_max_algorithm_boot,
    parms = parameters,
    boot_samples = bt_samples,
    my_preds = my_predictors,
    grp_flds = grp_flds,
    RF_obj_path = RF_out_pth,
    diagn_tab_path = diag_t_pth,
    map_path = map_pth,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts,
    pxl_dts_pt = sqr_dts_pth,
    train_dts_path = train_dts_pth,
    data_squares = data_sqr_covariates,
    all_squares = all_sqr_covariates,
    data_sqr_predictions_out_path = data_sqr_predictions_out_path,
    all_sqr_predictions_out_path = global_predictions_out_path)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
