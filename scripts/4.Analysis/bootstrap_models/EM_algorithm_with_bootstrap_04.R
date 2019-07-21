# Runs the EM algorithm on each 20km square bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "random_forest", "wrapper_functions_for_boot_analysis.R"),
  file.path("R", "plotting", "functions_for_plotting_raster_maps.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "prepare_datasets", "average_up.R"))

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 29,
                   dependent_variable = "FOI",
                   no_predictors = 26,
                   ranger_threads = 1,
                   id_fld = "unique_id",
                   grp_flds = c("unique_id", "ID_0", "ID_1"))
  
  
# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------  


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

no_samples <- parameters$no_samples

grid_size <- parameters$grid_size

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

covariates_dir <- parameters$covariates_dir

foi_data_pth <- file.path("output", 
                          "EM_algorithm",
                          "bootstrap_models",
                          model_type, 
                          "adm_foi_data",
                          "boot_samples") 


# load data ------------------------------------------------------------------- 


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE)

all_sqr_covariates <- readRDS(file.path("output", 
                                        "env_variables", 
                                        "all_squares_env_var_0_1667_deg.rds"))

data_sqr_covariates <- readRDS(file.path("output", 
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "model_15",
                                         "env_variables", 
                                         "env_vars_20km.rds"))


# pre process ----------------------------------------------------------------- 


number_of_predictors <- parameters$no_predictors
  
my_predictors <- predictor_rank$name[1:number_of_predictors]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   get_bsample_and_EM_fit(
#     seq_len(no_samples)[1],
#     parms = parameters,
#     foi_data_path = foi_data_pth,
#     my_preds = my_predictors,
#     RF_obj_path = RF_out_pth,
#     diagn_tab_path = diag_t_pth,
#     map_path = map_pth,
#     sct_plt_path = sct_plt_pth,
#     adm_dataset = adm_dataset,
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
    get_bsample_and_EM_fit,
    obj,
    parms = parameters,
    foi_data_path = foi_data_pth,
    my_preds = my_predictors,
    RF_obj_path = RF_out_pth,
    diagn_tab_path = diag_t_pth,
    map_path = map_pth,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dataset,
    pxl_dts_pt = sqr_dts_pth,
    train_dts_path = train_dts_pth,
    data_squares = data_sqr_covariates,
    all_squares = all_sqr_covariates,
    data_sqr_predictions_out_path = data_sqr_predictions_out_path,
    all_sqr_predictions_out_path = global_predictions_out_path)

} else {

  EM_alg_run_exp <- lapply(
    seq_len(no_samples)[1],
    get_bsample_and_EM_fit,
    parms = parameters,
    foi_data_path = foi_data_pth,
    my_preds = my_predictors,
    RF_obj_path = RF_out_pth,
    diagn_tab_path = diag_t_pth,
    map_path = map_pth,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dataset,
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
