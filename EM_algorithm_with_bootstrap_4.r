# Runs the EM algorithm on each 20km square bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"),
  file.path("R", "random_forest", "exp_max_algorithm.r"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "generic_scatter_plot.r"))

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2", "weights")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 1,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 9)   

var_to_fit <- "FOI"

grp_flds <- c("ID_0", "ID_1", "unique_id")

full_pxl_df_name <- "env_vars_20km.rds"

predictor_path <- file.path("output", 
                            "variable_selection",
                            "metropolis_hastings",
                            "exp_1",
                            "variable_rank_final_fits_exp_1.csv")

# number_of_predictors <- 13
# predictor_path <- file.path("output", 
#                             "variable_selection", 
#                             "stepwise", 
#                             "predictor_rank.csv")


# define variables ------------------------------------------------------------  


no_samples <- parameters$no_samples

grid_size <- parameters$grid_size
  
model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", grid_size)

RF_nm_all <- paste0("RF_obj_sample_", seq_len(no_samples), ".rds")

diag_t_nm_all <- paste0("diagno_table_", seq_len(no_samples), ".rds")

map_nm_all <- paste0("map_", seq_len(no_samples))

tra_dts_nm_all <- paste0("train_dts_", seq_len(no_samples), ".rds")
  
RF_out_pth <- file.path("output", 
                        "EM_algorithm",
                        "bootstrap_models",
                        my_dir,
                        model_type,
                        "optimized_model_objects")

diag_t_pth <- file.path("output", 
                        "EM_algorithm",
                        "bootstrap_models",
                        my_dir,
                        model_type,
                        "diagnostics")

train_dts_pth <- file.path("output",
                           "EM_algorithm",
                           "bootstrap_models",
                           my_dir,
                           model_type,
                           "training_datasets")

map_pth <- file.path("figures", 
                     "EM_algorithm",
                     my_dir, 
                     model_type, 
                     "maps", 
                     paste0("sample_", seq_len(no_samples)))

sct_plt_pth <- file.path("figures", 
                         "EM_algorithm",
                         my_dir, 
                         model_type,
                         "iteration_fits",
                         paste0("sample_", seq_len(no_samples)))

sqr_dts_pth <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir, 
                         paste0("env_variables_", var_to_fit, "_fit"),
                         "boot_samples")


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
}


# load data ------------------------------------------------------------------- 


full_pxl_df <- readRDS(file.path("output", 
                                 "EM_algorithm",
                                 "best_fit_models",
                                 "env_variables", 
                                 full_pxl_df_name))

predictor_rank <- read.csv(predictor_path, stringsAsFactors = FALSE)

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
#     pxl_dataset_orig = full_pxl_df,
#     my_preds = my_predictors,
#     grp_flds = grp_flds,
#     RF_obj_path = RF_out_pth,
#     RF_obj_name = RF_nm_all,
#     diagn_tab_path = diag_t_pth,
#     diagn_tab_name = diag_t_nm_all,
#     map_path = map_pth,
#     map_name = map_nm_all,
#     sct_plt_path = sct_plt_pth,
#     adm_dataset = adm_dts,
#     pxl_dts_pt = sqr_dts_pth,
#     var_to_fit = var_to_fit,
#     train_dts_path = train_dts_pth,
#     train_dts_name = tra_dts_nm_all))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  EM_alg_run_exp <- queuer::qlapply(
    seq_len(no_samples),
    exp_max_algorithm_boot,
    obj,
    parms = parameters,
    boot_samples = bt_samples,
    pxl_dataset_orig = full_pxl_df,
    my_preds = my_predictors,
    grp_flds = grp_flds,
    RF_obj_path = RF_out_pth,
    RF_obj_name = RF_nm_all,
    diagn_tab_path = diag_t_pth,
    diagn_tab_name = diag_t_nm_all,
    map_path = map_pth,
    map_name = map_nm_all,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts,
    pxl_dts_pt = sqr_dts_pth,
    var_to_fit = var_to_fit,
    train_dts_path = train_dts_pth,
    train_dts_name = tra_dts_nm_all)

} else {

  EM_alg_run_exp <- lapply(
    seq_len(no_samples)[1],
    exp_max_algorithm_boot,
    parms = parameters,
    boot_samples = bt_samples,
    pxl_dataset_orig = full_pxl_df,
    my_preds = my_predictors,
    grp_flds = grp_flds,
    RF_obj_path = RF_out_pth,
    RF_obj_name = RF_nm_all,
    diagn_tab_path = diag_t_pth,
    diagn_tab_name = diag_t_nm_all,
    map_path = map_pth,
    map_name = map_nm_all,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts,
    pxl_dts_pt = sqr_dts_pth,
    var_to_fit = var_to_fit,
    train_dts_path = train_dts_pth,
    train_dts_name = tra_dts_nm_all)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
