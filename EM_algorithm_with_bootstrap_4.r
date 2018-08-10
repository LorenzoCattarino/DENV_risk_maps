# Runs the EM algorithm on each 20km square bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.R"),
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
  resample_grid_size = 20,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  grid_size = 5,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 26)   

grp_flds <- c("ID_0", "ID_1", "unique_id")

model_type_tag <- "_boot_model_21"


# define variables ------------------------------------------------------------  


no_samples <- parameters$no_samples

grid_size <- parameters$grid_size
  
model_type <- paste0(parameters$dependent_variable, model_type_tag)

my_dir <- paste0("grid_size_", grid_size)

RF_nm_all <- paste0("sample_", seq_len(no_samples), ".rds")

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
                     "bootstrap_models",
                     my_dir, 
                     model_type, 
                     "maps")

sct_plt_pth <- file.path("figures", 
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir, 
                         model_type,
                         "iteration_fits")

sqr_dts_pth <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir, 
                         paste0("env_variables_", parameters$dependent_variable, "_fit"),
                         "boot_samples")


# are you using the cluster? --------------------------------------------------


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")#GeneralNodes", wholenode = TRUE)
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
  
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
#     RF_obj_name = RF_nm_all,
#     diagn_tab_path = diag_t_pth,
#     diagn_tab_name = diag_t_nm_all,
#     map_path = map_pth,
#     map_name = map_nm_all,
#     sct_plt_path = sct_plt_pth,
#     adm_dataset = adm_dts,
#     pxl_dts_pt = sqr_dts_pth,
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
    train_dts_path = train_dts_pth,
    train_dts_name = tra_dts_nm_all)

} else {

  EM_alg_run_exp <- lapply(
    seq_len(no_samples)[1],
    exp_max_algorithm_boot,
    parms = parameters,
    boot_samples = bt_samples,
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
    train_dts_path = train_dts_pth,
    train_dts_name = tra_dts_nm_all)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
