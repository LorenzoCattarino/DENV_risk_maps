# Runs the EM algorithm on each 20km square bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "fit_h2o_random_forest_model.r"),
  file.path("R", "random_forest", "make_h2o_RF_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "quick_raster_map.r"),
  file.path("R", "random_forest", "get_lm_equation.r"),
  file.path("R", "generic_scatter_plot.r"))

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


model_type <- "boot_model_20km_cw"

no_fits <- 50

niter <- 10

grp_flds <- c("ID_0", "ID_1", "unique_id")

dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 0.25

boot_pxl_df_path <- file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples")

full_pxl_df_name <- "aggreg_pixel_level_env_vars_20km.rds"

RF_nm_all <- paste0("RF_obj_sample_", seq_len(no_fits), ".rds")

diag_t_nm_all <- paste0("diagno_table_", seq_len(no_fits), ".rds")

map_nm_all <- paste0("map_", seq_len(no_fits))

sq_pred_nm_all <- paste0("dd_debug_", seq_len(no_fits), ".rds")


# ========================================
# 
# output paths - IMPORTANT!
# 
# ========================================


RF_out_pth <- file.path(
  "output", 
  "EM_algorithm", 
  model_type,
  "optimized_model_objects")

diag_t_pth <- file.path(
  "output", 
  "EM_algorithm", 
  model_type,
  "diagnostics")

sq_pred_pth <- file.path(
  "output", 
  "EM_algorithm",
  model_type, 
  "square_predictions")

map_pth <- file.path(
  "figures", 
  "EM_algorithm", 
  model_type, 
  "maps", 
  paste0("sample_", seq_len(no_fits)))
  
sct_plt_pth <- file.path(
  "figures", 
  "EM_algorithm", 
  model_type,
  "iteration_fits",
  paste0("sample_", seq_len(no_fits)))


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::parallel_cluster_start(8, ctx)
  
}


# ---------------------------------------- load data


# foi_data <- read.csv(
#   file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
#   stringsAsFactors = FALSE) 

full_pxl_df <- readRDS(
  file.path("output", 
            "EM_algorithm",
            "env_variables", 
            full_pxl_df_name))

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

adm_dataset <- read.csv(  
  file.path("output",
            "env_variables",
            "All_adm1_env_var.csv"),
  header = TRUE,
  sep = ",", 
  stringsAsFactors = FALSE)

bt_samples <- readRDS(
  file.path("output",
            "EM_algorithm",
            "boot_samples",
            "bootstrap_samples.rds"))


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre process the admin data set


adm_dts <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   exp_max_algorithm_boot(
#     seq_len(no_fits)[1],
#     pxl_dts_path = boot_pxl_df_path,
#     boot_samples = bt_samples,
#     pxl_dataset_orig = full_pxl_df,
#     y_var = dependent_variable,
#     my_preds = my_predictors,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     grp_flds = grp_flds,
#     niter = niter,
#     all_wgt = all_wgt,
#     pAbs_wgt = pAbs_wgt,
#     RF_obj_path = RF_out_pth,
#     RF_obj_name = RF_nm_all,
#     diagn_tab_path = diag_t_pth,
#     diagn_tab_name = diag_t_nm_all,
#     map_path = map_pth,
#     map_name = map_nm_all,
#     sq_pr_path = sq_pred_pth,
#     sq_pr_name = sq_pred_nm_all,
#     sct_plt_path = sct_plt_pth,
#     adm_dataset = adm_dts))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  EM_alg_run_exp <- queuer::qlapply(
    seq_len(no_fits),
    exp_max_algorithm_boot,
    obj,
    pxl_dts_path = boot_pxl_df_path,
    boot_samples = bt_samples,
    pxl_dataset_orig = full_pxl_df,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    RF_obj_path = RF_out_pth,
    RF_obj_name = RF_nm_all,
    diagn_tab_path = diag_t_pth,
    diagn_tab_name = diag_t_nm_all,
    map_path = map_pth,
    map_name = map_nm_all,
    sq_pr_path = sq_pred_pth,
    sq_pr_name = sq_pred_nm_all,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts)

} else {

  EM_alg_run <- lapply(
    seq_len(no_fits)[1],
    exp_max_algorithm_boot,
    pxl_dts_path = boot_pxl_df_path,
    boot_samples = bt_samples,
    pxl_dataset_orig = full_pxl_df,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    RF_obj_path = RF_out_pth,
    RF_obj_name = RF_nm_all,
    diagn_tab_path = diag_t_pth,
    diagn_tab_name = diag_t_nm_all,
    map_path = map_pth,
    map_name = map_nm_all,
    sq_pr_path = sq_pred_pth,
    sq_pr_name = sq_pred_nm_all,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
