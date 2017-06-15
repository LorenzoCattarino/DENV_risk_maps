# Runs the EM algorithm on different bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ranger", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "24Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)

}


# ---------------------------------------- define parameters


no_fits <- 200

dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 0.25

niter <- 35

grp_flds <- c("ID_0", "ID_1", "data_id")

full_pxl_df_name <- "aggreg_pixel_level_env_vars_20km.rds"

boot_pxl_df_name <- "aggreg_pixel_level_env_vars_20km_sample_"

boot_pxl_df_path <- file.path("output", "env_variables", "boot_samples")

out_md_nm_all <- paste0("boot_model_20km_cw_run_", seq_len(no_fits), ".rds")

out_prd_nm_all <- paste0("square_predictions_boot_model_20km_cw_run_", seq_len(no_fits), ".rds")

md_out_pth <- file.path("output", "model_objects")

prd_out_pth <- file.path("output", "predictions", "boot_model_20km_cw", "boot_samples")


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 

boot_samples <- readRDS(
  file.path("output",
            "foi",
            "bootstrap_samples.rds"))
  
full_pxl_df <- readRDS(
  file.path("output", 
            "env_variables", 
            full_pxl_df_name))

predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)


# ---------------------------------------- pre processing


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- submit one job 


t <- obj$enqueue(
  exp_max_algorithm_boot(
    seq_len(no_fits)[1],
    pxl_dts_nm_root = boot_pxl_df_name,
    pxl_dts_path = boot_pxl_df_path,
    boot_ls = boot_samples,
    original_data = foi_data,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    out_model_name = out_md_nm_all,
    out_pred_name = out_prd_nm_all,
    pxl_dataset_full = full_pxl_df,
    model_out_path = md_out_pth,
    pred_out_path = prd_out_pth))


# ---------------------------------------- submit all jobs


# if (CLUSTER) {
# 
#   EM_alg_run <- queuer::qlapply(
#     seq_len(no_fits)[1:10],
#     exp_max_algorithm_boot,
#     obj,
#     pxl_dts_nm_root = boot_pxl_df_name,
#     pxl_dts_path = boot_pxl_df_path,
#     boot_ls = boot_samples,
#     original_data = foi_data,
#     y_var = dependent_variable,
#     my_preds = my_predictors,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     grp_flds = grp_flds,
#     niter = niter,
#     all_wgt = all_wgt,
#     pAbs_wgt = pAbs_wgt,
#     out_model_name = out_md_nm_all,
#     out_pred_name = out_prd_nm_all,
#     pxl_dataset_full = full_pxl_df,
#     model_out_path = md_out_pth,
#     pred_out_path = prd_out_pth)
# 
# }else{
# 
#   EM_alg_run <- lapply(
#     seq_len(no_fits)[1],
#     exp_max_algorithm_boot,
#     pxl_dts_nm_root = boot_pxl_df_name,
#     pxl_dts_path = boot_pxl_df_path,
#     boot_ls = boot_samples,
#     original_data = foi_data,
#     y_var = dependent_variable,
#     my_preds = my_predictors,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     grp_flds = grp_flds,
#     niter = niter,
#     all_wgt = all_wgt,
#     pAbs_wgt = pAbs_wgt,
#     out_model_name = out_md_nm_all,
#     out_pred_name = out_prd_nm_all,
#     pxl_dataset_full = full_pxl_df,
#     model_out_path = md_out_pth,
#     pred_out_path = prd_out_pth)
# 
# }
# 
# if (!CLUSTER) {
#   context::parallel_cluster_stop()
# }
