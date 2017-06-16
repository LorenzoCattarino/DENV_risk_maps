# Runs the EM algorithm on each 20km square bootstrap samples

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
  context::parallel_cluster_start(8, ctx)

}


# ---------------------------------------- define parameters


no_fits <- 1

dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

pseudoAbs_value <- 0

all_wgt <- 1

pAbs_wgt <- 0.25

niter <- 35

grp_flds <- c("ID_0", "ID_1", "data_id")

boot_pxl_df_path <- file.path("output", "EM_algorithm", "env_variables_foi", "boot_samples")

full_pxl_df_name <- "aggreg_pixel_level_env_vars_20km.rds"

prd_out_pth <- file.path("output", "predictions", "boot_model_20km_cw", "boot_samples")

out_prd_nm_all <- paste0("square_predictions_boot_model_20km_cw_sample_", seq_len(no_fits), ".rds")


# ---------------------------------------- load data


foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 

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


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]


# ---------------------------------------- pre processing the original dataset


foi_data[foi_data$type == "pseudoAbsence", "FOI"] <- pseudoAbs_value

foi_data$new_weight <- all_wgt

foi_data[foi_data$type == "pseudoAbsence", "new_weight"] <- pAbs_wgt

names(foi_data)[names(foi_data) == "FOI"] <- dependent_variable

foi_data <- foi_data[, c(grp_flds, dependent_variable, "new_weight")]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   exp_max_algorithm_boot(
#     seq_len(no_fits)[1],
#     pxl_dts_path = boot_pxl_df_path,
#     adm_dts_orig = foi_data,
#     pxl_dataset_orig = full_pxl_df,
#     y_var = dependent_variable,
#     my_preds = my_predictors,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     grp_flds = grp_flds,
#     niter = niter,
#     all_wgt = all_wgt,
#     pAbs_wgt = pAbs_wgt,
#     out_pred_name = out_prd_nm_all,
#     pred_out_path = prd_out_pth))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  EM_alg_run <- queuer::qlapply(
    seq_len(no_fits),
    exp_max_algorithm_boot,
    obj,
    pxl_dts_path = boot_pxl_df_path,
    adm_dts_orig = foi_data,
    pxl_dataset_orig = full_pxl_df,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    out_pred_name = out_prd_nm_all,
    pred_out_path = prd_out_pth)

}else{

  EM_alg_run <- lapply(
    seq_len(no_fits),
    exp_max_algorithm_boot,
    pxl_dts_path = boot_pxl_df_path,
    adm_dts_orig = foi_data,
    pxl_dataset_orig = full_pxl_df,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    out_pred_name = out_prd_nm_all,
    pred_out_path = prd_out_pth)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
