# Runs the EM algorithm on different bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- FALSE

my_resources <- c(
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "grid_up_foi_dataset.r"),
  file.path("R", "random_forest", "get_1_0_point_position.r"),
  file.path("R", "random_forest", "spatial_sampK_cv_rng3.r"),
  file.path("R", "random_forest", "fit_random_forest_model.r"),
  file.path("R", "random_forest", "make_RF_predictions.r"),
  file.path("R", "random_forest", "calculate_sum_squared_errors.r"),
  file.path("R", "prepare_datasets", "filter_and_resample.r"),
  file.path("R", "prepare_datasets", "average_up.r"),
  file.path("R", "prepare_datasets", "remove_NA_rows.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- c("ranger", "data.table", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  context::parallel_cluster_start(8, ctx)
}


# ---------------------------------------- define parameters


no_fits <- 200

dependent_variable <- "o_j"

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

pAbs_wgt <- 0.25

niter <- 35

resample_res_km <- 20

resample_res <- (1 / 120) * resample_res_km

grp_flds <- c("ID_0", "ID_1", "data_id")

in_pt <- file.path("data", "gadm_codes")

aggr_dts_name <- "aggreg_pixel_level_env_vars_20km.rds"

out_md_nm_all <- paste0("boot_model_20km_cw_run_", seq_len(no_fits), ".rds")

out_prd_nm_all <- paste0("square_predictions_boot_model_20km_cw_run_", seq_len(no_fits), ".rds")

md_out_pth <- file.path("output", "model_objects")

prd_out_pth <- file.path("output", "predictions", "boot_model_20km_cw", "all_runs")


# ---------------------------------------- load data


boot_samples <- readRDS(
  file.path("output",
            "foi",
            "bootstrap_samples.rds"))
  
all_predictors <- read.table(
  file.path("output", 
            "datasets", 
            "all_predictors.txt"), 
  header = TRUE, 
  stringsAsFactors = FALSE)
 
predictor_rank <- read.csv(
  file.path("output", 
            "variable_selection", 
            "metropolis_hastings", 
            "exp_1", 
            "variable_rank_final_fits_exp_1.csv"),
  stringsAsFactors = FALSE)

aggreg_pxl_env_var <- readRDS(
  file.path("output", 
            "env_variables", 
            aggr_dts_name))

foi_data <- read.csv(
  file.path("output", "foi", "All_FOI_estimates_linear_env_var.csv"),
  stringsAsFactors = FALSE) 


# ---------------------------------------- pre processing


var_names <- all_predictors$variable

my_predictors <- predictor_rank$variable[1:9]

fi <- list.files(in_pt, 
                 pattern = "^tile",
                 full.names = TRUE)


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   exp_max_algorithm_boot(
#     seq_len(no_fits)[1],
#     boot_ls = boot_samples,
#     original_data = foi_data,
#     y_var = dependent_variable,
#     my_preds = my_predictors,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     fi = fi,
#     var_names = var_names,
#     grp_flds = grp_flds,
#     new_res = resample_res,
#     niter = niter,
#     all_wgt = all_wgt,
#     pAbs_wgt = pAbs_wgt,
#     out_model_name = out_md_nm_all,
#     out_pred_name = out_prd_nm_all,
#     pxl_dataset_full = aggreg_pxl_env_var,
#     model_out_path = md_out_pth,
#     pred_out_path = prd_out_pth))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  EM_alg_run <- queuer::qlapply(
    seq_len(no_fits)[1:10],
    exp_max_algorithm_boot,
    obj,
    boot_ls = boot_samples,
    original_data = foi_data,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    fi = fi,
    var_names = var_names,
    grp_flds = grp_flds,
    new_res = resample_res,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    out_model_name = out_md_nm_all,
    out_pred_name = out_prd_nm_all,
    pxl_dataset_full = aggreg_pxl_env_var,
    model_out_path = md_out_pth,
    pred_out_path = prd_out_pth)

}else{

  EM_alg_run <- lapply(
    seq_len(no_fits)[10],
    exp_max_algorithm_boot,
    boot_ls = boot_samples,
    original_data = foi_data,
    y_var = dependent_variable,
    my_preds = my_predictors,
    no_trees = no_trees,
    min_node_size = min_node_size,
    fi = fi,
    var_names = var_names,
    grp_flds = grp_flds,
    new_res = resample_res,
    niter = niter,
    all_wgt = all_wgt,
    pAbs_wgt = pAbs_wgt,
    out_model_name = out_md_nm_all,
    out_pred_name = out_prd_nm_all,
    pxl_dataset_full = aggreg_pxl_env_var,
    model_out_path = md_out_pth,
    pred_out_path = prd_out_pth)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
