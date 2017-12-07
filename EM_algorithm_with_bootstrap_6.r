# Runs the EM algorithm on each 20km square bootstrap samples

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "wrapper_to_Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "random_forest", "Exp_Max_algorithm.r"),
  file.path("R", "random_forest", "get_lm_equation.r"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "generic_scatter_plot.r"))

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# ---------------------------------------- define parameters


model_type <- "boot_model_20km_2"

var_to_fit <- "FOI"

pseudoAbsence_value <- -0.02

no_fits <- 200

niter <- 10

all_wgt <- 1

wgt_limits <- c(1, 25)

grp_flds <- c("ID_0", "ID_1", "unique_id")

full_pxl_df_name <- "env_vars_20km.rds"


# ---------------------------------------- define variables 


RF_nm_all <- paste0("RF_obj_sample_", seq_len(no_fits), ".rds")

diag_t_nm_all <- paste0("diagno_table_", seq_len(no_fits), ".rds")

map_nm_all <- paste0("map_", seq_len(no_fits))

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

sqr_dts_pth <- file.path(
  "output", 
  "EM_algorithm", 
  paste0("env_variables_", var_to_fit, "_fit"),
  "boot_samples")


# ---------------------------------------- are you using the cluster? 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "12and16Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  #context::parallel_cluster_start(8, ctx)
  
}


# ---------------------------------------- load data


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
            "bootstrap_samples.rds"))


# ---------------------------------------- get the vector of best predictors


my_predictors <- predictor_rank$variable[1:9]

#my_predictors <- c(my_predictors, "RFE_const_term", "pop_den")


# ---------------------------------------- pre process the admin data set


adm_dts <- adm_dataset[!duplicated(adm_dataset[, c("ID_0", "ID_1")]), ]


# ---------------------------------------- submit one job 


# t <- obj$enqueue(
#   exp_max_algorithm_boot(
#     seq_len(no_fits)[1],
#     boot_samples = bt_samples,
#     pxl_dataset_orig = full_pxl_df,
#     psAbs = pseudoAbsence_value,
#     my_preds = my_predictors,
#     grp_flds = grp_flds,
#     niter = niter,
#     all_wgt = all_wgt,
#     wgt_limits = wgt_limits,
#     RF_obj_path = RF_out_pth,
#     RF_obj_name = RF_nm_all,
#     diagn_tab_path = diag_t_pth,
#     diagn_tab_name = diag_t_nm_all,
#     map_path = map_pth,
#     map_name = map_nm_all,
#     sct_plt_path = sct_plt_pth,
#     adm_dataset = adm_dts,
#     pxl_dts_pt = sqr_dts_pth,
#     var_to_fit = var_to_fit))


# ---------------------------------------- submit all jobs


if (CLUSTER) {

  EM_alg_run_exp <- queuer::qlapply(
    seq_len(no_fits),
    exp_max_algorithm_boot,
    obj,
    boot_samples = bt_samples,
    pxl_dataset_orig = full_pxl_df,
    psAbs = pseudoAbsence_value,
    my_preds = my_predictors,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    wgt_limits = wgt_limits,
    RF_obj_path = RF_out_pth,
    RF_obj_name = RF_nm_all,
    diagn_tab_path = diag_t_pth,
    diagn_tab_name = diag_t_nm_all,
    map_path = map_pth,
    map_name = map_nm_all,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts,
    pxl_dts_pt = sqr_dts_pth,
    var_to_fit = var_to_fit)

} else {

  EM_alg_run_exp <- lapply(
    seq_len(no_fits)[1],
    exp_max_algorithm_boot,
    boot_samples = bt_samples,
    pxl_dataset_orig = full_pxl_df,
    psAbs = pseudoAbsence_value,
    my_preds = my_predictors,
    grp_flds = grp_flds,
    niter = niter,
    all_wgt = all_wgt,
    wgt_limits = wgt_limits,
    RF_obj_path = RF_out_pth,
    RF_obj_name = RF_nm_all,
    diagn_tab_path = diag_t_pth,
    diagn_tab_name = diag_t_nm_all,
    map_path = map_pth,
    map_name = map_nm_all,
    sct_plt_path = sct_plt_pth,
    adm_dataset = adm_dts,
    pxl_dts_pt = sqr_dts_pth,
    var_to_fit = var_to_fit)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
