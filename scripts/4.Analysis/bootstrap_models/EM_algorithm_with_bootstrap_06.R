# For each bootstrap sample of the original dataset, it creates a data frame with:  
#
# 1) admin unit observations
# 2) admin unit predictions 
# 3) population weighted average of the square predictions, within the observation's admin unit
# 4) population weighted average of the 1 km pixel predictions, within the observation's admin unit

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "join_predictions.R"),
  file.path("R", "random_forest", "wrapper_functions_for_boot_analysis.R"),
  file.path("R", "prepare_datasets", "average_up.R"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))

my_pkgs <- c("ranger", "dplyr")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters -----------------------------------------------------------  


extra_prms <- list(id = 29,
                   dependent_variable = "FOI",
                   no_predictors = 26,
                   id_fld = "data_id",
                   grp_flds = c("data_id", "ID_0", "ID_1"))


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------ 


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

no_samples <- parameters$no_samples

RF_obj_path <- file.path("output",
                         "EM_algorithm",
                         "bootstrap_models",
                         model_type,
                         "optimized_model_objects")

out_pt <- file.path("output",
                    "EM_algorithm",
                    "bootstrap_models",
                    model_type,
                    "data_admin_predictions")

data_sqr_predictions_pth <- file.path("output",
                                          "EM_algorithm",
                                          "bootstrap_models",
                                          model_type,
                                          "data_square_predictions")

covariates_dir <- parameters$covariates_dir

foi_data_pth <- file.path("output", 
                          "EM_algorithm",
                          "bootstrap_models",
                          model_type, 
                          "adm_foi_data",
                          "boot_samples") 


# load data -------------------------------------------------------------------  


original_foi <- readRDS(file.path("output", 
                                  "EM_algorithm", 
                                  "bootstrap_models", 
                                  model_type,
                                  "adm_foi_data",
                                  "adm_foi_data.rds")) 
  
data_sqr_covariates <- readRDS(file.path("output", 
                                         "EM_algorithm",
                                         "best_fit_models",
                                         "model_15",
                                         "env_variables", 
                                         "env_vars_20km.rds"))

adm_dataset <- read.csv(file.path("output",
                                  "env_variables",
                                  "All_adm1_env_var.csv"),
                        header = TRUE,
                        stringsAsFactors = FALSE)

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing --------------------------------------------------------------


my_predictors <- predictor_rank$name[1:parameters$no_predictors]


# submit one job --------------------------------------------------------------


# t <- obj$enqueue(
#   get_bsample_and_join_predictions(
#     seq_len(no_samples)[1],
#     model_path = RF_obj_path,
#     original_foi = original_foi,
#     boot_foi_data_path = foi_data_pth,
#     adm_dts = adm_dataset,
#     predictors = my_predictors,
#     data_sqr_predictions_path = data_sqr_predictions_pth,
#     sqr_dts = data_sqr_covariates,
#     out_path = out_pt,
#     parms = parameters))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  bsamples_preds <- queuer::qlapply(
    seq_len(no_samples),
    get_bsample_and_join_predictions,
    obj,
    model_path = RF_obj_path,
    original_foi = original_foi,
    boot_foi_data_path = foi_data_pth,
    adm_dts = adm_dataset,
    predictors = my_predictors,
    data_sqr_predictions_path = data_sqr_predictions_pth,
    sqr_dts = data_sqr_covariates,
    out_path = out_pt,
    parms = parameters)

} else {

  bsamples_preds <- lapply(
    seq_len(no_samples)[1],
    get_bsample_and_join_predictions,
    model_path = RF_obj_path,
    original_foi = original_foi,
    boot_foi_data_path = foi_data_pth,
    adm_dts = adm_dataset,
    predictors = my_predictors,
    data_sqr_predictions_path = data_sqr_predictions_pth,
    sqr_dts = data_sqr_covariates,
    out_path = out_pt,
    parms = parameters)

}

if (!CLUSTER) {
  context::parallel_cluster_stop()
}
