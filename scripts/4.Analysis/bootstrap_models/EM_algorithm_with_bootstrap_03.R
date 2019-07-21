# Estimates foi (or R0) for each resampled square, of each 20km resolution bootstrap sample

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.R"),
  file.path("R", "create_parameter_list.R"),
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "wrapper_functions_for_boot_analysis.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


extra_prms <- list(id = 29,
                   dependent_variable = "FOI",
                   no_predictors = 26) 


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  obj <- didehpc::queue_didehpc(ctx)
  
} else {
  
  context::context_load(ctx)
  
}


# define variables ------------------------------------------------------------


parameters <- create_parameter_list(extra_params = extra_prms)

model_type <- paste0("model_", parameters$id)

in_path <- file.path("output", 
                     "EM_algorithm",
                     "bootstrap_models",
                     model_type, 
                     "env_variables", 
                     "boot_samples")

RF_obj_path <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
                         model_type, 
                         "model_objects", 
                         "boot_samples")

out_pth <- file.path("output", 
                     "EM_algorithm",
                     "bootstrap_models",
                     model_type, 
                     "env_variables_and_init_pred", 
                     "boot_samples")

covariates_dir <- parameters$covariates_dir


# load data ------------------------------------------------------------------- 


predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     covariates_dir,
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]

no_samples <- parameters$no_samples


# submit one job -------------------------------------------------------------- 


# t <- obj$enqueue(
#   get_bsample_and_predict(
#     seq_len(no_samples)[1],
#     RF_obj_path = RF_obj_path,
#     my_preds = my_predictors,
#     out_path = out_pth,
#     in_path = in_path))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  initial_square_preds <- queuer::qlapply(
    seq_len(no_samples),
    get_bsample_and_predict,
    obj,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    out_path = out_pth,
    in_path = in_path)

} else {

  initial_square_preds <- lapply(
    seq_len(no_samples)[1],
    get_bsample_and_predict,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    out_path = out_pth,
    in_path = in_path)

}
