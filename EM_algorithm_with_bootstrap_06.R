# Estimates foi (or R0) of each 20 km square of the full dataset, 
# for each bootstrap sample

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "ranger"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  grid_size = 5,
  no_samples = 200,
  no_predictors = 23)   

model_type_tag <- "_boot_model_22"


# define variables ------------------------------------------------------------


var_to_fit <- parameters$dependent_variable

model_type <- paste0(var_to_fit, model_type_tag)

my_dir <- paste0("grid_size_", parameters$grid_size)

RF_obj_path <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir, 
                         model_type,
                         "optimized_model_objects")

out_pth <- NULL


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


pxl_data <- readRDS(file.path("output", 
                              "EM_algorithm", 
                              "best_fit_models",
                              paste0("env_variables_", var_to_fit, "_fit"), 
                              "covariates_and_foi_20km_2.rds"))

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection",
                                     "stepwise",
                                     "predictor_rank.csv"), 
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$name[1:parameters$no_predictors]

no_samples <- parameters$no_samples


# submit one job -------------------------------------------------------------- 


# t <- obj$enqueue(
#   wrapper_to_make_ranger_preds(
#     seq_len(no_samples)[1],
#     model_in_path = RF_obj_path,
#     dataset = pxl_data,
#     predictors = my_predictors))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  final_sqr_preds <- queuer::qlapply(
    seq_len(no_samples),
    wrapper_to_make_ranger_preds,
    obj,
    model_in_path = RF_obj_path,
    dataset = pxl_data,
    predictors = my_predictors)

} else {

  final_sqr_preds <- lapply(
    seq_len(no_samples)[1],
    wrapper_to_make_ranger_preds,
    RF_obj_path = RF_obj_path,
    model_in_path = RF_obj_path,
    dataset = pxl_data,
    predictors = my_predictors)

}
