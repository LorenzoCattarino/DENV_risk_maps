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
  id = 1,
  shape_1 = 0,
  shape_2 = 5,
  shape_3 = 1e6,
  all_wgt = 1,
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_predictors = 9,
  resample_grid_size = 20,
  foi_offset = 0.03,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10) 


# define variables ------------------------------------------------------------


model_type <- paste0("model_", parameters$id)

var_to_fit <- parameters$dependent_variable

my_dir <- paste0("grid_size_", parameters$grid_size)

RF_obj_path <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
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
                              "env_variables", 
                              "env_vars_20km_2.rds"))

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
