# Estimates foi (or R0) for each resampled square, of each 20km resolution bootstrap sample

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


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)

in_path <- file.path("output", 
                     "EM_algorithm",
                     "bootstrap_models",
                     my_dir, 
                     "env_variables", 
                     "boot_samples")

RF_obj_path <- file.path("output", 
                         "EM_algorithm",
                         "bootstrap_models",
                         my_dir, 
                         paste0("model_objects_", parameters$dependent_variable, "_fit"), 
                         "boot_samples")

out_pth <- file.path("output", 
                     "EM_algorithm",
                     "bootstrap_models",
                     my_dir, 
                     paste0("env_variables_", parameters$dependent_variable, "_fit"), 
                     "boot_samples")


# are you using the cluster? -------------------------------------------------- 


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


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
#   load_predict_and_save(
#     seq_len(no_samples)[1],
#     RF_obj_path = RF_obj_path,
#     my_preds = my_predictors,
#     out_path = out_pth,
#     in_path = in_path))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  initial_square_preds <- queuer::qlapply(
    seq_len(no_samples),
    load_predict_and_save,
    obj,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    out_path = out_pth,
    in_path = in_path)

} else {

  initial_square_preds <- lapply(
    seq_len(no_samples),
    load_predict_and_save,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    out_path = out_pth,
    in_path = in_path)

}
