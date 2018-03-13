# Estimates foi (or R0) for each resampled square, of each 20km resolution bootstrap sample

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "utility_functions.r"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


var_to_fit <- "FOI"

no_fits <- 200

grid_size <- 10


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", grid_size)

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
                         paste0("model_objects_", var_to_fit, "_fit"), 
                         "boot_samples")

out_pth <- file.path("output", 
                     "EM_algorithm",
                     "bootstrap_models",
                     my_dir, 
                     paste0("env_variables_", var_to_fit, "_fit"), 
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
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$variable[1:9]


# submit one job -------------------------------------------------------------- 


t <- obj$enqueue(
  load_predict_and_save(
    seq_len(no_fits)[1],
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    no_fits = no_fits,
    out_file_path = out_pth,
    in_path = in_path,
    start_h2o = TRUE,
    shut_h2o = TRUE))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {
  
  initial_square_preds <- queuer::qlapply(
    seq_len(no_fits),
    load_predict_and_save,
    obj,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    no_fits = no_fits,
    out_file_path = out_pth,
    in_path = in_path,
    start_h2o = TRUE,
    shut_h2o = TRUE)
  
} else {
  
  h2o.init()
  
  initial_square_preds <- lapply(
    seq_len(no_fits),
    load_predict_and_save,
    RF_obj_path = RF_obj_path,
    my_preds = my_predictors,
    no_fits = no_fits,
    out_file_path = out_pth,
    in_path = in_path,
    start_h2o = FALSE,
    shut_h2o = FALSE)
  
  h2o.shutdown(prompt = FALSE)
  
}

