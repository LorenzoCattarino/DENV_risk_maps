# Fits RF to each bootstrap sample of the original data (using fixed RF parameters)
# and saves the RF model object

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "functions_for_fitting_h2o_RF_and_making_predictions.r"),
  file.path("R", "prepare_datasets", "set_pseudo_abs_weights.R"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


dependent_variable <- "FOI"

pseudoAbsence_value <- -0.02

no_fits <- 200

grid_size <- 10

no_trees <- 500

min_node_size <- 20

all_wgt <- 1

wgt_limits <- c(1, 500)


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", grid_size)

out_pt <- file.path("output", 
                    "EM_algorithm",
                    "bootstrap_models",
                    my_dir, 
                    paste0("model_objects_", dependent_variable, "_fit"), 
                    "boot_samples")


# are you using the cluster? --------------------------------------------------  


if (CLUSTER) {
  
  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)
  
} else {
  
  context::context_load(ctx)
  
}


# load data ------------------------------------------------------------------- 


boot_samples <- readRDS(file.path("output", 
                                  "EM_algorithm",
                                  "bootstrap_models",
                                  my_dir, 
                                  "bootstrap_samples.rds"))  

predictor_rank <- read.csv(file.path("output", 
                                     "variable_selection", 
                                     "metropolis_hastings", 
                                     "exp_1", 
                                     "variable_rank_final_fits_exp_1.csv"),
                           stringsAsFactors = FALSE)


# pre processing -------------------------------------------------------------- 


my_predictors <- predictor_rank$variable[1:9]


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   get_boot_sample_and_fit_RF(
#     seq_len(no_fits)[1],
#     boot_ls = boot_samples,
#     my_preds = my_predictors,
#     y_var = dependent_variable,
#     no_trees = no_trees,
#     min_node_size = min_node_size,
#     out_path = out_pt,
#     psAb_val = pseudoAbsence_value,
#     all_wgt = all_wgt,
#     wgt_limits = wgt_limits,
#     start_h2o = TRUE,
#     shut_h2o = TRUE))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {
  
  RF_obj <- queuer::qlapply(
    seq_len(no_fits),
    get_boot_sample_and_fit_RF,
    obj,
    boot_ls = boot_samples,
    my_preds = my_predictors,
    y_var = dependent_variable,
    no_trees = no_trees,
    min_node_size = min_node_size,
    out_path = out_pt,
    psAb_val = pseudoAbsence_value,
    all_wgt = all_wgt,
    wgt_limits = wgt_limits,
    start_h2o = TRUE,
    shut_h2o = TRUE)
  
} else {
  
  h2o.init()
  
  RF_obj <- lapply(
    seq_len(no_fits),
    get_boot_sample_and_fit_RF,
    boot_ls = boot_samples,
    my_preds = my_predictors,
    y_var = dependent_variable,
    no_trees = no_trees,
    min_node_size = min_node_size,
    out_path = out_pt,
    psAb_val = pseudoAbsence_value,
    all_wgt = all_wgt,
    wgt_limits = wgt_limits,
    start_h2o = FALSE,
    shut_h2o = FALSE)
  
  h2o.shutdown(prompt = FALSE)
  
}
