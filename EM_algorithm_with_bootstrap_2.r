# Fits RF to each bootstrap sample of the original data (using fixed RF parameters)
# and saves the RF model object

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- "h2o"

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  grid_size = 1 / 120,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  no_predictors = 9)   


# define variables ------------------------------------------------------------


my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", 
                    "EM_algorithm",
                    "bootstrap_models",
                    my_dir, 
                    paste0("model_objects_", parameters$dependent_variable, "_fit"), 
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


my_predictors <- predictor_rank$name[1:parameters$no_predictors]

no_samples <- parameters$no_samples


# submit one job --------------------------------------------------------------  


# t <- obj$enqueue(
#   get_boot_sample_and_fit_RF(
#     seq_len(no_samples)[1],
#     parms = parameters,
#     boot_ls = boot_samples,
#     my_preds = my_predictors,
#     out_path = out_pt,
#     start_h2o = TRUE,
#     shut_h2o = TRUE))


# submit all jobs ------------------------------------------------------------- 


if (CLUSTER) {

  RF_obj <- queuer::qlapply(
    seq_len(no_samples),
    get_boot_sample_and_fit_RF,
    obj,
    parms = parameters,
    boot_ls = boot_samples,
    my_preds = my_predictors,
    out_path = out_pt,
    start_h2o = TRUE,
    shut_h2o = TRUE)

} else {

  h2o.init()

  RF_obj <- lapply(
    seq_len(no_samples)[1],
    get_boot_sample_and_fit_RF,
    parms = parameters,
    boot_ls = boot_samples,
    my_preds = my_predictors,
    out_path = out_pt,
    start_h2o = FALSE,
    shut_h2o = FALSE)

  h2o.shutdown(prompt = FALSE)

}
