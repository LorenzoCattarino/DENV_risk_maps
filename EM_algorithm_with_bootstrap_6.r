# Load back in the results of the EM algorithm.
# Specifically, for each bootstrap sample, get:
#
# 1) Vector of square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "utility_functions.r"),
  file.path("R", "random_forest", "fit_h2o_RF_and_make_predictions.r"),
  file.path("R", "random_forest", "exp_max_algorithm.r"),
  file.path("R", "plotting", "quick_raster_map.r"),
  file.path("R", "plotting", "generic_scatter_plot.r"))

my_pkgs <- c("h2o", "dplyr", "fields", "ggplot2")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  grid_size = 1,
  resample_grid_size = 20,
  no_trees = 500,
  min_node_size = 20,
  pseudoAbs_value = -0.02,
  all_wgt = 1,
  wgt_limits = c(1, 500),
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 9)   

var_to_fit <- "FOI"

out_fl_nm <- "square_predictions_all_data.rds"


# define variables ------------------------------------------------------------  


model_type <- paste0(var_to_fit, "_boot_model")

my_dir <- paste0("grid_size_", parameters$grid_size)

out_pt <- file.path("output", "EM_algorithm", "bootstrap_models", my_dir, model_type)


# rebuild the queue object? --------------------------------------------------- 


if (CLUSTER) {

  config <- didehpc::didehpc_config(template = "20Core")
  obj <- didehpc::queue_didehpc(ctx, config = config)

} else {

  context::context_load(ctx)

}


# get results ----------------------------------------------------------------- 


bundles <- obj$task_bundle_info()

my_task_id <- bundles[nrow(bundles), "name"] 

EM_alg_run_t <- obj$task_bundle_get(my_task_id)

EM_alg_run <- EM_alg_run_t$results()


# combine results ------------------------------------------------------------- 


prediction_sets <- do.call("cbind", EM_alg_run)

write_out_rds(prediction_sets, out_pt, out_fl_nm)
