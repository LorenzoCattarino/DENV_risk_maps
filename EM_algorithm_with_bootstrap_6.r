# Load back in the results of the EM algorithm.
# Specifically, for each bootstrap sample, get:
#
# 1) Vector of square-level predictions for the entire 20km dataset

options(didehpc.cluster = "fi--didemrchnb")

CLUSTER <- TRUE

my_resources <- c(
  file.path("R", "random_forest", "fit_ranger_RF_and_make_predictions.R"),
  file.path("R", "random_forest", "exp_max_algorithm.R"),
  file.path("R", "plotting", "quick_raster_map.R"),
  file.path("R", "plotting", "generic_scatter_plot.R"),
  file.path("R", "prepare_datasets", "calculate_wgt_corr.R"),
  file.path("R", "utility_functions.R"))

my_pkgs <- c("ranger", "dplyr", "fields", "ggplot2", "weights", "colorRamps")

context::context_log_start()
ctx <- context::context_save(path = "context",
                             sources = my_resources,
                             packages = my_pkgs)


# define parameters ----------------------------------------------------------- 


parameters <- list(
  dependent_variable = "FOI",
  pseudoAbs_value = -0.02,
  foi_offset = 0.03,
  grid_size = 5,
  no_trees = 500,
  min_node_size = 20,
  no_samples = 200,
  EM_iter = 10,
  no_predictors = 26)   

model_type_tag <- "_boot_model"

out_fl_nm <- "square_predictions_all_data.rds"


# define variables ------------------------------------------------------------  


model_type <- paste0(parameters$dependent_variable, model_type_tag)

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
